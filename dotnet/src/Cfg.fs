module Cfg

type SimpleInstr =
    | Label of string
    | ConditionalJump of string
    | UnconditionalJump of string
    | Return
    | Other

[<CustomEquality; CustomComparison>]
type NodeId =
    | Entry
    | Block of int
    | Exit

    override this.Equals(obj) =
        match obj with
        | :? NodeId as other ->
            match (this, other) with
            | Entry, Entry -> true
            | Block a, Block b -> a = b
            | Exit, Exit -> true
            | _ -> false
        | _ -> false

    override this.GetHashCode() =
        match this with
        | Entry -> 0
        | Block n -> hash (1, n)
        | Exit -> 2

    interface System.IComparable with
        member this.CompareTo(obj) =
            let tag =
                function
                | Entry -> 0
                | Block _ -> 1
                | Exit -> 2
            match obj with
            | :? NodeId as other ->
                match (this, other) with
                | Entry, Entry -> 0
                | Exit, Exit -> 0
                | Block a, Block b -> compare a b
                | _ -> compare (tag this) (tag other)
            | _ ->
                invalidArg "obj" "Cannot compare values of different types"

type BasicBlock<'v, 'instr> = {
    id: NodeId
    instructions: ('v * 'instr) list
    preds: NodeId list
    succs: NodeId list
    value: 'v
}

type ControlFlowGraph<'v, 'instr> = {
    (* store basic blocks in association list, indexed by block # *)
    BasicBlocks: (int * BasicBlock<'v, 'instr>) list
    entrySuccs: NodeId list
    exitPreds: NodeId list
    debugLabel: string
}

let private findBlock n blocks =
    match List.tryFind (fun (k, _) -> k = n) blocks with
    | Some (_, blk) -> Ok blk
    | None -> Error (CompilerError.InternalError "block not found in CFG")

let getSuccs ndId cfg =
    match ndId with
    | Entry -> Ok cfg.entrySuccs
    | Block n -> findBlock n cfg.BasicBlocks |> Result.map (fun blk -> blk.succs)
    | Exit -> Ok []

let getBlockValue blocknum cfg =
    findBlock blocknum cfg.BasicBlocks |> Result.map (fun blk -> blk.value)

let private updateBlock f blockNum g =
    let newBlocks =
        List.map
            (fun ((i, blk) as entry) ->
                if i = blockNum then (i, f blk) else entry)
            g.BasicBlocks
    { g with BasicBlocks = newBlocks }

let private updateSuccessors f ndId g =
    match ndId with
    | Entry -> Ok { g with entrySuccs = f g.entrySuccs }
    | Block n -> Ok (updateBlock (fun blk -> { blk with succs = f blk.succs }) n g)
    | Exit -> Error (CompilerError.InternalError "malformed CFG: updateSuccessors on Exit")

let private updatePredecessors f ndId g =
    match ndId with
    | Entry -> Error (CompilerError.InternalError "malformed CFG: updatePredecessors on Entry")
    | Block n -> Ok (updateBlock (fun blk -> { blk with preds = f blk.preds }) n g)
    | Exit -> Ok { g with exitPreds = f g.exitPreds }

let addEdge pred succ g =
    let addId ndId idList =
        if List.contains ndId idList then idList
        else ndId :: idList
    match updateSuccessors (addId succ) pred g with
    | Ok g' -> updatePredecessors (addId pred) succ g'
    | Error e -> Error e

let removeEdge pred succ g =
    let removeId ndId idList =
        List.filter (fun i -> i <> ndId) idList
    match updateSuccessors (removeId succ) pred g with
    | Ok g' -> updatePredecessors (removeId pred) succ g'
    | Error e -> Error e

(* replace block with given block ID *)
let updateBasicBlock blockIdx newBlock g =
    let newBlocks =
        List.map
            (fun ((i, _) as blk) ->
                if i = blockIdx then (i, newBlock) else blk)
            g.BasicBlocks
    { g with BasicBlocks = newBlocks }

(* constructing the CFG *)
let private partitionIntoBasicBlocks simplify instructions =
    let f (finished_blocks, current_block) i =
        match simplify i with
        | Label _ ->
            let finished_blocks' =
                if current_block = [] then finished_blocks
                else List.rev current_block :: finished_blocks
            (finished_blocks', [ i ])
        | ConditionalJump _ | UnconditionalJump _ | Return ->
            let finished = List.rev (i :: current_block)
            (finished :: finished_blocks, [])
        | Other -> (finished_blocks, i :: current_block)
    let finished, last = List.fold f ([], []) instructions
    let allBlocks =
        if last = [] then finished else List.rev last :: finished
    List.rev allBlocks

let private addAllEdges simplify g =
    (* build map from labels to the IDs of the blocks that they start with *)
    let labelMap =
        List.fold
            (fun lblMap (_, blk) ->
                match blk.instructions with
                | (_, firstInstr) :: _ ->
                    match simplify firstInstr with
                    | Label lbl -> Map.add lbl blk.id lblMap
                    | _ -> lblMap
                | [] -> lblMap)
            Map.empty g.BasicBlocks

    let findLabel target =
        match Map.tryFind target labelMap with
        | Some id -> Ok id
        | None -> Error (CompilerError.InternalError ("label not found in CFG: " + target))

    (* add outgoing edges from a single basic block *)
    let processNode g (id_num, block) =
        match g with
        | Error e -> Error e
        | Ok g ->
        let next_block =
            match ListUtil.tryLast g.BasicBlocks with
            | Some (lastIdx, _) when id_num = lastIdx -> Exit
            | _ -> Block(id_num + 1)
        match ListUtil.tryLast block.instructions with
        | None -> Error (CompilerError.InternalError "empty basic block")
        | Some (_, last_instr) ->
        match simplify last_instr with
        | Return -> addEdge block.id Exit g
        | UnconditionalJump target ->
            match findLabel target with
            | Ok target_id -> addEdge block.id target_id g
            | Error e -> Error e
        | ConditionalJump target ->
            match findLabel target with
            | Ok target_id ->
                match addEdge block.id next_block g with
                | Ok g' -> addEdge block.id target_id g'
                | Error e -> Error e
            | Error e -> Error e
        | _ -> addEdge block.id next_block g

    match addEdge Entry (Block 0) g with
    | Error e -> Error e
    | Ok g -> List.fold processNode (Ok g) g.BasicBlocks

let instructionsToCfg simplify debugLabel instructions =
    let toNode idx instructions =
        let ann x = ((), x)
        (idx,
         { id = Block idx
           instructions = List.map ann instructions
           preds = []
           succs = []
           value = () })
    let cfg =
        { BasicBlocks =
              List.mapi toNode
                  (partitionIntoBasicBlocks simplify instructions)
          entrySuccs = []
          exitPreds = []
          debugLabel = debugLabel }

    addAllEdges simplify cfg

(* converting back to instructions *)
let cfgToInstructions g =
    let blkToInstrs (_, { instructions = instructions }) =
        List.map snd instructions
    List.collect blkToInstrs g.BasicBlocks

(* working with annotations *)
(* NOTE: Cannot use { x with ... } here because F# doesn't allow changing
   the type parameter in a record update expression (unlike OCaml).
   We construct new records explicitly to allow 'v to change. *)
let initializeAnnotation cfg dummyVal =
    let initializeInstruction (_, i) = (dummyVal, i)
    let initializeBlock (idx, b) =
        (idx,
         { id = b.id
           instructions = List.map initializeInstruction b.instructions
           preds = b.preds
           succs = b.succs
           value = dummyVal })
    { BasicBlocks = List.map initializeBlock cfg.BasicBlocks
      entrySuccs = cfg.entrySuccs
      exitPreds = cfg.exitPreds
      debugLabel = cfg.debugLabel }

let stripAnnotations cfg = initializeAnnotation cfg ()

(* debugging *)
let graphvizToString (ppInstr: System.IO.TextWriter -> 'instr -> unit)
                      (ppVal: System.IO.TextWriter -> 'v -> unit)
                      (counter: UniqueIds.Counter)
                      (cfg: ControlFlowGraph<'v, 'instr>) =
    let c, lbl = UniqueIds.makeLabel cfg.debugLabel counter
    use writer = new System.IO.StringWriter()
    let ppNodeId (out: System.IO.TextWriter) = function
        | Exit -> out.Write("exit")
        | Entry -> out.Write("entry")
        | Block n -> out.Write(sprintf "block%d" n)
    let ppAnnotatedInstruction (out: System.IO.TextWriter) (v, i) =
        out.Write("<tr>")
        out.Write("<td align=\"left\">")
        ppInstr out i
        out.Write("</td>")
        out.Write("<td align=\"left\">")
        ppVal out v
        out.Write("</td>")
        out.Write("</tr>")
        out.WriteLine()
    let ppBlockInstructions (out: System.IO.TextWriter) blk =
        out.Write("<table>")
        out.Write("<tr><td colspan=\"2\"><b>")
        ppNodeId out blk.id
        out.Write("</b></td></tr>")
        out.WriteLine()
        List.iter (ppAnnotatedInstruction out) blk.instructions
        out.Write("<tr><td colspan=\"2\">")
        ppVal out blk.value
        out.Write("</td></tr>")
        out.Write("</table>")
    let ppBlock (out: System.IO.TextWriter) (lbl, b) =
        out.Write(sprintf "block%d[label=<" lbl)
        ppBlockInstructions out b
        out.Write(">]")
    let ppEntryEdge (out: System.IO.TextWriter) lbl =
        out.Write("entry -> ")
        ppNodeId out lbl
    let ppEdgei i (out: System.IO.TextWriter) succ =
        out.Write(sprintf "block%d -> " i)
        ppNodeId out succ
    let ppEdges (out: System.IO.TextWriter) ((lbl: int), (blk: BasicBlock<'a, _>)) =
        List.iter (ppEdgei lbl out) blk.succs
    writer.WriteLine("digraph {")
    writer.WriteLine("  labeljust=l")
    writer.WriteLine("  node[shape=\"box\"]")
    writer.WriteLine("  entry[label=\"ENTRY\"]")
    writer.WriteLine("  exit[label=\"EXIT\"]")
    List.iter (fun b -> ppBlock writer b; writer.WriteLine()) cfg.BasicBlocks
    List.iter (fun e -> ppEntryEdge writer e; writer.WriteLine()) cfg.entrySuccs
    List.iter (fun b -> ppEdges writer b; writer.WriteLine()) cfg.BasicBlocks
    writer.WriteLine("}")
    writer.Flush()
    (c, lbl, writer.ToString())
