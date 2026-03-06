module Regalloc

open Assembly
open ResultCE

// Operand module mostly for type definition compat
module Operand =
    type OperandType = AsmOperand
    let compare (a: AsmOperand) (b: AsmOperand) = Operators.compare a b

let showReg (r: AsmReg) = sprintf "%A" r
let ppOperand (out: System.IO.TextWriter) (op: AsmOperand) = out.Write(sprintf "%A" op)

// F# Sets and Maps are generic, but we define aliases to match OCaml naming
type OperandSet = Set<Operand.OperandType>
type StringSet = Set<string>
type StringMap<'v> = Map<string, 'v>
type IntMap<'v> = Map<int, 'v>

// DisjointSets is used directly (F# has no functors)

let debugPrint debug fmt =
    Printf.kprintf
        (fun msg -> if debug then printf "%s" msg)
        fmt

// extract all operands from an instruction.
let getOperands = function
    | Mov (_, src, dst) -> Ok [ src; dst ]
    | Movsx i -> Ok [ i.src; i.dst ]
    | MovZeroExtend zx -> Ok [ zx.src; zx.dst ]
    | Lea (src, dst) -> Ok [ src; dst ]
    | Cvttsd2si (_, src, dst) -> Ok [ src; dst ]
    | Cvtsi2sd (_, src, dst) -> Ok [ src; dst ]
    | Unary (_, _, op) -> Ok [ op ]
    | Binary b -> Ok [ b.src; b.dst ]
    | Cmp (_, v1, v2) -> Ok [ v1; v2 ]
    | Idiv (_, op) -> Ok [ op ]
    | Div (_, op) -> Ok [ op ]
    | SetCC (_, op) -> Ok [ op ]
    | Push op -> Ok [ op ]
    | Label _ | Call _ | Ret | Cdq _ | JmpCC _ | Jmp _ -> Ok []
    | Pop _ -> Error (CompilerError.InternalError "Internal error")

// map function f over all the operands in an instruction
let replaceOps f i =
    match i with
    | Mov (t, src, dst) -> Ok (Mov (t, f src, f dst))
    | Movsx sx -> Ok (Movsx { sx with dst = f sx.dst; src = f sx.src })
    | MovZeroExtend zx -> Ok (MovZeroExtend { zx with dst = f zx.dst; src = f zx.src })
    | Lea (src, dst) -> Ok (Lea (f src, f dst))
    | Cvttsd2si (t, src, dst) -> Ok (Cvttsd2si (t, f src, f dst))
    | Cvtsi2sd (t, src, dst) -> Ok (Cvtsi2sd (t, f src, f dst))
    | Unary (operator, t, operand) -> Ok (Unary (operator, t, f operand))
    | Binary b -> Ok (Binary { b with dst = f b.dst; src = f b.src })
    | Cmp (code, v1, v2) -> Ok (Cmp (code, f v1, f v2))
    | Idiv (t, v) -> Ok (Idiv (t, f v))
    | Div (t, v) -> Ok (Div (t, f v))
    | SetCC (code, dst) -> Ok (SetCC (code, f dst))
    | Push v -> Ok (Push (f v))
    | Label _ | Call _ | Ret | Cdq _ | Jmp _ | JmpCC _ -> Ok i
    | Pop _ -> Error (CompilerError.InternalError "Shouldn't use this yet")

let cleanupMovs instructions =
    let isRedundantMov = function
        | Mov (_, src, dst) when src = dst -> true
        | _ -> false
    in
    List.filter (fun i -> not (isRedundantMov i)) instructions

// Configuration type to replace the OCaml functor argument module
type RegTypeOps = {
    suffix : string
    all_hardregs : Assembly.AsmReg list
    caller_saved_regs : Assembly.AsmReg list
    pseudo_is_current_type : AssemblySymbols.AsmSymbolTableMap -> string -> Result<bool, CompilerError.CompilerError>
}

// Helper to mimic OCaml's List.foldLeftMap
// OCaml: ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
// F# mapFold: ('State -> 'T -> 'Result * 'State) -> ...
let foldLeftMap f acc xs =
    let fSwapped state item =
        let (newState, result) = f state item
        (result, newState)
    let (results, finalState) = List.mapFold fSwapped acc xs
    (finalState, results)

// Node type for the interference graph
type AllocNode = {
    id : Assembly.AsmOperand
    neighbors : OperandSet
    spillCost : float
    color : int option
    pruned : bool
}

let private lookupNode (graph: Map<AsmOperand, AllocNode>) nodeId =
    match Map.tryFind nodeId graph with
    | Some nd -> Ok nd
    | None -> Error (CompilerError.InternalError ("node not found in graph: " + sprintf "%A" nodeId))

// The Allocator Functor converted to a closure
let makeAllocator (R : RegTypeOps) =

    // convenience function : convert set of regs to set of operands
    let regsToOperands regs = List.map (fun r -> Reg r) regs

    // values derived from R
    let allHardregs = R.all_hardregs |> regsToOperands |> Set.ofList

    let callerSavedRegs =
        R.caller_saved_regs |> regsToOperands |> Set.ofList

    // Helper to compute used/written registers from an instruction.
    // The paramRegsForCall function resolves param regs for Call instructions.
    let regsUsedAndWrittenWith paramRegsForCall i =
        let opsUsed, opsWritten =
            match i with
            | Mov (_, src, dst) -> ([ src ], [ dst ])
            | MovZeroExtend zx -> ([ zx.src ], [ zx.dst ])
            | Movsx sx -> ([ sx.src ], [ sx.dst ])
            | Cvtsi2sd (_, src, dst) -> ([ src ], [ dst ])
            | Cvttsd2si (_, src, dst) -> ([ src ], [ dst ])
            (* dst of binary or unary instruction is both read and written *)
            | Binary b -> ([ b.src; b.dst ], [ b.dst ])
            | Unary (_, _, op) -> ([ op ], [ op ])
            | Cmp (_, v1, v2) -> ([ v1; v2 ], [])
            | SetCC (_, op) -> ([], [ op ])
            | Push v -> ([ v ], [])
            | Idiv (_, op) -> ([ op; Reg AX; Reg DX ], [ Reg AX; Reg DX ])
            | Div (_, op) -> ([ op; Reg AX; Reg DX ], [ Reg AX; Reg DX ])
            | Cdq _ -> ([ Reg AX ], [ Reg DX ])
            | Call f ->
                let used =
                    paramRegsForCall f
                    |> List.filter (fun r -> List.contains r R.all_hardregs)
                    |> List.map (fun r -> Reg r)
                (used, Set.toList callerSavedRegs)
            | Lea (src, dst) -> ([ src ], [ dst ])
            | Jmp _ | JmpCC _ | Label _ | Ret -> ([], [])
            | Pop _ -> ([], [])
        (* convert list of operands read into list of hard/pseudoregs read *)
        let regsUsedToRead opr =
            match opr with
            | Pseudo _ | Reg _ -> [ opr ]
            | Memory (r, _) -> [ Reg r ]
            | Indexed x -> [ Reg x.baseReg; Reg x.index ]
            | Imm _ | Data _ | PseudoMem _ -> []
        in
        let regsRead1 = List.collect regsUsedToRead opsUsed in
        let regsUsedToUpdate opr =
            match opr with
            | Pseudo _ | Reg _ -> ([], [ opr ])
            | Memory (r, _) -> ([ Reg r ], [])
            | Indexed x -> ([ Reg x.baseReg; Reg x.index ], [])
            | Imm _ | Data _ | PseudoMem _ -> ([], [])
        in
        let concatPair (a, b) = (List.concat a, List.concat b) in
        let regsRead2, regsWritten =
            List.map regsUsedToUpdate opsWritten |> List.unzip |> concatPair
        in
        ( Set.ofList (regsRead1 @ regsRead2),
          Set.ofList regsWritten )

    // Result-returning version: looks up param regs from the symbol table
    let regsUsedAndWritten (asmSymbols: AssemblySymbols.AsmSymbolTableMap) i =
        match i with
        | Call f ->
            result {
                let! paramRegs = AssemblySymbols.paramRegsUsed f asmSymbols
                return regsUsedAndWrittenWith (fun _ -> paramRegs) i
            }
        | Pop _ -> Error (CompilerError.InternalError "Internal error")
        | _ -> Ok (regsUsedAndWrittenWith (fun _ -> []) i)

    // Types defined inside the allocator
    // Note: In F# types must be defined before use in the class or outside.
    // Since they depend on generic concepts but not strictly on R values for definition,
    // we define the structure here.

    // type nodeId = Assembly.AsmOperand // Alias

    // type node = {
    //    id : Assembly.AsmOperand;
    //    mutable neighbors : OperandSet;
    //    spillCost : float;
    //    color : int option;
    //    pruned : bool;
    // }

    // type NodeMap<'T> = Map<Operand.OperandType, 'T>
    // type graph = NodeMap<node>

    let showNodeId nd =
        let s =
            match nd with
            | Reg r -> Ok (showReg r)
            | Pseudo p -> Ok p
            | _ ->
                Error (CompilerError.InternalError "malformed interference graph")
        in
        Result.map (String.map (function '.' -> '_' | c -> c)) s

    // Since types need to be concrete for the methods
    // We will use local record definitions or assume they are mapped to the structure below

    // Helper function for Liveness
    // OCaml foldLeftMap: ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
    let foldLeftMap f acc lst =
        let rec go acc result = function
            | [] -> (acc, List.rev result)
            | x :: xs ->
                let acc', y = f acc x
                go acc' (y :: result) xs
        go acc [] lst

    // Plain-value meet function: uses pre-computed returnRegs
    // Uses direct lookup on BasicBlocks to avoid Result-returning getBlockValue
    let meet returnRegs (cfg: Cfg.ControlFlowGraph<Set<AsmOperand>, AsmInstruction>) (block: Cfg.BasicBlock<Set<AsmOperand>, AsmInstruction>) =
        let getBlockValueDirect n =
            match List.tryFind (fun (k, _) -> k = n) cfg.BasicBlocks with
            | Some (_, blk) -> blk.value
            | None -> Set.empty // block not found; should not happen in a well-formed CFG
        let updateLive live = function
            | Cfg.Entry -> live // Entry predecessors don't contribute live regs
            | Cfg.Exit -> Set.union live returnRegs
            | Cfg.Block n -> Set.union live (getBlockValueDirect n)
        List.fold updateLive Set.empty block.succs

    // Plain-value transfer function: uses pre-computed paramRegsMap
    let transfer paramRegsMap (block: Cfg.BasicBlock<Set<AsmOperand>, AsmInstruction>) (endLiveRegs: Set<AsmOperand>) =
        let paramRegsForCall f =
            match Map.tryFind f paramRegsMap with
            | Some regs -> regs
            | None -> []
        let processInstr currentLiveRegs ((_: Set<AsmOperand>), (i: AsmInstruction)) =
            let annotatedInstr = (currentLiveRegs, i) in
            let newLiveRegs =
                let regsUsed, regsWritten = regsUsedAndWrittenWith paramRegsForCall i in
                let withoutKilled = Set.difference currentLiveRegs regsWritten in
                Set.union withoutKilled regsUsed
            in
            (newLiveRegs, annotatedInstr)
        in
        let incomingLiveRegs, annotatedReversedInstructions =
            block.instructions
            |> List.rev
            |> foldLeftMap processInstr endLiveRegs
        in
        { block with
            instructions = List.rev annotatedReversedInstructions
            value = incomingLiveRegs }

    // Collect all function names referenced by Call instructions
    let collectCalledFunctions instructions =
        List.choose (function Call f -> Some f | _ -> None) instructions
        |> List.distinct

    let analyzeLiveness debug counter (asmSymbols: AssemblySymbols.AsmSymbolTableMap) fn_name cfg instructions =
        result {
            // Pre-compute return regs for this function
            let! allReturnRegsList = AssemblySymbols.returnRegsUsed fn_name asmSymbols
            let allReturnRegs =
                allReturnRegsList
                |> regsToOperands
                |> Set.ofList
            let returnRegs = Set.intersect allHardregs allReturnRegs

            // Pre-compute param regs for all called functions
            let calledFunctions = collectCalledFunctions instructions
            let! paramRegsPairs =
                resultTraverse (fun f ->
                    result {
                        let! regs = AssemblySymbols.paramRegsUsed f asmSymbols
                        return (f, regs)
                    }) calledFunctions
            let paramRegsMap = Map.ofList paramRegsPairs

            return!
                Backward_dataflow.analyze
                    debug
                    ppOperand
                    (Set.empty : Set<AsmOperand>)
                    (=)
                    Set.toList
                    (meet returnRegs)
                    (transfer paramRegsMap)
                    Cfg.initializeAnnotation
                    Cfg.updateBasicBlock
                    (fun (blk: Cfg.BasicBlock<Set<AsmOperand>, AsmInstruction>) -> blk.value)
                    (fun (blk: Cfg.BasicBlock<Set<AsmOperand>, AsmInstruction>) -> blk.preds)
                    (fun (cfg: Cfg.ControlFlowGraph<Set<AsmOperand>, AsmInstruction>) -> cfg.BasicBlocks)
                    (fun (cfg: Cfg.ControlFlowGraph<Set<AsmOperand>, AsmInstruction>) -> cfg.debugLabel)
                    (fun lbl (cfg: Cfg.ControlFlowGraph<Set<AsmOperand>, AsmInstruction>) -> { cfg with debugLabel = lbl })
                    (Cfg.graphvizToString (fun (out: System.IO.TextWriter) (i: AsmInstruction) -> out.Write(sprintf "%A" i)))
                    counter
                    cfg
        }

    let k = Set.count allHardregs

    let allocate debug counter (asmSymbols: AssemblySymbols.AsmSymbolTableMap) fn_name aliased_pseudos instructions =

        // Define node and graph locally to closing over R if needed,
        // though strictly they could be outside.

        let mkBaseGraph () =
            let addNode g r =
                Map.add r
                    {
                        id = r;
                        neighbors = Set.remove r allHardregs;
                        spillCost = infinity;
                        color = None;
                        pruned = false;
                    }
                    g
            in
            List.fold addNode Map.empty (Set.toList allHardregs)

        let getPseudoNodes aliased_pseudos instructions =
            result {
                let operandsToPseudos = function
                    | Assembly.Pseudo r -> Some r
                    | _ -> None
                let getPseudos i =
                    result {
                        let! ops = getOperands i
                        return List.choose operandsToPseudos ops
                    }
                let! allPseudoLists = resultTraverse getPseudos instructions
                let allPseudos = List.concat allPseudoLists
                let! filteredPseudos =
                    resultTraverse (fun r ->
                        result {
                            let! isCurrentType = R.pseudo_is_current_type asmSymbols r
                            let! isStatic = AssemblySymbols.isStatic r asmSymbols
                            let include' = isCurrentType && not (isStatic || Set.contains r aliased_pseudos)
                            return if include' then Some r else None
                        }) allPseudos
                let pseudos =
                    filteredPseudos
                    |> List.choose id
                    |> List.distinct
                    |> List.sort
                let initializeNode pseudo =
                    {
                        id = Pseudo pseudo;
                        neighbors = Set.empty;
                        spillCost = 0.0;
                        color = None;
                        pruned = false;
                    }
                return List.map initializeNode pseudos
            }

        let addPseudoNodes aliased_pseudos graph instructions =
            result {
                let! nds = getPseudoNodes aliased_pseudos instructions
                let addNode g (nd : AllocNode) = Map.add nd.id nd g
                return List.fold addNode graph nds
            }

        let getNodeById graph nodeId = lookupNode graph nodeId

        let addEdge g nd_id1 nd_id2 =
            result {
                let! nd1 = lookupNode g nd_id1
                let! nd2 = lookupNode g nd_id2
                return
                    g |> Map.add nd_id1 { nd1 with neighbors = Set.add nd_id2 nd1.neighbors }
                      |> Map.add nd_id2 { nd2 with neighbors = Set.add nd_id1 nd2.neighbors }
            }

        let removeEdge g nd_id1 nd_id2 =
            result {
                let! nd1 = getNodeById g nd_id1
                let! nd2 = getNodeById g nd_id2
                return
                    g |> Map.add nd_id1 { nd1 with neighbors = Set.remove nd_id2 nd1.neighbors }
                      |> Map.add nd_id2 { nd2 with neighbors = Set.remove nd_id1 nd2.neighbors }
            }

        let degree graph nd_id =
            result {
                let! nd = getNodeById graph nd_id
                return Set.count nd.neighbors
            }

        let areNeighbors g nd_id1 nd_id2 =
            result {
                let! nd1 = lookupNode g nd_id1
                return Set.contains nd_id2 nd1.neighbors
            }

        let addEdges (livenessCfg: Cfg.ControlFlowGraph<Set<AsmOperand>, AsmInstruction>) interference_graph =
            let handleInstr g (liveAfterInstr, i) =
                result {
                    let! _, updatedRegs = regsUsedAndWritten asmSymbols i

                    let! g' =
                        resultFold (fun g l ->
                            match i with
                            | Mov (_, src, _) when src = l -> Ok g
                            | _ ->
                                resultFold (fun g u ->
                                    if
                                        u <> l
                                        && Map.containsKey l g
                                        && Map.containsKey u g
                                    then addEdge g l u
                                    else Ok g
                                ) g (Set.toList updatedRegs)
                        ) g (Set.toList liveAfterInstr)
                    return g'
                }

            let allInstructions =
                List.collect
                    (fun (_, (blk: Cfg.BasicBlock<Set<AsmOperand>, AsmInstruction>)) -> blk.instructions)
                    livenessCfg.BasicBlocks
            in
            resultFold handleInstr interference_graph allInstructions

        let buildInterferenceGraph debug counter fn_name aliased_pseudos instructions =
            result {
                let baseGraph = mkBaseGraph ()
                let! graph = addPseudoNodes aliased_pseudos baseGraph instructions
                let! cfg = AsmCfg.instructionsToCfg fn_name instructions
                let! counter', livenessCfg, dots = analyzeLiveness debug counter asmSymbols fn_name cfg instructions
                let! graphWithEdges = addEdges livenessCfg graph
                return (counter', graphWithEdges, dots)
            }

        let addSpillCosts graph instructions =
            result {
                let incrCount (counts : Map<string, int>) pseudo =
                    let updater = function None -> Some 1 | Some i -> Some (i + 1) in
                    // F# Map.change is equivalent to OCaml Map.update
                    Map.change pseudo updater counts
                in
                let! operandLists = resultTraverse getOperands instructions
                let operands = List.concat operandLists
                let getPseudo = function Assembly.Pseudo r -> Some r | _ -> None in
                let pseudos = List.choose getPseudo operands in
                let countMap = List.fold incrCount Map.empty pseudos in
                let setSpillCost (nd : AllocNode) =
                    match nd.id with
                    | Pseudo r ->
                        match Map.tryFind r countMap with
                        | Some c -> Ok { nd with spillCost = float c }
                        | None -> Error (CompilerError.InternalError ("pseudo not found in count map: " + r))
                    | _ -> Ok nd
                let! entries =
                    resultTraverse (fun (k, v) ->
                        result {
                            let! v' = setSpillCost v
                            return (k, v')
                        }) (Map.toList graph)
                return Map.ofList entries
            }

        let georgeTest graph hardreg pseudo =
            result {
                let! pseudoNd = getNodeById graph pseudo
                let pseudoregNeighbors = pseudoNd.neighbors
                let! results =
                    resultTraverse (fun neighborId ->
                        result {
                            let! isNeighbor = areNeighbors graph neighborId hardreg
                            let! deg = degree graph neighborId
                            return isNeighbor || deg < k
                        }) (Set.toList pseudoregNeighbors)
                return List.forall id results
            }

        let briggsTest graph x y =
            result {
                let! xNd = getNodeById graph x
                let! yNd = getNodeById graph y
                let neighbors = Set.union xNd.neighbors yNd.neighbors
                let! significantNeighborCount =
                    resultFold (fun cnt neighborId ->
                        result {
                            let! deg = degree graph neighborId
                            let! xNeighbor = areNeighbors graph x neighborId
                            let! yNeighbor = areNeighbors graph y neighborId
                            let adjustedDeg =
                                if xNeighbor && yNeighbor
                                then deg - 1
                                else deg
                            return if adjustedDeg >= k then cnt + 1 else cnt
                        }) 0 (Set.toList neighbors)
                return significantNeighborCount < k
            }

        let conservativeCoalescable graph src dst =
            result {
                let! briggsResult = briggsTest graph src dst
                if briggsResult then return true
                else
                    match (src, dst) with
                    | Reg _, _ -> return! georgeTest graph src dst
                    | _, Reg _ -> return! georgeTest graph dst src
                    | _ -> return false
            }

        let updateGraph g to_merge to_keep =
            result {
                let! mergeNd = getNodeById g to_merge
                let! g' =
                    resultFold (fun g neighborId ->
                        result {
                            let! g = addEdge g neighborId to_keep
                            return! removeEdge g neighborId to_merge
                        }) g (Set.toList mergeNd.neighbors)
                return Map.remove to_merge g'
            }

        let coalesce graph instructions =
            let processInstr (g, regMap) = function
                | Mov (_, src, dst) ->
                    let src' = DisjointSets.find src regMap
                    let dst' = DisjointSets.find dst regMap
                    if
                        Map.containsKey src' g
                        && Map.containsKey dst' g
                        && src' <> dst'
                    then
                        result {
                            let! neighbors = areNeighbors g src' dst'
                            if neighbors then return (g, regMap)
                            else
                                let! coalescable = conservativeCoalescable g src' dst'
                                if not coalescable then return (g, regMap)
                                else
                                    match src' with
                                    | Reg _ ->
                                        let! g' = updateGraph g dst' src'
                                        return ( g', DisjointSets.union dst' src' regMap )
                                    | _ ->
                                        let! g' = updateGraph g src' dst'
                                        return ( g', DisjointSets.union src' dst' regMap )
                        }
                    else Ok (g, regMap)
                | _ -> Ok (g, regMap)
            in
            result {
                let! _updated_graph, newInstructions =
                    resultFold processInstr (graph, DisjointSets.init) instructions
                return newInstructions
            }

        let rewriteCoalesced instructions coalescedRegs =
            let f r = DisjointSets.find r coalescedRegs in
            let rewriteInstruction = function
                | Mov (t, src, dst) ->
                    let newSrc = f src
                    let newDst = f dst
                    if newSrc = newDst then Ok None else Ok (Some (Mov (t, newSrc, newDst)))
                | i ->
                    result {
                        let! replaced = replaceOps f i
                        return Some replaced
                    }
            in
            result {
                let! results = resultTraverse rewriteInstruction instructions
                return List.choose id results
            }

        let rec colorGraph graph =
            result {
                let remaining =
                    graph
                    |> Map.toList
                    |> List.map snd
                    |> List.filter (fun (nd: AllocNode) -> not nd.pruned)
                in
                match remaining with
                | [] -> return graph
                | _ ->
                    let notPruned nd_id =
                        result {
                            let! nd = lookupNode graph nd_id
                            return not nd.pruned
                        }
                    let degree (nd: AllocNode) =
                        result {
                            let! unprunedResults =
                                resultTraverse (fun nId ->
                                    result {
                                        let! np = notPruned nId
                                        return if np then Some nId else None
                                    }) (Set.toList nd.neighbors)
                            let unprunedNeighbors = List.choose id unprunedResults
                            return List.length unprunedNeighbors
                        }
                    let! nextNode =
                        // Try to find a low-degree node
                        let rec findLowDegree = function
                            | [] -> Ok None
                            | nd :: rest ->
                                result {
                                    let! deg = degree nd
                                    if deg < k then return Some nd
                                    else return! findLowDegree rest
                                }
                        result {
                            let! lowDeg = findLowDegree remaining
                            match lowDeg with
                            | Some nd -> return nd
                            | None ->
                                let! spillMetrics =
                                    resultTraverse (fun nd ->
                                        result {
                                            let! deg = degree nd
                                            let metric = nd.spillCost / float deg
                                            return (nd, metric, deg)
                                        }) remaining
                                let printSpillInfo (nd, metric, deg) =
                                    result {
                                        let! nodeIdStr = showNodeId nd.id
                                        debugPrint debug "Node %s has degree %d, spill cost %f and metric %f\n"
                                            nodeIdStr deg nd.spillCost metric
                                    }
                                debugPrint debug "================================\n"
                                let! _ = resultTraverse printSpillInfo spillMetrics
                                let cmp (_, m1, _) (_, m2, _) = compare m1 m2
                                match ListUtil.tryMin cmp spillMetrics with
                                | Some (spilled, _, _) ->
                                    let! spilledIdStr = showNodeId spilled.id
                                    debugPrint debug "Spill candidate: %s\n" spilledIdStr
                                    return spilled
                                | None -> return! Error (CompilerError.InternalError "no remaining nodes to spill")
                        }
                    let prunedGraph =
                        Map.change nextNode.id
                            (function
                                | Some nd -> Some { nd with pruned = true }
                                | None -> None)
                            graph
                    let! partlyColored = colorGraph prunedGraph
                    let allColors = List.init k id in
                    let removeNeighborColor neighborId remainingColors =
                        result {
                            let! neighborNd = lookupNode partlyColored neighborId
                            match neighborNd.color with
                            | Some c -> return List.filter (fun col -> col <> c) remainingColors
                            | None -> return remainingColors
                        }
                    let! availableColors =
                        resultFold (fun acc elem -> removeNeighborColor elem acc) allColors (Set.toList nextNode.neighbors)
                    match availableColors with
                    | [] -> return partlyColored
                    | _ :: _ ->
                        let c =
                            match nextNode.id with
                            | Reg r when not (List.contains r R.caller_saved_regs) ->
                                match ListUtil.tryMax compare availableColors with
                                | Some c -> c
                                | None -> List.head availableColors // unreachable, list is non-empty
                            | _ ->
                                match ListUtil.tryMin compare availableColors with
                                | Some c -> c
                                | None -> List.head availableColors // unreachable, list is non-empty
                        return
                            Map.change nextNode.id
                                (function
                                    | Some nd -> Some { nd with pruned = false; color = Some c }
                                    | None -> None)
                                partlyColored
            }

        let makeRegisterMap fn_name graph =
            result {
                let! colorsToRegs =
                    resultFold (fun colorMap (nd_id, (nd: AllocNode)) ->
                        match nd_id with
                        | Reg r ->
                            match nd.color with
                            | Some c -> Ok (Map.add c r colorMap)
                            | None -> Error (CompilerError.InternalError "hardreg node without color")
                        | _ -> Ok colorMap
                    ) Map.empty (Map.toList graph)

                let! usedCalleeSaved, regMap =
                    resultFold (fun (usedCalleeSaved, regMap) (_k, (nd: AllocNode)) ->
                        match nd with
                        | { id = Pseudo p; color = Some c } ->
                            match Map.tryFind c colorsToRegs with
                            | Some hardreg ->
                                let usedCalleeSaved =
                                    if List.contains hardreg R.caller_saved_regs then usedCalleeSaved
                                    else Reg_set.add hardreg usedCalleeSaved
                                Ok (usedCalleeSaved, Map.add p hardreg regMap)
                            | None -> Error (CompilerError.InternalError ("color not found in register map: " + sprintf "%d" c))
                        | _ -> Ok (usedCalleeSaved, regMap)
                    ) (Reg_set.empty, Map.empty) (Map.toList graph)

                let! asmSymbols' = AssemblySymbols.addCalleeSavedRegsUsed fn_name usedCalleeSaved asmSymbols
                return (asmSymbols', regMap)
            }

        let replacePseudoregs instructions regMap =
            let f = function
                | Assembly.Pseudo p as op ->
                    match Map.tryFind p regMap with
                    | Some r -> Reg r
                    | None -> op
                | op -> op
            in
            result {
                let! replaced = resultTraverse (replaceOps f) instructions
                return cleanupMovs replaced
            }

        let rec coalesceLoop counter currentInstructions accDots =
            result {
                let! counter', graph, dots = buildInterferenceGraph debug counter fn_name aliased_pseudos currentInstructions
                let accDots' = accDots @ dots
                let! coalescedRegs = coalesce graph currentInstructions
                if DisjointSets.isEmpty coalescedRegs then return (counter', graph, currentInstructions, accDots')
                else
                    let! newInstructions =
                        rewriteCoalesced currentInstructions coalescedRegs
                    return! coalesceLoop counter' newInstructions accDots'
            }

        result {
            let! counter', coalescedGraph, coalescedInstructions, dots = coalesceLoop counter instructions []
            let! graphWithSpillCosts =
                addSpillCosts coalescedGraph coalescedInstructions
            let! coloredGraph = colorGraph graphWithSpillCosts
            let! asmSymbols', registerMap = makeRegisterMap fn_name coloredGraph
            let! replacedInstrs = replacePseudoregs coalescedInstructions registerMap
            return (counter', asmSymbols', replacedInstrs, dots)
        }

    allocate

let gpAllocate = makeAllocator {
    suffix = "gp"
    all_hardregs = [ AX; BX; CX; DX; DI; SI; R8; R9; R12; R13; R14; R15 ]
    caller_saved_regs = [ AX; CX; DX; DI; SI; R8; R9 ]
    pseudo_is_current_type = fun ast p ->
        result {
            let! t = AssemblySymbols.getType p ast
            return t <> Double
        }
}

let xmmAllocate = makeAllocator {
    suffix = "xmm"
    all_hardregs =
        [
            XMM0; XMM1; XMM2; XMM3; XMM4; XMM5; XMM6;
            XMM7; XMM8; XMM9; XMM10; XMM11; XMM12; XMM13;
        ]
    caller_saved_regs =
        [
            XMM0; XMM1; XMM2; XMM3; XMM4; XMM5; XMM6;
            XMM7; XMM8; XMM9; XMM10; XMM11; XMM12; XMM13;
        ]
    pseudo_is_current_type = fun ast p ->
        result {
            let! t = AssemblySymbols.getType p ast
            return t = Double
        }
}

let allocateRegisters debug counter asmSymbols aliased_pseudos (Program tls) =
    let allocateRegsForFun counter asmSymbols fnName instructions =
        result {
            let! counter', asmSymbols', instrs, dots1 = gpAllocate debug counter asmSymbols fnName aliased_pseudos instructions
            let! counter'', asmSymbols'', instrs', dots2 = xmmAllocate debug counter' asmSymbols' fnName aliased_pseudos instrs
            return (counter'', asmSymbols'', instrs', dots1 @ dots2)
        }
    in
    let allocInTl counter asmSymbols = function
        | Function f ->
            result {
                let! counter', asmSymbols', instrs, dots = allocateRegsForFun counter asmSymbols f.name f.instructions
                return (counter', asmSymbols', Function { f with instructions = instrs }, dots)
            }
        | tl -> Ok (counter, asmSymbols, tl, [])
    in
    resultFold (fun (c, ast, acc, accDots) tl ->
        result {
            let! c', ast', tl', dots = allocInTl c ast tl
            return (c', ast', acc @ [tl'], accDots @ dots)
        }) (counter, asmSymbols, [], []) tls
    |> Result.map (fun (c, ast, tls', dots) -> (c, ast, Program tls', dots))
