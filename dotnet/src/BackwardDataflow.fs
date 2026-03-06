module Backward_dataflow

type Annotation<'varSet> = 'varSet
type AnnotatedBlock<'a, 'block> = Cfg.BasicBlock<'a, 'block>
type AnnotatedGraph<'a, 'block> = Cfg.ControlFlowGraph<'a, 'block>

let debugPrint (debug: bool) (extra_tag: string) (pp_var: System.IO.TextWriter -> 'var -> unit)
                (elements: 'varSet -> 'var list)
                (print_graphviz: (System.IO.TextWriter -> 'varSet -> unit) -> UniqueIds.Counter -> 'cfg -> UniqueIds.Counter * string * string)
                (counter: UniqueIds.Counter)
                (debugLabel: string)
                (setDebugLabel: string -> 'cfg -> 'cfg)
                (cfg: 'cfg) =
    if debug then
        let livevarPrinter (fmt: System.IO.TextWriter) (liveVars: 'varSet) =
            elements liveVars
            |> List.iteri (fun i v ->
                if i > 0 then fmt.Write(", ")
                pp_var fmt v)
        let lbl = debugLabel + "_dse" + extra_tag
        let counter', label, dotContent = print_graphviz livevarPrinter counter (setDebugLabel lbl cfg)
        (counter', Some (label, dotContent))
    else (counter, None)

let analyze (debug: bool) (pp_var: System.IO.TextWriter -> 'var -> unit)
            (empty: 'varSet)
            (equal: 'varSet -> 'varSet -> bool)
            (elements: 'varSet -> 'var list)
            (meet_fn: 'cfg -> 'block -> 'varSet)
            (transfer_fn: 'block -> 'varSet -> 'block)
            (initialize_annotation: 'cfg0 -> 'varSet -> 'cfg)
            (update_basic_block: int -> 'block -> 'cfg -> 'cfg)
            (get_value: 'block -> 'varSet)
            (get_preds: 'block -> Cfg.NodeId list)
            (get_basic_blocks: 'cfg -> (int * 'block) list)
            (getDebugLabel: 'cfg -> string)
            (setDebugLabel: string -> 'cfg -> 'cfg)
            (print_graphviz: (System.IO.TextWriter -> 'varSet -> unit) -> UniqueIds.Counter -> 'cfg -> UniqueIds.Counter * string * string)
            (counter: UniqueIds.Counter)
            (cfg: 'cfg0) =
    let startingCfg = initialize_annotation cfg empty
    let rec processWorklist (counter: UniqueIds.Counter) (currentCfg: 'cfg)
                             (worklist: (int * 'block) list)
                             (accDots: (string * string) list) =
        let counter', maybeDot = debugPrint debug "_in_progress_" pp_var elements print_graphviz
                                    counter (getDebugLabel currentCfg) setDebugLabel currentCfg
        let accDots' = match maybeDot with Some d -> d :: accDots | None -> accDots
        match worklist with
        | [] -> Ok (counter', currentCfg, List.rev accDots')
        | (blockIdx, blk) :: rest ->
            let oldAnnotation = get_value blk
            let liveVarsAtExit = meet_fn currentCfg blk
            let block' = transfer_fn blk liveVarsAtExit
            let updatedCfg = update_basic_block blockIdx block' currentCfg
            let newWorklistResult =
                if equal oldAnnotation (get_value block') then Ok rest
                else
                    List.fold
                        (fun acc pred ->
                            match acc with
                            | Error e -> Error e
                            | Ok wklist ->
                            match pred with
                            | Cfg.Entry -> Ok wklist
                            | Cfg.Exit ->
                                Error (CompilerError.InternalError "malformed CFG")
                            | Cfg.Block n ->
                                if List.exists (fun (k, _) -> k = n) wklist then Ok wklist
                                else
                                    let blocks = get_basic_blocks updatedCfg
                                    match List.tryFind (fun (k, _) -> k = n) blocks with
                                    | Some (_, b) -> Ok ((n, b) :: wklist)
                                    | None -> Error (CompilerError.InternalError "block not found in CFG"))
                        (Ok rest) (get_preds block')
            match newWorklistResult with
            | Error e -> Error e
            | Ok newWorklist ->
                processWorklist counter' updatedCfg newWorklist accDots'
    processWorklist counter startingCfg (List.rev (get_basic_blocks startingCfg)) []