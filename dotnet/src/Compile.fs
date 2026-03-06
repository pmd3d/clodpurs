module Compile

open ResultCE

type CompileResult = {
    assemblyFile: (string * string) option     // (path, content) — None if stopped before emit
    debugFiles: (string * string) list         // (path, content) for tacky/prealloc/postalloc
    graphvizDots: (string * string) list       // (label, dotContent) — label used to derive .dot/.png paths
}

let emptyResult = { assemblyFile = None; debugFiles = []; graphvizDots = [] }

let compile (config: Settings.CompilerConfig) (stage: Settings.Stage) (optimizations: Settings.Optimizations) (src_file: string) (source: string) : Result<CompileResult, CompilerError.CompilerError> =
    result {
        let counter = UniqueIds.initialCounter
        // Lex it
        let! tokens = Lexer.lex source |> Result.mapError CompilerError.LexError
        if stage = Settings.Lex then return emptyResult
        else
            let! ast = Parse.parse tokens |> Result.mapError CompilerError.ParseError
            if stage = Settings.Parse then
                printf "%A" ast
                return emptyResult
            else
                // Semantic analysis has three steps:
                // 1. resolve identifiers
                let! counter', resolved_ast =
                    Resolve.resolve counter ast |> Result.mapError CompilerError.ResolveError
                // 2. annotate loops and break/continue statements
                let! counter'', annotated_ast =
                    Label_loops.labelLoops counter' resolved_ast |> Result.mapError CompilerError.LoopLabelError
                // 3. typecheck definitions and uses of functions and variables
                let! tchkState =
                    let initState : Typecheck.TypecheckState = { counter = counter''; st = Symbols.empty; tt = TypeTable.empty }
                    Typecheck.typecheck initState annotated_ast
                let tchkState', typed_ast = tchkState
                if stage = Settings.Validate then return emptyResult
                else
                    // Convert the AST to TACKY
                    let! counter''', st', tacky = TackyGen.gen tchkState'.counter tchkState'.st tchkState'.tt typed_ast
                    // collect debug tacky output
                    let counter'''', tackyDebug = Tacky_print.debugPrintTacky config.Debug counter''' src_file tacky
                    let debugFiles =
                        match tackyDebug with
                        | Some (path, content) -> [(path, content)]
                        | None -> []
                    // optimize it!
                    let optimized_tacky = Optimize.optimize optimizations src_file tacky
                    if stage = Settings.Tacky then return { emptyResult with debugFiles = debugFiles }
                    else
                        // start by getting all aliased vars, we'll need them for register
                        // allocation
                        let aliased_vars = Address_taken.analyzeProgram optimized_tacky
                        // Assembly generation has three steps:
                        // 1. convert TACKY to assembly
                        let! _counter5, asmSymbols, asm_ast = Codegen.gen counter'''' tchkState'.tt st' AssemblySymbols.empty optimized_tacky
                        // collect pre-pseudoreg-allocation assembly if debug enabled
                        let! debugFiles =
                            if config.Debug then
                                result {
                                    let prealloc_filename =
                                        System.IO.Path.ChangeExtension(src_file, null) + ".prealloc.debug.s"
                                    let! content = Emit.emitToString config.Platform asmSymbols asm_ast
                                    return debugFiles @ [(prealloc_filename, content)]
                                }
                            else Ok debugFiles
                        // replace remaining pseudoregisters with Data/Stack operands
                        let! _counter6, asmSymbols', asm_ast1, graphvizDots = Regalloc.allocateRegisters config.Debug _counter5 asmSymbols aliased_vars asm_ast
                        let! debugFiles =
                            if config.Debug then
                                result {
                                    let postalloc_filename =
                                        System.IO.Path.ChangeExtension(src_file, null) + ".postalloc.debug.s"
                                    let! content = Emit.emitToString config.Platform asmSymbols' asm_ast
                                    return debugFiles @ [(postalloc_filename, content)]
                                }
                            else Ok debugFiles
                        let! asmSymbols'', asm_ast2 = ReplacePseudos.replacePseudos asmSymbols' asm_ast1
                        // fix up instructions
                        let! asm_ast3 = InstructionFixup.fixupProgram asmSymbols'' asm_ast2
                        if stage = Settings.Codegen then return { emptyResult with debugFiles = debugFiles; graphvizDots = graphvizDots }
                        else
                            let asm_filename = System.IO.Path.ChangeExtension(src_file, ".s")
                            let! content = Emit.emitToString config.Platform asmSymbols'' asm_ast3
                            return { assemblyFile = Some (asm_filename, content); debugFiles = debugFiles; graphvizDots = graphvizDots }
    }
