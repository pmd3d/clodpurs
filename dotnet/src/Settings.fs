module Settings

type Stage =
    | Lex
    | Parse
    | Validate
    | Tacky
    | Codegen
    | Assembly
    | Obj
    | Executable

type Target = OS_X | Linux

type CompilerConfig = {
    Platform: Target
    Debug: bool
}

type Optimizations = {
    constant_folding: bool
    dead_store_elimination: bool
    unreachable_code_elimination: bool
    copy_propagation: bool
}