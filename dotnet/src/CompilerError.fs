module CompilerError

type CompilerError =
    | LexError of string
    | ParseError of string
    | ResolveError of string
    | LoopLabelError of string
    | TypeError of string
    | DriverError of string
    | InternalError of string

let show = function
    | LexError msg -> "Lex error: " + msg
    | ParseError msg -> "Parse error: " + msg
    | ResolveError msg -> "Resolve error: " + msg
    | LoopLabelError msg -> "Loop label error: " + msg
    | TypeError msg -> "Type error: " + msg
    | DriverError msg -> "Driver error: " + msg
    | InternalError msg -> "Internal error: " + msg
