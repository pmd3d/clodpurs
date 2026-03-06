module Symbols

type InitialValue =
    | Tentative
    | Initial of Initializers.StaticInit list
    | NoInitializer

type FunAttr = { defined: bool; isGlobal: bool }
type StaticAttr = { init: InitialValue; isGlobal: bool }

type IdentifierAttrs =
    | FunAttr of FunAttr
    | StaticAttr of StaticAttr
    | ConstAttr of Initializers.StaticInit
    | LocalAttr

type SymbolEntry = { symType: Types.CType; attrs: IdentifierAttrs }

type SymbolTableMap = Map<string, SymbolEntry>

let empty : SymbolTableMap = Map.empty

// always use replace instead of add; we want to remove old binding when we add a new one

let addAutomaticVar name (t: Types.CType) (st: SymbolTableMap) : SymbolTableMap =
    Map.add name { symType = t; attrs = LocalAttr } st

let addStaticVar name (t: Types.CType) (isGlobal: bool) (init: InitialValue) (st: SymbolTableMap) : SymbolTableMap =
    Map.add name { symType = t; attrs = StaticAttr { init = init; isGlobal = isGlobal } } st

let addFun name (t: Types.CType) (isGlobal: bool) (defined: bool) (st: SymbolTableMap) : SymbolTableMap =
    Map.add name { symType = t; attrs = FunAttr { isGlobal = isGlobal; defined = defined } } st

let get name (st: SymbolTableMap) =
    match Map.tryFind name st with
    | Some v -> Ok v
    | None -> Error (CompilerError.InternalError ("symbol not found: " + name))

let getOpt name (st: SymbolTableMap) = Map.tryFind name st

let addStringWithCounter (counter: UniqueIds.Counter) (s: string) (st: SymbolTableMap) =
    let counter', str_id = UniqueIds.makeNamedTemporary "string" counter
    let t = Types.Array (Types.Char, int64 (String.length s + 1))
    let st' = Map.add str_id { symType = t; attrs = ConstAttr (Initializers.StringInit (s, true)) } st
    (counter', str_id, st')

let isGlobal name (st: SymbolTableMap) =
    get name st
    |> Result.map (fun entry ->
        match entry.attrs with
        | LocalAttr | ConstAttr _ -> false
        | StaticAttr { isGlobal = g } -> g
        | FunAttr { isGlobal = g } -> g)

let bindings (st: SymbolTableMap) = Map.toList st
