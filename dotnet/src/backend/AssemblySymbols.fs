module AssemblySymbols

type AsmFunEntry = {
    defined: bool
    bytes_required: int
    return_on_stack: bool
    param_regs: Assembly.AsmReg list
    return_regs: Assembly.AsmReg list
    callee_saved_regs_used: Reg_set.RegSet
}

type AsmObjEntry = {
    t: Assembly.AsmType
    is_static: bool
    constant: bool
}

type AsmSymbolEntry =
    | Fun of AsmFunEntry
    | Obj of AsmObjEntry

type AsmSymbolTableMap = Map<string, AsmSymbolEntry>

let empty : AsmSymbolTableMap = Map.empty

let addFun fun_name defined return_on_stack param_regs return_regs (ast: AsmSymbolTableMap) : AsmSymbolTableMap =
    Map.add fun_name (Fun {
        defined = defined
        bytes_required = 0
        callee_saved_regs_used = Reg_set.empty
        return_on_stack = return_on_stack
        param_regs = param_regs
        return_regs = return_regs
    }) ast

let addVar var_name t is_static (ast: AsmSymbolTableMap) : AsmSymbolTableMap =
    Map.add var_name (Obj { t = t; is_static = is_static; constant = false }) ast

let addConstant const_name t (ast: AsmSymbolTableMap) : AsmSymbolTableMap =
    Map.add const_name (Obj { t = t; is_static = true; constant = true }) ast

let private find name (ast: AsmSymbolTableMap) =
    match Map.tryFind name ast with
    | Some v -> Ok v
    | None -> Error (CompilerError.InternalError ("assembly symbol not found: " + name))

let setBytesRequired fun_name bytes_required (ast: AsmSymbolTableMap) : Result<AsmSymbolTableMap, CompilerError.CompilerError> =
    match find fun_name ast with
    | Ok (Fun f) -> Ok (Map.add fun_name (Fun { f with bytes_required = bytes_required }) ast)
    | Ok (Obj _) -> Error (CompilerError.InternalError "not a function")
    | Error e -> Error e

let getBytesRequired fun_name (ast: AsmSymbolTableMap) : Result<int, CompilerError.CompilerError> =
    match find fun_name ast with
    | Ok (Fun f) -> Ok f.bytes_required
    | Ok (Obj _) -> Error (CompilerError.InternalError "not a function")
    | Error e -> Error e

let addCalleeSavedRegsUsed fun_name regs (ast: AsmSymbolTableMap) : Result<AsmSymbolTableMap, CompilerError.CompilerError> =
    match find fun_name ast with
    | Ok (Fun f) ->
        Ok (Map.add fun_name (Fun { f with callee_saved_regs_used = Reg_set.union f.callee_saved_regs_used regs }) ast)
    | Ok (Obj _) -> Error (CompilerError.InternalError "not a function")
    | Error e -> Error e

let getCalleeSavedRegsUsed fun_name (ast: AsmSymbolTableMap) : Result<Reg_set.RegSet, CompilerError.CompilerError> =
    match find fun_name ast with
    | Ok (Fun f) -> Ok f.callee_saved_regs_used
    | Ok (Obj _) -> Error (CompilerError.InternalError "not a function")
    | Error e -> Error e

let getSize var_name (ast: AsmSymbolTableMap) : Result<int, CompilerError.CompilerError> =
    match find var_name ast with
    | Ok (Obj { t = Assembly.Byte }) -> Ok 1
    | Ok (Obj { t = Assembly.Longword }) -> Ok 4
    | Ok (Obj { t = Assembly.Quadword })
    | Ok (Obj { t = Assembly.Double }) -> Ok 8
    | Ok (Obj { t = Assembly.ByteArray { size = size } }) -> Ok size
    | Ok (Fun _) -> Error (CompilerError.InternalError "this is a function, not an object")
    | Error e -> Error e

let getType var_name (ast: AsmSymbolTableMap) : Result<Assembly.AsmType, CompilerError.CompilerError> =
    match find var_name ast with
    | Ok (Obj { t = t }) -> Ok t
    | Ok (Fun _) -> Error (CompilerError.InternalError "this is a function, not an object")
    | Error e -> Error e

let getAlignment var_name (ast: AsmSymbolTableMap) : Result<int, CompilerError.CompilerError> =
    match find var_name ast with
    | Ok (Obj { t = Assembly.Byte }) -> Ok 1
    | Ok (Obj { t = Assembly.Longword }) -> Ok 4
    | Ok (Obj { t = Assembly.Quadword })
    | Ok (Obj { t = Assembly.Double }) -> Ok 8
    | Ok (Obj { t = Assembly.ByteArray { alignment = alignment } }) -> Ok alignment
    | Ok (Fun _) -> Error (CompilerError.InternalError "this is a function, not an object")
    | Error e -> Error e

let isDefined fun_name (ast: AsmSymbolTableMap) : Result<bool, CompilerError.CompilerError> =
    match find fun_name ast with
    | Ok (Fun { defined = defined }) -> Ok defined
    | Ok _ -> Error (CompilerError.InternalError "not a function")
    | Error e -> Error e

let isStatic var_name (ast: AsmSymbolTableMap) : Result<bool, CompilerError.CompilerError> =
    match find var_name ast with
    | Ok (Obj o) -> Ok o.is_static
    | Ok (Fun _) -> Error (CompilerError.InternalError "functions don't have storage duration")
    | Error e -> Error e

let isConstant name (ast: AsmSymbolTableMap) : Result<bool, CompilerError.CompilerError> =
    match find name ast with
    | Ok (Obj { constant = true }) -> Ok true
    | Ok (Obj _) -> Ok false
    | Ok (Fun _) -> Error (CompilerError.InternalError "is_constant doesn't make sense for functions")
    | Error e -> Error e

let returnsOnStack fun_name (ast: AsmSymbolTableMap) : Result<bool, CompilerError.CompilerError> =
    match find fun_name ast with
    | Ok (Fun f) -> Ok f.return_on_stack
    | Ok (Obj _) -> Error (CompilerError.InternalError "this is an object, not a function")
    | Error e -> Error e

let paramRegsUsed fun_name (ast: AsmSymbolTableMap) : Result<Assembly.AsmReg list, CompilerError.CompilerError> =
    match find fun_name ast with
    | Ok (Fun f) -> Ok f.param_regs
    | Ok (Obj _) -> Error (CompilerError.InternalError "not a function")
    | Error e -> Error e

let returnRegsUsed fun_name (ast: AsmSymbolTableMap) : Result<Assembly.AsmReg list, CompilerError.CompilerError> =
    match find fun_name ast with
    | Ok (Fun f) -> Ok f.return_regs
    | Ok (Obj _) -> Error (CompilerError.InternalError "not a function")
    | Error e -> Error e
