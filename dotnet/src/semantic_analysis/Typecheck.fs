module Typecheck

open Types
open TypeUtils
open StringUtil
open ListUtil
open Ast.TypedExp
open ResultCE

module U = Ast.Untyped
module T = Ast.TypedExp
module UE = Ast.UntypedExp

type TypecheckState = {
    counter: UniqueIds.Counter
    st: Symbols.SymbolTableMap
    tt: TypeTable.TypeTableMap
}

let rec isLvalue { T.e = e } =
    match e with
    | T.Dereference _ | T.Subscript _ | T.Var _ | T.String _ | T.Arrow _ -> true
    | T.Dot(strct, _) -> isLvalue strct
    | _ -> false

let rec validateType tt = function
    | Types.Array(elem_type, _) ->
        if isComplete tt elem_type then validateType tt elem_type
        else Error (CompilerError.TypeError "Array of incomplete type")
    | Types.Pointer t -> validateType tt t
    | FunType(param_types, ret_type) ->
        result {
            do! resultTraverse (validateType tt) param_types |> Result.map ignore
            do! validateType tt ret_type
        }
    | Char | SChar | UChar | Int | Long | UInt | ULong | Double | Void
    | Structure _ ->
        Ok ()

let validateStructDefinition tt { U.tag = tag; U.members = members } =
    if TypeTable.mem tag tt then Error (CompilerError.TypeError "Structure was already declared")
    else
        let validateMember member_names { U.memberName = member_name; U.memberType = member_type } =
            result {
                if Set.contains member_name member_names then
                    return!
                        Error
                            (CompilerError.TypeError
                                ("Duplicate declaration of member "
                                 + member_name
                                 + " in structure "
                                 + tag))
                do! validateType tt member_type
                do! match member_type with
                    | Types.FunType _ ->
                        Error (CompilerError.TypeError "Can't declare structure member with function type")
                    | _ ->
                        if isComplete tt member_type then Ok ()
                        else Error (CompilerError.TypeError "Cannot declare structure member with incomplete type")
                return Set.add member_name member_names
            }
        resultFold validateMember Set.empty members |> Result.map ignore

let typecheckStructDecl tt ({ U.tag = tag; U.members = members } as sd) =
    result {
        do! if members <> [] then
                validateStructDefinition tt sd
            else Ok ()
        let! tt' =
            if members <> [] then
                let build_member_def (current_size, current_alignment, current_members)
                        { U.memberName = member_name; U.memberType = member_type } =
                    result {
                        let! member_alignment = getAlignment tt member_type
                        let offset =
                            Rounding.roundAwayFromZero member_alignment current_size
                        let member_entry = { TypeTable.member_type = member_type; TypeTable.offset = offset }
                        let new_alignment = Operators.max current_alignment member_alignment
                        let! member_size = getSize tt member_type
                        let new_size = offset + int member_size
                        let new_members =
                            Map.add member_name member_entry current_members
                        return (new_size, new_alignment, new_members)
                    }
                result {
                    let! (unpadded_size, alignment, member_defs) =
                        resultFold build_member_def (0, 1, Map.empty) members
                    let size = Rounding.roundAwayFromZero alignment unpadded_size
                    let struct_def = { TypeTable.alignment = alignment; TypeTable.size = size; TypeTable.members = member_defs }
                    return TypeTable.addStructDefinition tag struct_def tt
                }
            else
                Ok tt

        let cvt { U.memberName = member_name; U.memberType = member_type } =
            { Ast.Typed.memberName = member_name; Ast.Typed.memberType = member_type }
        return (tt', { Ast.Typed.tag = tag; Ast.Typed.members = List.map cvt members })
    }

let convertTo e target_type =
    let cast = T.Cast(target_type, e)
    setType cast target_type

let getCommonType tt t1 t2 =
    let t1 = if isCharacter t1 then Types.Int else t1
    let t2 = if isCharacter t2 then Types.Int else t2
    if t1 = t2 then Ok t1
    else if t1 = Types.Double || t2 = Double then Ok Double
    else
        result {
            let! size1 = getSize tt t1
            let! size2 = getSize tt t2
            if size1 = size2 then
                let! signed1 = isSigned t1
                return if signed1 then t2 else t1
            else if size1 > size2 then return t1
            else return t2
        }

let isZeroInt = function
    | Const.ConstInt i when i = 0 -> true
    | Const.ConstLong l when l = 0L -> true
    | Const.ConstUInt u when u = 0u -> true
    | Const.ConstULong ul when ul = 0UL -> true
    | _ -> false

let isNullPointerConstant = function
    | T.Constant c -> isZeroInt c
    | _ -> false

let getCommonPointerType e1 e2 =
    if e1.t = e2.t then Ok e1.t
    else if isNullPointerConstant e1.e then Ok e2.t
    else if isNullPointerConstant e2.e then Ok e1.t
    else if
        (e1.t = Pointer Void && isPointer e2.t)
        || (e2.t = Pointer Void && isPointer e1.t)
    then Ok (Pointer Void)
    else Error (CompilerError.TypeError "Expressions have incompatible types")

let convertByAssignment e target_type =
    if e.t = target_type then Ok e
    else if isArithmetic e.t && isArithmetic target_type then
        Ok (convertTo e target_type)
    else if isNullPointerConstant e.e && isPointer target_type then
        Ok (convertTo e target_type)
    else if
        (target_type = Pointer Void && isPointer e.t)
        || (isPointer target_type && e.t = Pointer Void)
    then Ok (convertTo e target_type)
    else Error (CompilerError.TypeError "Cannot convert type for asignment")

let typecheckVar st v =
    result {
        let! entry = Symbols.get v st
        let v_type = entry.symType
        let e = T.Var v
        return!
            match v_type with
            | FunType _ -> Error (CompilerError.TypeError "Tried to use function name as variable ")
            | _ -> Ok (setType e v_type)
    }

let typecheckConst c =
    let e = T.Constant c
    Ok (setType e (Const.typeOfConst c))

let optTypecheckResult f = function
    | Some e -> f e |> Result.map Some
    | None -> Ok None

let typecheckString s =
    let e = T.String s
    let t = Types.Array(Char, int64 (String.length s + 1))
    Ok (setType e t)

let rec typecheckExp st tt = function
    | UE.Var v -> typecheckVar st v
    | UE.Constant c -> typecheckConst c
    | UE.String s -> typecheckString s
    | UE.Cast(target_type, inner) -> typecheckCast st tt target_type inner
    | UE.Unary (Ast.Ops.Not, inner) -> typecheckNot st tt inner
    | UE.Unary (Ast.Ops.Complement, inner) -> typecheckComplement st tt inner
    | UE.Unary (Ast.Ops.Negate, inner) -> typecheckNegate st tt inner
    | UE.Binary (op, e1, e2) ->
        (match op with
         | Ast.Ops.And | Ast.Ops.Or -> typecheckLogical st tt op e1 e2
         | Ast.Ops.Add -> typecheckAddition st tt e1 e2
         | Ast.Ops.Subtract -> typecheckSubtraction st tt e1 e2
         | Ast.Ops.Multiply | Ast.Ops.Divide | Ast.Ops.Mod -> typecheckMultiplicative st tt op e1 e2
         | Ast.Ops.Equal | Ast.Ops.NotEqual -> typecheckEquality st tt op e1 e2
         | Ast.Ops.GreaterThan | Ast.Ops.GreaterOrEqual | Ast.Ops.LessThan | Ast.Ops.LessOrEqual ->
             typecheckComparison st tt op e1 e2)
    | UE.Assignment (lhs, rhs) -> typecheckAssignment st tt lhs rhs
    | UE.Conditional(condition, then_result, else_result) ->
        typecheckConditional st tt condition then_result else_result
    | UE.FunCall(f, args) -> typecheckFunCall st tt f args
    | UE.Dereference inner -> typecheckDereference st tt inner
    | UE.AddrOf inner -> typecheckAddrOf st tt inner
    | UE.Subscript(ptr, index) -> typecheckSubscript st tt ptr index
    | UE.SizeOfT t -> typecheckSizeOfT st tt t
    | UE.SizeOf e -> typecheckSizeOf st tt e
    | UE.Dot(strct, memberName) -> typecheckDotOperator st tt strct memberName
    | UE.Arrow(strct, memberName) -> typecheckArrowOperator st tt strct memberName

and typecheckCast st tt target_type inner =
    result {
        do! validateType tt target_type
        let! typed_inner = typecheckAndConvert st tt inner
        return!
            match (target_type, typed_inner.t) with
            | Types.Double, Types.Pointer _ | Pointer _, Double ->
                Error (CompilerError.TypeError "Cannot cast between pointer and double")
            | Void, _ ->
                let cast_exp = T.Cast(Void, typed_inner)
                Ok (setType cast_exp Void)
            | _ ->
                if not (isScalar target_type) then
                    Error (CompilerError.TypeError "Can only cast to scalar types or void")
                else if not (isScalar typed_inner.t) then
                    Error (CompilerError.TypeError "Can only cast scalar expressions to non-void type")
                else
                    let cast_exp = T.Cast(target_type, typed_inner)
                    Ok (setType cast_exp target_type)
    }

and typecheckScalar st tt e =
    result {
        let! typed_e = typecheckAndConvert st tt e
        if isScalar typed_e.t then return typed_e
        else return! Error (CompilerError.TypeError "A scalar operand is required")
    }

and typecheckNot st tt inner =
    result {
        let! typed_inner = typecheckScalar st tt inner
        let not_exp = T.Unary (Ast.Ops.Not, typed_inner)
        return setType not_exp Int
    }

and typecheckComplement st tt inner =
    result {
        let! typed_inner = typecheckAndConvert st tt inner
        if not (isInteger typed_inner.t) then
            return! Error (CompilerError.TypeError "Bitwise complement only valid for integer types")
        else
            let typed_inner =
                if isCharacter typed_inner.t then convertTo typed_inner Int
                else typed_inner
            let complement_exp = T.Unary (Ast.Ops.Complement, typed_inner)
            return setType complement_exp typed_inner.t
    }

and typecheckNegate st tt inner =
    result {
        let! typed_inner = typecheckAndConvert st tt inner
        if isArithmetic typed_inner.t then
            let typed_inner =
                if isCharacter typed_inner.t then convertTo typed_inner Int
                else typed_inner
            let negate_exp = T.Unary (Ast.Ops.Negate, typed_inner)
            return setType negate_exp typed_inner.t
        else return! Error (CompilerError.TypeError "Can only negate arithmetic types")
    }

and typecheckLogical st tt op e1 e2 =
    result {
        let! typed_e1 = typecheckScalar st tt e1
        let! typed_e2 = typecheckScalar st tt e2
        let typed_binexp = T.Binary (op, typed_e1, typed_e2)
        return setType typed_binexp Int
    }

and typecheckAddition st tt e1 e2 =
    result {
        let! typed_e1 = typecheckAndConvert st tt e1
        let! typed_e2 = typecheckAndConvert st tt e2
        if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
            let! common_type = getCommonType tt typed_e1.t typed_e2.t
            let converted_e1 = convertTo typed_e1 common_type
            let converted_e2 = convertTo typed_e2 common_type
            let add_exp = T.Binary (Ast.Ops.Add, converted_e1, converted_e2)
            return setType add_exp common_type
        else if isCompletePointer tt typed_e1.t && isInteger typed_e2.t then
            let converted_e2 = convertTo typed_e2 Types.Long
            let add_exp = T.Binary (Ast.Ops.Add, typed_e1, converted_e2)
            return setType add_exp typed_e1.t
        else if isCompletePointer tt typed_e2.t && isInteger typed_e1.t then
            let converted_e1 = convertTo typed_e1 Types.Long
            let add_exp = T.Binary (Ast.Ops.Add, converted_e1, typed_e2)
            return setType add_exp typed_e2.t
        else return! Error (CompilerError.TypeError "invalid operands for addition")
    }

and typecheckSubtraction st tt e1 e2 =
    result {
        let! typed_e1 = typecheckAndConvert st tt e1
        let! typed_e2 = typecheckAndConvert st tt e2
        if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
            let! common_type = getCommonType tt typed_e1.t typed_e2.t
            let converted_e1 = convertTo typed_e1 common_type
            let converted_e2 = convertTo typed_e2 common_type
            let sub_exp = T.Binary (Ast.Ops.Subtract, converted_e1, converted_e2)
            return setType sub_exp common_type
        else if isCompletePointer tt typed_e1.t && isInteger typed_e2.t then
            let converted_e2 = convertTo typed_e2 Types.Long
            let sub_exp = T.Binary (Ast.Ops.Subtract, typed_e1, converted_e2)
            return setType sub_exp typed_e1.t
        else if isCompletePointer tt typed_e1.t && typed_e1.t = typed_e2.t then
            let sub_exp = T.Binary (Ast.Ops.Subtract, typed_e1, typed_e2)
            return setType sub_exp Types.Long
        else return! Error (CompilerError.TypeError "Invalid operands for subtraction")
    }

and typecheckMultiplicative st tt op e1 e2 =
    result {
        let! typed_e1 = typecheckAndConvert st tt e1
        let! typed_e2 = typecheckAndConvert st tt e2
        if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
            let! common_type = getCommonType tt typed_e1.t typed_e2.t
            let converted_e1 = convertTo typed_e1 common_type
            let converted_e2 = convertTo typed_e2 common_type
            let binary_exp = T.Binary (op, converted_e1, converted_e2)
            return!
                match op with
                | Ast.Ops.Mod when common_type = Double -> Error (CompilerError.TypeError "Can't apply % to double")
                | Ast.Ops.Multiply | Ast.Ops.Divide | Ast.Ops.Mod -> Ok (setType binary_exp common_type)
                | _ ->
                    Error
                        (CompilerError.InternalError
                            (sprintf "%A" op
                             + " isn't a multiplicative operator"))
        else return! Error (CompilerError.TypeError "Can only multiply arithmetic types")
    }

and typecheckEquality st tt op e1 e2 =
    result {
        let! typed_e1 = typecheckAndConvert st tt e1
        let! typed_e2 = typecheckAndConvert st tt e2
        let! common_type =
            if isPointer typed_e1.t || isPointer typed_e2.t then
                getCommonPointerType typed_e1 typed_e2
            else if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
                getCommonType tt typed_e1.t typed_e2.t
            else Error (CompilerError.TypeError "Invalid operands for equality")
        let converted_e1 = convertTo typed_e1 common_type
        let converted_e2 = convertTo typed_e2 common_type
        let binary_exp = T.Binary (op, converted_e1, converted_e2)
        return setType binary_exp Int
    }

and typecheckComparison st tt op e1 e2 =
    result {
        let! typed_e1 = typecheckAndConvert st tt e1
        let! typed_e2 = typecheckAndConvert st tt e2
        let! common_type =
            if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
                getCommonType tt typed_e1.t typed_e2.t
            else if isPointer typed_e1.t && typed_e1.t = typed_e2.t then Ok typed_e1.t
            else Error (CompilerError.TypeError "invalid types for comparions")
        let converted_e1 = convertTo typed_e1 common_type
        let converted_e2 = convertTo typed_e2 common_type
        let binary_exp = T.Binary (op, converted_e1, converted_e2)
        return setType binary_exp Int
    }

and typecheckAssignment st tt lhs rhs =
    result {
        let! typed_lhs = typecheckAndConvert st tt lhs
        if isLvalue typed_lhs then
            let lhs_type = getType typed_lhs
            let! typed_rhs = typecheckAndConvert st tt rhs
            let! converted_rhs = convertByAssignment typed_rhs lhs_type
            let assign_exp = T.Assignment (typed_lhs, converted_rhs)
            return setType assign_exp lhs_type
        else return! Error (CompilerError.TypeError "left hand side of assignment is invalid lvalue")
    }

and typecheckConditional st tt condition then_exp else_exp =
    result {
        let! typed_conditon = typecheckScalar st tt condition
        let! typed_then = typecheckAndConvert st tt then_exp
        let! typed_else = typecheckAndConvert st tt else_exp
        let! result_type =
            if typed_then.t = Void && typed_else.t = Void then Ok Types.Void
            else if isPointer typed_then.t || isPointer typed_else.t then
                getCommonPointerType typed_then typed_else
            else if isArithmetic typed_then.t && isArithmetic typed_else.t then
                getCommonType tt typed_then.t typed_else.t
            else if typed_then.t = typed_else.t then Ok typed_then.t
            else Error (CompilerError.TypeError "Invalid operands for conditional")
        let converted_then = convertTo typed_then result_type
        let converted_else = convertTo typed_else result_type
        let conditional_exp =
            T.Conditional(typed_conditon, converted_then, converted_else)
        return setType conditional_exp result_type
    }

and typecheckFunCall st tt f args =
    result {
        let! f_entry = Symbols.get f st
        let f_type = f_entry.symType
        return!
            match f_type with
            | FunType(param_types, ret_type) ->
                if List.length param_types <> List.length args then
                    Error (CompilerError.TypeError "Function called with wrong number of arguments")
                else
                    let process_arg arg param_t =
                        result {
                            let! typed_arg = typecheckAndConvert st tt arg
                            return! convertByAssignment typed_arg param_t
                        }
                    result {
                        let! converted_args = resultTraverse2 process_arg args param_types
                        let call_exp = T.FunCall(f, converted_args)
                        return setType call_exp ret_type
                    }
            | _ -> Error (CompilerError.TypeError "Tried to use variable as function name")
    }

and typecheckDereference st tt inner =
    result {
        let! typed_inner = typecheckAndConvert st tt inner
        return!
            match getType typed_inner with
            | Pointer Void -> Error (CompilerError.TypeError "Can't dereference pointer to void")
            | Pointer referenced_t ->
                let deref_exp = T.Dereference typed_inner
                Ok (setType deref_exp referenced_t)
            | _ -> Error (CompilerError.TypeError "Tried to dereference non-pointer")
    }

and typecheckAddrOf st tt inner =
    result {
        let! typed_inner = typecheckExp st tt inner
        if isLvalue typed_inner then
            let inner_t = getType typed_inner
            let addr_exp = T.AddrOf typed_inner
            return setType addr_exp (Pointer inner_t)
        else return! Error (CompilerError.TypeError "Cannot take address of non-value")
    }

and typecheckSubscript st tt e1 e2 =
    result {
        let! typed_e1 = typecheckAndConvert st tt e1
        let! typed_e2 = typecheckAndConvert st tt e2
        let! ptr_type, converted_e1, converted_e2 =
            if isCompletePointer tt typed_e1.t && isInteger typed_e2.t then
                Ok (typed_e1.t, typed_e1, convertTo typed_e2 Types.Long)
            else if isCompletePointer tt typed_e2.t && isInteger typed_e1.t then
                Ok (typed_e2.t, convertTo typed_e1 Long, typed_e2)
            else Error (CompilerError.TypeError "Invalid types for subscript operation")
        let! result_type =
            match ptr_type with
            | Pointer referenced -> Ok referenced
            | _ -> Error (CompilerError.InternalError "typechecking subscript")
        let subscript_exp =
            T.Subscript(converted_e1, converted_e2)
        return setType subscript_exp result_type
    }

and typecheckSizeOfT st tt t =
    result {
        do! validateType tt t
        if isComplete tt t then
            let sizeof_exp = T.SizeOfT t
            return setType sizeof_exp ULong
        else return! Error (CompilerError.TypeError "Can't apply sizeof to incomplete type")
    }

and typecheckSizeOf st tt inner =
    result {
        let! typed_inner = typecheckExp st tt inner
        if isComplete tt typed_inner.t then
            let sizeof_exp = T.SizeOf typed_inner
            return setType sizeof_exp ULong
        else return! Error (CompilerError.TypeError "Can't apply sizeof to incomplete type")
    }

and typecheckAndConvert st tt e =
    result {
        let! typed_e = typecheckExp st tt e
        return!
            match typed_e.t with
            | Types.Structure _ when not (isComplete tt typed_e.t) ->
                Error (CompilerError.TypeError "Incomplete structure type not permitted here")
            | Types.Array(elem_type, _) ->
                let addr_exp = T.AddrOf typed_e
                Ok (setType addr_exp (Pointer elem_type))
            | _ -> Ok typed_e
    }

and typecheckDotOperator st tt strct memberName =
    result {
        let! typed_strct = typecheckAndConvert st tt strct
        return!
            match typed_strct.t with
            | Types.Structure tag ->
                result {
                    let! struct_def = TypeTable.find tag tt
                    return!
                        match Map.tryFind memberName struct_def.members with
                        | Some entry ->
                            let dot_exp = T.Dot(typed_strct, memberName)
                            Ok (setType dot_exp entry.member_type)
                        | None ->
                            Error (CompilerError.TypeError ("Struct type " + tag + " has no member " + memberName))
                }
            | _ ->
                Error
                    (CompilerError.TypeError "Dot operator can only be applied to expressions with structure type")
    }

and typecheckArrowOperator st tt strct_ptr memberName =
    result {
        let! typed_strct_ptr = typecheckAndConvert st tt strct_ptr
        return!
            match typed_strct_ptr.t with
            | Types.Pointer (Structure tag) ->
                result {
                    let! struct_def = TypeTable.find tag tt
                    return!
                        match Map.tryFind memberName struct_def.members with
                        | Some entry ->
                            let arrow_exp = T.Arrow(typed_strct_ptr, memberName)
                            Ok (setType arrow_exp entry.member_type)
                        | None ->
                            Error
                                (CompilerError.TypeError
                                    ("Struct type " + tag + " is incomplete or has no member " + memberName))
                }
            | _ -> Error (CompilerError.TypeError "Arrow operator can only be applied to pointers to structure")
    }

let rec staticInitHelper (counter, st) tt var_type init =
    match (var_type, init) with
    | Types.Array(elem_type, size), UE.SingleInit (UE.String s) ->
        if isCharacter elem_type then
            (match int size - String.length s with
             | 0 -> Ok ((counter, st), [ Initializers.StringInit (s, false) ])
             | 1 -> Ok ((counter, st), [ Initializers.StringInit (s, true) ])
             | n when n > 0 ->
                 Ok ((counter, st), [ Initializers.StringInit (s, true); Initializers.ZeroInit (n - 1) ])
             | _ -> Error (CompilerError.TypeError "string is too long for initializer"))
        else
            Error
                (CompilerError.TypeError "Can't initialize array of non-character type with string literal")
    | Types.Array _, UE.SingleInit _ ->
        Error (CompilerError.TypeError "Can't initialize array from scalar value")
    | Types.Pointer Char, UE.SingleInit (UE.String s) ->
        let counter', str_id, st' = Symbols.addStringWithCounter counter s st
        Ok ((counter', st'), [ Initializers.PointerInit str_id ])
    | _, UE.SingleInit (UE.String _) ->
        Error (CompilerError.TypeError "String literal can only initialize char *")
    | Structure tag, UE.CompoundInit inits ->
        result {
            let! struct_def = TypeTable.find tag tt
            let! members = TypeTable.getMembers tag tt
            if List.length inits > List.length members then
                return! Error (CompilerError.TypeError "Too many elements in struct initializer")
            else
                let handle_member (cs, current_offset, current_inits) (memb: TypeTable.MemberDef) init =
                    result {
                        let padding =
                            if current_offset < memb.offset then
                                [ Initializers.ZeroInit (memb.offset - current_offset) ]
                            else []
                        let! cs', more_static_inits = staticInitHelper cs tt memb.member_type init
                        let new_inits = current_inits @ padding @ more_static_inits
                        let! member_size = getSize tt memb.member_type
                        let new_offset = memb.offset + int member_size
                        return (cs', new_offset, new_inits)
                    }
                let initialized_members = ListUtil.take (List.length inits) members
                let! cs', initialized_size, explicit_initializers =
                    resultFold2 handle_member ((counter, st), 0, []) initialized_members inits
                let trailing_padding =
                    if initialized_size < struct_def.size then
                        [ Initializers.ZeroInit (struct_def.size - initialized_size) ]
                    else []
                return (cs', explicit_initializers @ trailing_padding)
        }
    | Structure _, UE.SingleInit _ ->
        Error (CompilerError.TypeError " Can't initialize static structure with scalar value")
    | _, UE.SingleInit (UE.Constant c) when isZeroInt c ->
        result {
            let! zero_inits = Initializers.zero tt var_type
            return ((counter, st), zero_inits)
        }
    | Types.Pointer _, _ -> Error (CompilerError.TypeError "invalid static initializer for pointer")
    | _, UE.SingleInit (UE.Constant c) ->
        if isArithmetic var_type then
            result {
                let! converted = ConstConvert.constConvert var_type c
                let init_val =
                    match converted with
                    | Const.ConstChar c -> Initializers.CharInit c
                    | Const.ConstInt i -> Initializers.IntInit i
                    | Const.ConstLong l -> Initializers.LongInit l
                    | Const.ConstUChar uc -> Initializers.UCharInit uc
                    | Const.ConstUInt ui -> Initializers.UIntInit ui
                    | Const.ConstULong ul -> Initializers.ULongInit ul
                    | Const.ConstDouble d -> Initializers.DoubleInit d
                return ((counter, st), [ init_val ])
            }
        else
            Error
                (CompilerError.InternalError
                    ("should have already rejected initializer with type "
                     + Types.show var_type))
    | _, UE.SingleInit _ -> Error (CompilerError.TypeError "non-constant initializer")
    | Array(elem_type, size), UE.CompoundInit inits ->
        result {
            let! cs', static_inits_nested =
                resultFold
                    (fun (cs, acc) init ->
                        staticInitHelper cs tt elem_type init
                        |> Result.map (fun (cs', inits) -> (cs', acc @ [inits])))
                    ((counter, st), []) inits
            let static_inits = List.concat static_inits_nested
            let! padding =
                match int size - List.length inits with
                | 0 -> Ok []
                | n when n > 0 ->
                    result {
                        let! elem_size = getSize tt elem_type
                        let zero_bytes = int elem_size * n
                        return [ Initializers.ZeroInit zero_bytes ]
                    }
                | _ -> Error (CompilerError.TypeError "Too many values in static initializer")
            return (cs', static_inits @ padding)
        }
    | _, UE.CompoundInit _ ->
        Error (CompilerError.TypeError "Can't use compound initializer for object with scalar type")

let toStaticInit (counter, st) tt var_type init =
    staticInitHelper (counter, st) tt var_type init
    |> Result.map (fun (cs', inits) -> (cs', Symbols.Initial inits))

let rec makeZeroInit tt t =
    let scalar c = Ok (T.SingleInit { e = Constant c; t = t })
    match t with
    | Types.Array(elem_type, size) ->
        result {
            let! elems = resultTraverse (fun _ -> makeZeroInit tt elem_type) (ListUtil.makeList (int size) ())
            return T.CompoundInit (t, elems)
        }
    | Structure tag ->
        result {
            let! members = TypeTable.getMembers tag tt
            let! member_inits = resultTraverse (fun (m: TypeTable.MemberDef) -> makeZeroInit tt m.member_type) members
            return T.CompoundInit (t, member_inits)
        }
    | Char | SChar -> scalar (Const.ConstChar 0y)
    | Int -> scalar (Const.ConstInt 0)
    | UChar -> scalar (Const.ConstUChar 0uy)
    | UInt -> scalar (Const.ConstUInt 0u)
    | Long -> scalar (Const.ConstLong 0L)
    | ULong | Pointer _ -> scalar (Const.ConstULong 0UL)
    | Double -> scalar (Const.ConstDouble 0.0)
    | (FunType _ | Void) as t ->
        Error
            (CompilerError.InternalError
                ("can't create zero initializer with type "
                 + Types.show t))

let rec typecheckInit st tt target_type init =
    match (target_type, init) with
    | Types.Array(elem_type, size), UE.SingleInit (UE.String s) ->
        if not (isCharacter elem_type) then
            Error (CompilerError.TypeError "Can't initialize non-character type with string literal")
        else if String.length s > int size then
            Error (CompilerError.TypeError "Too many characters in string literal")
        else Ok (T.SingleInit (setType (T.String s) target_type))
    | Types.Structure tag, UE.CompoundInit init_list ->
        result {
            let! members = TypeTable.getMembers tag tt
            if List.length init_list > List.length members then
                return! Error (CompilerError.TypeError "Too many elements in structure initializer")
            else
                let initialized_members, uninitialized_members =
                    ListUtil.takeDrop (List.length init_list) members
                let! typechecked_members =
                    resultTraverse2
                        (fun (memb: TypeTable.MemberDef) init -> typecheckInit st tt memb.member_type init)
                        initialized_members init_list
                let! padding =
                    resultTraverse
                        (fun (m: TypeTable.MemberDef) -> makeZeroInit tt m.member_type)
                        uninitialized_members
                return T.CompoundInit (target_type, typechecked_members @ padding)
        }
    | _, UE.SingleInit e ->
        result {
            let! typechecked_e = typecheckAndConvert st tt e
            let! cast_exp = convertByAssignment typechecked_e target_type
            return T.SingleInit cast_exp
        }
    | Array(elem_type, size), UE.CompoundInit inits ->
        if List.length inits > int size then
            Error (CompilerError.TypeError "too many values in initializer ")
        else
            result {
                let! typechecked_inits = resultTraverse (typecheckInit st tt elem_type) inits
                let! padding =
                    resultTraverse
                        (fun _ -> makeZeroInit tt elem_type)
                        (ListUtil.makeList (int size - List.length inits) ())
                return T.CompoundInit (target_type, typechecked_inits @ padding)
            }
    | _ -> Error (CompilerError.TypeError "Can't initializer scalar value from compound initializer")

let rec typecheckBlock state ret_type (U.Block b) =
    resultFold
        (fun (state, acc) item ->
            typecheckBlockItem state ret_type item
            |> Result.map (fun (state', item') -> (state', acc @ [item'])))
        (state, []) b
    |> Result.map (fun (state', items) -> (state', Ast.Typed.Block items))

and typecheckBlockItem state ret_type = function
    | U.Stmt s ->
        typecheckStatement state ret_type s
        |> Result.map (fun (state', s') -> (state', Ast.Typed.Stmt s'))
    | U.Decl d ->
        typecheckLocalDecl state d
        |> Result.map (fun (state', d') -> (state', Ast.Typed.Decl d'))

and typecheckStatement state ret_type = function
    | U.Return (Some e) ->
        if ret_type = Types.Void then
            Error (CompilerError.TypeError "function with void return type cannot return a value")
        else
            result {
                let! typed_e = typecheckAndConvert state.st state.tt e
                let! converted_e = convertByAssignment typed_e ret_type
                return (state, Ast.Typed.Return (Some converted_e))
            }
    | U.Return None ->
        if ret_type = Void then Ok (state, Ast.Typed.Return None)
        else Error (CompilerError.TypeError "Function with non-void return type must return a value")
    | U.Expression e ->
        typecheckAndConvert state.st state.tt e |> Result.map (fun e' -> (state, Ast.Typed.Expression e'))
    | U.If(condition, thenClause, elseClause) ->
        result {
            let! typedCond = typecheckScalar state.st state.tt condition
            let! (state', typedThen) = typecheckStatement state ret_type thenClause
            let! (state'', typedElse) =
                match elseClause with
                | Some s ->
                    typecheckStatement state' ret_type s
                    |> Result.map (fun (state'', s') -> (state'', Some s'))
                | None -> Ok (state', None)
            return (state'', Ast.Typed.If(typedCond, typedThen, typedElse))
        }
    | U.Compound block ->
        typecheckBlock state ret_type block
        |> Result.map (fun (state', b') -> (state', Ast.Typed.Compound b'))
    | U.While(condition, body, id) ->
        result {
            let! typedCond = typecheckScalar state.st state.tt condition
            let! (state', typedBody) = typecheckStatement state ret_type body
            return (state', Ast.Typed.While(typedCond, typedBody, id))
        }
    | U.DoWhile(body, condition, id) ->
        result {
            let! (state', typedBody) = typecheckStatement state ret_type body
            let! typedCond = typecheckScalar state'.st state'.tt condition
            return (state', Ast.Typed.DoWhile(typedBody, typedCond, id))
        }
    | U.For(init, condition, post, body, id) ->
        result {
            let! state', typechecked_for_init =
                match init with
                | U.InitDecl { U.storageClass = Some _ } ->
                    Error
                        (CompilerError.TypeError "Storage class not permitted on declaration in for loop header")
                | U.InitDecl d ->
                    typecheckLocalVarDecl state d
                    |> Result.map (fun (state', d') -> (state', Ast.Typed.InitDecl d'))
                | U.InitExp e ->
                    optTypecheckResult (typecheckAndConvert state.st state.tt) e
                    |> Result.map (fun e' -> (state, Ast.Typed.InitExp e'))
            let! typedCond = optTypecheckResult (typecheckScalar state'.st state'.tt) condition
            let! typedPost = optTypecheckResult (typecheckAndConvert state'.st state'.tt) post
            let! (state'', typedBody) = typecheckStatement state' ret_type body
            return (state'', Ast.Typed.For(typechecked_for_init, typedCond, typedPost, typedBody, id))
        }
    | U.Null -> Ok (state, Ast.Typed.Null)
    | U.Break s -> Ok (state, Ast.Typed.Break s)
    | U.Continue s -> Ok (state, Ast.Typed.Continue s)

and typecheckLocalDecl state = function
    | U.VarDecl vd ->
        typecheckLocalVarDecl state vd
        |> Result.map (fun (state', vd') -> (state', Ast.Typed.VarDecl vd'))
    | U.FunDecl fd ->
        typecheckFnDecl state fd
        |> Result.map (fun (state', fd') -> (state', Ast.Typed.FunDecl fd'))
    | U.StructDecl sd ->
        typecheckStructDecl state.tt sd
        |> Result.map (fun (tt', sd') -> ({ state with tt = tt' }, Ast.Typed.StructDecl sd'))

and typecheckLocalVarDecl state ({ U.name = name; U.varType = varType; U.init = init; U.storageClass = storageClass }: U.VariableDeclaration) : Result<TypecheckState * Ast.Typed.VariableDeclaration, CompilerError.CompilerError> =
    result {
        do! if varType = Void then Error (CompilerError.TypeError "No void declarations")
            else validateType state.tt varType
        return!
            match storageClass with
            | Some Ast.StorageClass.Extern ->
                result {
                    if Option.isSome init then
                        return! Error (CompilerError.TypeError "initializer on local extern declaration")
                    do! match Symbols.getOpt name state.st with
                        | Some { Symbols.symType = t } ->
                            if t <> varType then
                                Error (CompilerError.TypeError "Variable redeclared with different type")
                            else Ok ()
                        | None -> Ok ()
                    let st' =
                        match Symbols.getOpt name state.st with
                        | Some _ -> state.st
                        | None -> Symbols.addStaticVar name varType true Symbols.NoInitializer state.st
                    return ({ state with st = st' },
                            { Ast.Typed.name = name
                              Ast.Typed.init = None
                              Ast.Typed.storageClass = storageClass
                              Ast.Typed.varType = varType })
                }
            | _ when not (isComplete state.tt varType) ->
                Error (CompilerError.TypeError "Cannot define a variable with an incomplete type")
            | Some Ast.StorageClass.Static ->
                result {
                    let! zero_inits = Initializers.zero state.tt varType
                    let zero_init = Symbols.Initial zero_inits
                    let! (counter', st'), static_init =
                        match init with
                        | Some i -> toStaticInit (state.counter, state.st) state.tt varType i
                        | None -> Ok ((state.counter, state.st), zero_init)
                    let st'' = Symbols.addStaticVar name varType false static_init st'
                    return ({ state with counter = counter'; st = st'' },
                            { Ast.Typed.name = name
                              Ast.Typed.init = None
                              Ast.Typed.storageClass = storageClass
                              Ast.Typed.varType = varType })
                }
            | None ->
                let st' = Symbols.addAutomaticVar name varType state.st
                let state' = { state with st = st' }
                result {
                    let! typechecked_init = optTypecheckResult (typecheckInit state'.st state'.tt varType) init
                    return (state',
                            { Ast.Typed.name = name
                              Ast.Typed.varType = varType
                              Ast.Typed.storageClass = storageClass
                              Ast.Typed.init = typechecked_init })
                }
    }

and typecheckFnDecl state (fd: U.FunctionDeclaration) : Result<TypecheckState * Ast.Typed.FunctionDeclaration, CompilerError.CompilerError> =
    let name = fd.name
    let funType = fd.funType
    let paramList = fd.paramList
    let body = fd.body
    let storageClass = fd.storageClass
    result {
        do! validateType state.tt funType
        // Note: we do this _before_ adjusting param types
        let adjustParamType = function
            | Types.Array(elem_type, _) -> Ok (Types.Pointer elem_type)
            | Void -> Error (CompilerError.TypeError "No void params allowed")
            | t -> Ok t
        let! param_ts, return_t, funType =
            match funType with
            | Types.FunType(_, Types.Array(_, _)) ->
                Error (CompilerError.TypeError "A function cannot return an array")
            | Types.FunType(param_types, ret_type) ->
                result {
                    let! param_types = resultTraverse adjustParamType param_types
                    return (param_types, ret_type, Types.FunType(param_types, ret_type))
                }
            | _ ->
                Error (CompilerError.InternalError "function has non-function type")
        let has_body = Option.isSome body
        // can't define a function with incomplete return or param type
        if
            has_body
            && not
                 ((return_t = Void || isComplete state.tt return_t)
                  && List.forall (isComplete state.tt) param_ts)
        then
            return!
                Error
                    (CompilerError.TypeError "Can't define a function with incomplete return type or parameter type")
        else
            let isGlobal = storageClass <> Some Ast.StorageClass.Static
            // helper function to reconcile current and previous declarations
            let checkAgainstPrevious { Symbols.symType = prev_t; Symbols.attrs = attrs } =
                if prev_t <> funType then
                    Error (CompilerError.TypeError ("Redeclared function " + name + " with a different type"))
                else
                    match attrs with
                    | Symbols.FunAttr { isGlobal = prev_global; defined = prev_defined } ->
                        if prev_defined && has_body then
                            Error (CompilerError.TypeError ("Defined body of function " + name + "twice"))
                        else if prev_global && storageClass = Some Ast.StorageClass.Static then
                            Error (CompilerError.TypeError "Static function declaration follows non-static")
                        else
                            let defined = has_body || prev_defined
                            Ok (defined, prev_global)
                    | _ ->
                        Error
                            (CompilerError.InternalError "symbol has function type but not function attributes")
            let old_decl = Symbols.getOpt name state.st
            let! defined, isGlobal =
                match old_decl with
                | Some old_d -> checkAgainstPrevious old_d
                | None -> Ok (has_body, isGlobal)
            let st' = Symbols.addFun name funType isGlobal defined state.st
            let st'' =
                if has_body then
                    List.fold2 (fun s p t -> Symbols.addAutomaticVar p t s) st' paramList param_ts
                else st'
            let state' = { state with st = st'' }
            let! state'', body =
                match body with
                | Some b ->
                    typecheckBlock state' return_t b
                    |> Result.map (fun (state'', b') -> (state'', Some b'))
                | None -> Ok (state', None)
            let fd' : Ast.Typed.FunctionDeclaration =
                { funType = funType
                  name = name
                  paramList = paramList
                  body = body
                  storageClass = storageClass }
            return (state'', fd')
    }

let typecheckFileScopeVarDecl state
    ({ U.name = name; U.varType = varType; U.init = init; U.storageClass = storageClass }: U.VariableDeclaration)
    =
    result {
        do! if varType = Void then Error (CompilerError.TypeError "void variables not allowed")
            else validateType state.tt varType
        let default_init =
            if storageClass = Some Ast.StorageClass.Extern then Symbols.NoInitializer else Symbols.Tentative
        let! (counter', st'), static_init =
            match init with
            | Some i -> toStaticInit (state.counter, state.st) state.tt varType i
            | None -> Ok ((state.counter, state.st), default_init)
        if not (isComplete state.tt varType || static_init = Symbols.NoInitializer) then
            // note: some compilers permit tentative definition with incomplete type, if
            // it's completed later in the file. we don't.
            return! Error (CompilerError.TypeError "Can't define a variable with an incomplete type ")
        else
            let current_global = storageClass <> Some Ast.StorageClass.Static
            let old_decl = Symbols.getOpt name st'
            let checkAgainstPrevious { Symbols.symType = t; Symbols.attrs = attrs } =
                if t <> varType then Error (CompilerError.TypeError "Variable redeclared with different type")
                else
                    match attrs with
                    | Symbols.StaticAttr { isGlobal = prev_global; init = prev_init } ->
                        result {
                            let! isGlobal =
                                if storageClass = Some Ast.StorageClass.Extern then Ok prev_global
                                else if current_global = prev_global then Ok current_global
                                else Error (CompilerError.TypeError "Conflicting variable linkage")
                            let! init =
                                match (prev_init, static_init) with
                                | Symbols.Initial _, Symbols.Initial _ ->
                                    Error (CompilerError.TypeError "Conflicting global variable definition")
                                | Symbols.Initial _, _ -> Ok prev_init
                                | Symbols.Tentative, (Symbols.Tentative | Symbols.NoInitializer) -> Ok Symbols.Tentative
                                | _, Symbols.Initial _ | Symbols.NoInitializer, _ -> Ok static_init
                            return (isGlobal, init)
                        }
                    | _ ->
                        Error
                            (CompilerError.InternalError
                                "file-scope variable previously declared as local variable or function")
            let! isGlobal, init =
                match old_decl with
                | Some old_d -> checkAgainstPrevious old_d
                | None -> Ok (current_global, static_init)
            let st'' = Symbols.addStaticVar name varType isGlobal init st'
            return ({ state with counter = counter'; st = st'' },
                    { Ast.Typed.name = name
                      Ast.Typed.varType = varType
                      Ast.Typed.init = None
                      Ast.Typed.storageClass = storageClass })
    }

let typecheckGlobalDecl state = function
    | U.FunDecl fd ->
        typecheckFnDecl state fd
        |> Result.map (fun (state', d) -> (state', Ast.Typed.FunDecl d))
    | U.VarDecl vd ->
        typecheckFileScopeVarDecl state vd
        |> Result.map (fun (state', d) -> (state', Ast.Typed.VarDecl d))
    | U.StructDecl sd ->
        typecheckStructDecl state.tt sd
        |> Result.map (fun (tt', d) -> ({ state with tt = tt' }, Ast.Typed.StructDecl d))

let typecheck state (Ast.Untyped.Program decls) : Result<TypecheckState * Ast.Typed.TypedProgram, CompilerError.CompilerError> =
    resultFold
        (fun (state, acc) decl ->
            typecheckGlobalDecl state decl
            |> Result.map (fun (state', d) -> (state', acc @ [d])))
        (state, []) decls
    |> Result.map (fun (state', decls) -> (state', Ast.Typed.Program decls))
