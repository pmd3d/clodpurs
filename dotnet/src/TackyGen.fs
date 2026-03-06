module TackyGen

open Ast.Ops
open ResultCE
module Ast = Ast.Typed
module T = Tacky

let breakLabel id = "break." + id
let continueLabel id = "continue." + id

(* use this as the "result" of void expressions that don't return a result *)
let dummyOperand = T.Constant Const.intZero

let createTmp counter st t =
    let counter', name = UniqueIds.makeTemporary counter
    let st' = Symbols.addAutomaticVar name t st
    (counter', st', name)

let getPtrScale tt = function
    | Types.Pointer referenced ->
        TypeUtils.getSize tt referenced |> Result.map int
    | t ->
        Error
            (CompilerError.InternalError
                ("tried to get scale of non-pointer type: "
                 + Types.show t))

let getMemberOffset tt memberName = function
    | Types.Structure tag ->
        match TypeTable.tryFind tag tt with
        | Some entry ->
            match Map.tryFind memberName entry.members with
            | Some m -> Ok m.offset
            | None ->
                Error
                    (CompilerError.InternalError
                        ("failed to find member "
                         + memberName
                         + " in structure "
                         + tag))
        | None ->
            Error
                (CompilerError.InternalError
                    ("failed to find structure "
                     + tag
                     + " in type table"))
    | t ->
        Error
            (CompilerError.InternalError
                ("tried to get offset of member "
                 + memberName
                 + " within non-structure type "
                 + Types.show t))

let getMemberPointerOffset tt memberName = function
    | Types.Pointer t -> getMemberOffset tt memberName t
    | t ->
        Error
            (CompilerError.InternalError
                ("trying to get member through pointer but "
                 + Types.show t
                 + " is not a pointer type"))

let convertOp = function
    | Complement -> T.Complement
    | Negate -> T.Negate
    | Not -> T.Not

let convertBinop = function
    | Add -> Ok T.Add
    | Subtract -> Ok T.Subtract
    | Multiply -> Ok T.Multiply
    | Divide -> Ok T.Divide
    | Mod -> Ok T.Mod
    | Equal -> Ok T.Equal
    | NotEqual -> Ok T.NotEqual
    | LessThan -> Ok T.LessThan
    | LessOrEqual -> Ok T.LessOrEqual
    | GreaterThan -> Ok T.GreaterThan
    | GreaterOrEqual -> Ok T.GreaterOrEqual
    | And | Or ->
        Error
            (CompilerError.InternalError
                "cannot convert these directly to TACKY binops")

let evalSize tt t =
    TypeUtils.getSize tt t
    |> Result.map (fun size -> T.Constant(Const.ConstULong(uint64 size)))

(* an expression result that may or may not be lvalue converted *)
type ExpResult =
    | PlainOperand of T.TackyVal
    | DereferencedPointer of T.TackyVal
    | SubObject of string * int

(* return Result<(counter, st, instructions, ExpResult), CompilerError> *)
let rec emitTackyForExp counter st tt (exp: Ast.Exp) =
    let e = exp.e
    let t = exp.t
    match e with
    | Ast.InnerExp.Constant c -> Ok (counter, st, [], PlainOperand(T.Constant c))
    | Ast.InnerExp.Var v -> Ok (counter, st, [], PlainOperand(T.Var v))
    | Ast.InnerExp.String s ->
        let counter', str_id, st' = Symbols.addStringWithCounter counter s st
        Ok (counter', st', [], PlainOperand(T.Var str_id))
    | Ast.InnerExp.Cast(target_type, e) ->
        emitCastExpression counter st tt target_type e
    | Ast.InnerExp.Unary(op, inner) -> emitUnaryExpression counter st tt t op inner
    | Ast.InnerExp.Binary(And, e1, e2) -> emitAndExpression counter st tt e1 e2
    | Ast.InnerExp.Binary(Or, e1, e2) -> emitOrExpression counter st tt e1 e2
    | Ast.InnerExp.Binary(Add, e1, e2) when TypeUtils.isPointer t ->
        emitPointerAddition counter st tt t e1 e2
    | Ast.InnerExp.Binary(Subtract, ptr, index) when TypeUtils.isPointer t ->
        emitSubtractionFromPointer counter st tt t ptr index
    | Ast.InnerExp.Binary(Subtract, e1, e2) when TypeUtils.isPointer e1.t ->
        emitPointerDiff counter st tt t e1 e2
    | Ast.InnerExp.Binary(op, e1, e2) -> emitBinaryExpression counter st tt t op e1 e2
    | Ast.InnerExp.Assignment(lhs, rhs) -> emitAssignment counter st tt lhs rhs
    | Ast.InnerExp.Conditional(condition, then_result, else_result) ->
        emitConditionalExpression counter st tt t condition then_result else_result
    | Ast.InnerExp.FunCall(f, args) -> emitFunCall counter st tt t f args
    | Ast.InnerExp.Dereference inner -> emitDereference counter st tt inner
    | Ast.InnerExp.AddrOf inner -> emitAddrOf counter st tt t inner
    | Ast.InnerExp.Subscript(ptr, index) ->
        emitSubscript counter st tt t ptr index
    | Ast.InnerExp.SizeOfT szt ->
        result {
            let! sz = evalSize tt szt
            return (counter, st, [], PlainOperand sz)
        }
    | Ast.InnerExp.SizeOf inner ->
        result {
            let! sz = evalSize tt inner.t
            return (counter, st, [], PlainOperand sz)
        }
    | Ast.InnerExp.Dot(strct, mbr) ->
        emitDotOperator counter st tt t strct mbr
    | Ast.InnerExp.Arrow(strct, mbr) ->
        emitArrowOperator counter st tt t strct mbr

and emitUnaryExpression counter st tt t op inner =
    result {
        let! counter', st', eval_inner, v = emitTackyAndConvert counter st tt inner
        let counter'', st'', dst_name = createTmp counter' st' t
        let dst = T.Var dst_name
        let tacky_op = convertOp op
        let instructions =
            eval_inner @ [ T.Unary { op = tacky_op; src = v; dst = dst } ]
        return (counter'', st'', instructions, PlainOperand dst)
    }

and emitCastExpression counter st tt target_type inner =
    result {
        let! counter', st', eval_inner, r = emitTackyAndConvert counter st tt inner
        let inner_type = TypeUtils.getType inner
        if inner_type = target_type || target_type = Types.Void then
            return (counter', st', eval_inner, PlainOperand r)
        else
            let counter'', st'', dst_name = createTmp counter' st' target_type
            let dst = T.Var dst_name
            let! cast_instruction =
                match (target_type, inner_type) with
                | Types.Double, _ ->
                    result {
                        let! signed = TypeUtils.isSigned inner_type
                        if signed then
                            return T.IntToDouble { src = r; dst = dst }
                        else return T.UIntToDouble { src = r; dst = dst }
                    }
                | _, Types.Double ->
                    result {
                        let! signed = TypeUtils.isSigned target_type
                        if signed then
                            return T.DoubleToInt { src = r; dst = dst }
                        else return T.DoubleToUInt { src = r; dst = dst }
                    }
                | _ ->
                    result {
                        let! targetSize = TypeUtils.getSize tt target_type
                        let! innerSize = TypeUtils.getSize tt inner_type
                        if targetSize = innerSize then
                            return T.Copy { src = r; dst = dst }
                        else if targetSize < innerSize then
                            return T.Truncate { src = r; dst = dst }
                        else
                            let! signed = TypeUtils.isSigned inner_type
                            if signed then
                                return T.SignExtend { src = r; dst = dst }
                            else return T.ZeroExtend { src = r; dst = dst }
                    }
            let instructions = eval_inner @ [ cast_instruction ]
            return (counter'', st'', instructions, PlainOperand dst)
    }

and emitPointerAddition counter st tt t e1 e2 =
    result {
        let! counter', st', eval_v1, v1 = emitTackyAndConvert counter st tt e1
        let! counter'', st'', eval_v2, v2 = emitTackyAndConvert counter' st' tt e2
        let counter''', st''', dst_name = createTmp counter'' st'' t
        let dst = T.Var dst_name
        let ptr, index = if t = e1.t then (v1, v2) else (v2, v1)
        let! scale = getPtrScale tt t
        let instructions =
            eval_v1 @ eval_v2 @ [ T.AddPtr { ptr = ptr; index = index;
                                              scale = scale; dst = dst } ]
        return (counter''', st''', instructions, PlainOperand dst)
    }

and emitSubscript counter st tt t e1 e2 =
    result {
        let! counter', st', instructions, r =
            emitPointerAddition counter st tt (Types.Pointer t) e1 e2
        match r with
        | PlainOperand dst -> return (counter', st', instructions, DereferencedPointer dst)
        | _ ->
            return!
                Error
                    (CompilerError.InternalError
                        "expected result of pointer addition to be lvalue converted")
    }

and emitSubtractionFromPointer counter st tt t ptr_e idx_e =
    result {
        let! counter', st', eval_v1, ptr = emitTackyAndConvert counter st tt ptr_e
        let! counter'', st'', eval_v2, index = emitTackyAndConvert counter' st' tt idx_e
        let counter''', st''', dst_name = createTmp counter'' st'' t
        let dst = T.Var dst_name
        let counter'''', st'''', neg_name = createTmp counter''' st''' Types.Long
        let negated_index = T.Var neg_name
        let! scale = getPtrScale tt t
        return
            (counter'''', st'''',
             eval_v1
             @ eval_v2
             @ [ T.Unary { op = T.Negate; src = index; dst = negated_index }
                 T.AddPtr { ptr = ptr; index = negated_index; scale = scale;
                            dst = dst } ],
             PlainOperand dst)
    }

and emitPointerDiff counter st tt t e1 e2 =
    result {
        let! counter', st', eval_v1, v1 = emitTackyAndConvert counter st tt e1
        let! counter'', st'', eval_v2, v2 = emitTackyAndConvert counter' st' tt e2
        let counter''', st''', diff_name = createTmp counter'' st'' Types.Long
        let ptr_diff = T.Var diff_name
        let counter'''', st'''', dst_name = createTmp counter''' st''' t
        let dst = T.Var dst_name
        let! ptrScale = getPtrScale tt e1.t
        let scale =
            T.Constant(Const.ConstLong(int64 ptrScale))
        return
            (counter'''', st'''',
             eval_v1
             @ eval_v2
             @ [ T.Binary { op = T.Subtract; src1 = v1; src2 = v2; dst = ptr_diff }
                 T.Binary { op = T.Divide; src1 = ptr_diff; src2 = scale;
                            dst = dst } ],
             PlainOperand dst)
    }

and emitBinaryExpression counter st tt t op e1 e2 =
    result {
        let! counter', st', eval_v1, v1 = emitTackyAndConvert counter st tt e1
        let! counter'', st'', eval_v2, v2 = emitTackyAndConvert counter' st' tt e2
        let counter''', st''', dst_name = createTmp counter'' st'' t
        let dst = T.Var dst_name
        let! tacky_op = convertBinop op
        let instructions =
            eval_v1
            @ eval_v2
            @ [ T.Binary { op = tacky_op; src1 = v1; src2 = v2; dst = dst } ]
        return (counter''', st''', instructions, PlainOperand dst)
    }

and emitAndExpression counter st tt e1 e2 =
    result {
        let! counter', st', eval_v1, v1 = emitTackyAndConvert counter st tt e1
        let! counter'', st'', eval_v2, v2 = emitTackyAndConvert counter' st' tt e2
        let counter''', false_label = UniqueIds.makeLabel "and_false" counter''
        let counter'''', end_label = UniqueIds.makeLabel "and_end" counter'''
        let c5, st5, dst_name = createTmp counter'''' st'' Types.Int
        let dst = T.Var dst_name
        let instructions =
            eval_v1
            @ [ T.JumpIfZero(v1, false_label) ]
            @ eval_v2
            @ [ T.JumpIfZero(v2, false_label)
                T.Copy { src = T.Constant Const.intOne; dst = dst }
                T.Jump end_label
                T.Label false_label
                T.Copy { src = T.Constant Const.intZero; dst = dst }
                T.Label end_label ]
        return (c5, st5, instructions, PlainOperand dst)
    }

and emitOrExpression counter st tt e1 e2 =
    result {
        let! counter', st', eval_v1, v1 = emitTackyAndConvert counter st tt e1
        let! counter'', st'', eval_v2, v2 = emitTackyAndConvert counter' st' tt e2
        let counter''', true_label = UniqueIds.makeLabel "or_true" counter''
        let counter'''', end_label = UniqueIds.makeLabel "or_end" counter'''
        let c5, st5, dst_name = createTmp counter'''' st'' Types.Int
        let dst = T.Var dst_name
        let instructions =
            eval_v1
            @ (T.JumpIfNotZero(v1, true_label) :: eval_v2)
            @ T.JumpIfNotZero(v2, true_label)
              :: T.Copy { src = T.Constant Const.intZero; dst = dst }
              :: T.Jump end_label
              :: T.Label true_label
              :: T.Copy { src = T.Constant Const.intOne; dst = dst }
              :: [ T.Label end_label ]
        return (c5, st5, instructions, PlainOperand dst)
    }

and emitAssignment counter st tt lhs rhs =
    result {
        let! counter', st', lhs_instructions, lval = emitTackyForExp counter st tt lhs
        let! counter'', st'', rhs_instructions, rval = emitTackyAndConvert counter' st' tt rhs
        let instructions = lhs_instructions @ rhs_instructions
        match lval with
        | PlainOperand o ->
            return (counter'', st'', instructions @ [ T.Copy { src = rval; dst = o } ], lval)
        | DereferencedPointer ptr ->
            return (counter'', st'', instructions @ [ T.Store { src = rval; dst_ptr = ptr } ],
                     PlainOperand rval)
        | SubObject(baseObj, offset) ->
            return (counter'', st'', instructions @ [ T.CopyToOffset { src = rval; offset = offset;
                                                       dst = baseObj } ],
                     PlainOperand rval)
    }

and emitConditionalExpression counter st tt t condition e1 e2 =
    result {
        let! counter', st', eval_cond, c = emitTackyAndConvert counter st tt condition
        let! counter'', st'', eval_v1, v1 = emitTackyAndConvert counter' st' tt e1
        let! counter''', st''', eval_v2, v2 = emitTackyAndConvert counter'' st'' tt e2
        let counter'''', e2_label = UniqueIds.makeLabel "conditional_else" counter'''
        let c5, end_label = UniqueIds.makeLabel "conditional_end" counter''''
        let c6, st6, dst =
            if t = Types.Void then (c5, st''', dummyOperand)
            else
                let c6', st6', dst_name = createTmp c5 st''' t
                (c6', st6', T.Var dst_name)
        let common_instructions =
            eval_cond @ (T.JumpIfZero(c, e2_label) :: eval_v1)
        let remaining_instructions =
            if t = Types.Void then
                (T.Jump end_label :: T.Label e2_label :: eval_v2)
                @ [ T.Label end_label ]
            else
                T.Copy { src = v1; dst = dst }
                :: T.Jump end_label
                :: T.Label e2_label
                :: eval_v2
                @ (T.Copy { src = v2; dst = dst } :: [ T.Label end_label ])
        return (c6, st6, common_instructions @ remaining_instructions, PlainOperand dst)
    }

and emitFunCall counter st tt t f args =
    result {
        let counter', st', dst =
            if t = Types.Void then (counter, st, None)
            else
                let c', s', dst_name = createTmp counter st t
                (c', s', Some(T.Var dst_name))
        let! counter'', st'', arg_results =
            resultFold (fun (c, s, acc) arg ->
                result {
                    let! c', s', instrs, v = emitTackyAndConvert c s tt arg
                    return (c', s', acc @ [(instrs, v)])
                }) (counter', st', []) args
        let arg_instructions = List.collect fst arg_results
        let arg_vals = List.map snd arg_results
        let instructions =
            arg_instructions
            @ [ T.FunCall { f = f; args = arg_vals; dst = dst } ]
        let dst_val = Option.defaultValue dummyOperand dst
        return (counter'', st'', instructions, PlainOperand dst_val)
    }

and emitDereference counter st tt inner =
    result {
        let! counter', st', instructions, r = emitTackyAndConvert counter st tt inner
        return (counter', st', instructions, DereferencedPointer r)
    }

and emitDotOperator counter st tt t (strct: Ast.Exp) mbr =
    result {
        let! member_offset = getMemberOffset tt mbr strct.t
        let! counter', st', instructions, inner_object = emitTackyForExp counter st tt strct
        match inner_object with
        | PlainOperand(T.Var v) ->
            return (counter', st', instructions, SubObject(v, member_offset))
        | SubObject(baseObj, offset) ->
            return (counter', st', instructions, SubObject(baseObj, offset + member_offset))
        | DereferencedPointer ptr ->
            if member_offset = 0 then return (counter', st', instructions, DereferencedPointer ptr)
            else
                let counter'', st'', dst_name = createTmp counter' st' (Types.Pointer t)
                let dst = T.Var dst_name
                let index = T.Constant(Const.ConstLong(int64 member_offset))
                let add_ptr_instr =
                    T.AddPtr { ptr = ptr; index = index; scale = 1; dst = dst }
                return (counter'', st'', instructions @ [ add_ptr_instr ], DereferencedPointer dst)
        | PlainOperand(T.Constant _) ->
            return!
                Error
                    (CompilerError.InternalError
                        "found dot operator applied to constant")
    }

and emitArrowOperator counter st tt t (strct: Ast.Exp) mbr =
    result {
        let! member_offset = getMemberPointerOffset tt mbr strct.t
        let! counter', st', instructions, ptr = emitTackyAndConvert counter st tt strct
        if member_offset = 0 then return (counter', st', instructions, DereferencedPointer ptr)
        else
            let counter'', st'', dst_name = createTmp counter' st' (Types.Pointer t)
            let dst = T.Var dst_name
            let index = T.Constant(Const.ConstLong(int64 member_offset))
            let add_ptr_instr =
                T.AddPtr { ptr = ptr; index = index; scale = 1; dst = dst }
            return (counter'', st'', instructions @ [ add_ptr_instr ], DereferencedPointer dst)
    }

and emitAddrOf counter st tt t inner =
    result {
        let! counter', st', instructions, r = emitTackyForExp counter st tt inner
        match r with
        | PlainOperand o ->
            let counter'', st'', dst_name = createTmp counter' st' t
            let dst = T.Var dst_name
            return (counter'', st'', instructions @ [ T.GetAddress { src = o; dst = dst } ],
                     PlainOperand dst)
        | DereferencedPointer ptr -> return (counter', st', instructions, PlainOperand ptr)
        | SubObject(baseObj, offset) ->
            let counter'', st'', dst_name = createTmp counter' st' t
            let dst = T.Var dst_name
            let get_addr = T.GetAddress { src = T.Var baseObj; dst = dst }
            if offset = 0 then
                return (counter'', st'', instructions @ [ get_addr ], PlainOperand dst)
            else
                let index = T.Constant(Const.ConstLong(int64 offset))
                return (counter'', st'', instructions
                         @ [ get_addr
                             T.AddPtr { ptr = dst; index = index; scale = 1;
                                        dst = dst } ],
                         PlainOperand dst)
    }

and emitTackyAndConvert counter st tt e =
    result {
        let! counter', st', instructions, r = emitTackyForExp counter st tt e
        match r with
        | PlainOperand o -> return (counter', st', instructions, o)
        | DereferencedPointer ptr ->
            let counter'', st'', dst_name = createTmp counter' st' e.t
            let dst = T.Var dst_name
            return (counter'', st'', instructions @ [ T.Load { src_ptr = ptr; dst = dst } ], dst)
        | SubObject(baseObj, offset) ->
            let counter'', st'', dst_name = createTmp counter' st' e.t
            let dst = T.Var dst_name
            return (counter'', st'',
                     instructions
                     @ [ T.CopyFromOffset { src = baseObj; offset = offset; dst = dst } ],
                     dst)
    }

let rec emitStringInit dst offset (s: byte[]) =
    let len = Bytes.length s
    if len = 0 then []
    else if len >= 8 then
        let l = Bytes.getInt64Le s 0
        let instr =
            T.CopyToOffset { src = T.Constant(Const.ConstLong l);
                             dst = dst; offset = offset }
        let rest = Bytes.sub s 8 (len - 8)
        instr :: emitStringInit dst (offset + 8) rest
    else if len >= 4 then
        let i = Bytes.getInt32Le s 0
        let instr =
            T.CopyToOffset { src = T.Constant(Const.ConstInt i);
                             dst = dst; offset = offset }
        let rest = Bytes.sub s 4 (len - 4)
        instr :: emitStringInit dst (offset + 4) rest
    else
        let c = Bytes.getInt8 s 0
        let instr =
            T.CopyToOffset { src = T.Constant(Const.ConstChar c);
                             dst = dst; offset = offset }
        let rest = Bytes.sub s 1 (len - 1)
        instr :: emitStringInit dst (offset + 1) rest

let rec emitCompoundInit counter st tt name offset = function
    | Ast.Initializer.SingleInit { e = Ast.InnerExp.String s; t = Types.Array(_, size) } ->
        let str_bytes = Bytes.ofString s
        let padding_bytes = Bytes.make (int size - String.length s) (char 0)
        Ok (counter, st, emitStringInit name offset (Bytes.cat str_bytes padding_bytes))
    | Ast.Initializer.SingleInit e ->
        result {
            let! counter', st', eval_init, v = emitTackyAndConvert counter st tt e
            return
                (counter', st',
                 eval_init
                 @ [ T.CopyToOffset { src = v; dst = name; offset = offset } ])
        }
    | Ast.Initializer.CompoundInit(Types.Array(elem_type, _), inits) ->
        result {
            let! elem_size = TypeUtils.getSize tt elem_type
            let! counter', st', instrs =
                resultFold (fun (c, s, acc_instrs) (idx, elem_init) ->
                    result {
                        let new_offset =
                            offset + (idx * int elem_size)
                        let! c', s', instrs = emitCompoundInit c s tt name new_offset elem_init
                        return (c', s', acc_instrs @ instrs)
                    })
                    (counter, st, [])
                    (List.mapi (fun i init -> (i, init)) inits)
            return (counter', st', instrs)
        }
    | Ast.Initializer.CompoundInit(Types.Structure tag, inits) ->
        result {
            let! members = TypeTable.getMembers tag tt
            let! counter', st', instrs =
                resultFold2 (fun (c, s, acc_instrs) (memb: TypeTable.MemberDef) init ->
                    result {
                        let mem_offset = offset + memb.offset
                        let! c', s', instrs = emitCompoundInit c s tt name mem_offset init
                        return (c', s', acc_instrs @ instrs)
                    })
                    (counter, st, []) members inits
            return (counter', st', instrs)
        }
    | Ast.Initializer.CompoundInit(_, _) ->
        Error (CompilerError.InternalError "compound init has non-array type!")

let rec emitTackyForStatement counter st tt = function
    | Ast.Return e ->
        result {
            let! counter', st', eval_exp, v =
                match e with
                | Some expr ->
                    result {
                        let! c, s, instrs, r = emitTackyAndConvert counter st tt expr
                        return (c, s, instrs, Some r)
                    }
                | None -> Ok (counter, st, [], None)
            return (counter', st', eval_exp @ [ T.Return v ])
        }
    | Ast.Expression e ->
        result {
            let! counter', st', eval_exp, _ = emitTackyForExp counter st tt e
            return (counter', st', eval_exp)
        }
    | Ast.If(condition, then_clause, else_clause) ->
        emitTackyForIfStatement counter st tt condition then_clause else_clause
    | Ast.Compound(Ast.Block items) ->
        result {
            let! counter', st', instrs =
                resultFold (fun (c, s, acc) item ->
                    result {
                        let! c', s', instrs = emitTackyForBlockItem c s tt item
                        return (c', s', acc @ instrs)
                    }) (counter, st, []) items
            return (counter', st', instrs)
        }
    | Ast.Break id -> Ok (counter, st, [ T.Jump(breakLabel id) ])
    | Ast.Continue id -> Ok (counter, st, [ T.Jump(continueLabel id) ])
    | Ast.DoWhile(body, condition, id) ->
        emitTackyForDoLoop counter st tt body condition id
    | Ast.While(condition, body, id) ->
        emitTackyForWhileLoop counter st tt condition body id
    | Ast.For(init, condition, post, body, id) ->
        emitTackyForForLoop counter st tt init condition post body id
    | Ast.Null -> Ok (counter, st, [])

and emitTackyForBlockItem counter st tt = function
    | Ast.Stmt s -> emitTackyForStatement counter st tt s
    | Ast.Decl d -> emitLocalDeclaration counter st tt d

and emitLocalDeclaration counter st tt = function
    | Ast.VarDecl { storageClass = Some _ } -> Ok (counter, st, [])
    | Ast.VarDecl vd -> emitVarDeclaration counter st tt vd
    | Ast.FunDecl _ -> Ok (counter, st, [])
    | Ast.StructDecl _ -> Ok (counter, st, [])

and emitVarDeclaration counter st tt = function
    | { name = name; init = Some(Ast.Initializer.SingleInit({ e = Ast.InnerExp.String _; t = Types.Array _ }) as string_init) } ->
        emitCompoundInit counter st tt name 0 string_init
    | { name = name; init = Some(Ast.Initializer.SingleInit e); varType = varType } ->
        result {
            let! counter', st', eval_assignment, _assign_result =
                emitAssignment counter st tt { e = Ast.InnerExp.Var name; t = varType } e
            return (counter', st', eval_assignment)
        }
    | { name = name; init = Some compound_init } ->
        emitCompoundInit counter st tt name 0 compound_init
    | { init = None } ->
        Ok (counter, st, [])

and emitTackyForIfStatement counter st tt condition then_clause = function
    | None ->
        result {
            let counter', end_label = UniqueIds.makeLabel "if_end" counter
            let! counter'', st'', eval_condition, c = emitTackyAndConvert counter' st tt condition
            let! counter''', st''', then_instrs = emitTackyForStatement counter'' st'' tt then_clause
            return
                (counter''', st''',
                 eval_condition
                 @ (T.JumpIfZero(c, end_label)
                    :: then_instrs)
                 @ [ T.Label end_label ])
        }
    | Some else_clause ->
        result {
            let counter', else_label = UniqueIds.makeLabel "else" counter
            let counter'', end_label = UniqueIds.makeLabel "" counter'
            let! counter''', st''', eval_condition, c = emitTackyAndConvert counter'' st tt condition
            let! counter'''', st'''', then_instrs = emitTackyForStatement counter''' st''' tt then_clause
            let! c5, st5, else_instrs = emitTackyForStatement counter'''' st'''' tt else_clause
            return
                (c5, st5,
                 eval_condition
                 @ (T.JumpIfZero(c, else_label)
                    :: then_instrs)
                 @ T.Jump end_label
                   :: T.Label else_label
                   :: else_instrs
                 @ [ T.Label end_label ])
        }

and emitTackyForDoLoop counter st tt body condition id =
    result {
        let counter', start_label = UniqueIds.makeLabel "do_loop_start" counter
        let cont_label = continueLabel id
        let br_label = breakLabel id
        let! counter'', st'', body_instrs = emitTackyForStatement counter' st tt body
        let! counter''', st''', eval_condition, c = emitTackyAndConvert counter'' st'' tt condition
        return
            (counter''', st''',
             (T.Label start_label :: body_instrs)
             @ (T.Label cont_label :: eval_condition)
             @ [ T.JumpIfNotZero(c, start_label); T.Label br_label ])
    }

and emitTackyForWhileLoop counter st tt condition body id =
    result {
        let cont_label = continueLabel id
        let br_label = breakLabel id
        let! counter', st', eval_condition, c = emitTackyAndConvert counter st tt condition
        let! counter'', st'', body_instrs = emitTackyForStatement counter' st' tt body
        return
            (counter'', st'',
             (T.Label cont_label :: eval_condition)
             @ (T.JumpIfZero(c, br_label) :: body_instrs)
             @ [ T.Jump cont_label; T.Label br_label ])
    }

and emitTackyForForLoop counter st tt init condition post body id =
    result {
        let counter', start_label = UniqueIds.makeLabel "for_start" counter
        let cont_label = continueLabel id
        let br_label = breakLabel id
        let! counter'', st'', for_init_instructions =
            match init with
            | Ast.InitDecl d -> emitVarDeclaration counter' st tt d
            | Ast.InitExp e ->
                match e with
                | Some expr ->
                    result {
                        let! c, s, instrs, _ = emitTackyForExp counter' st tt expr
                        return (c, s, instrs)
                    }
                | None -> Ok (counter', st, [])
        let! counter''', st''', test_condition =
            match condition with
            | Some cond ->
                result {
                    let! c, s, instrs, v = emitTackyAndConvert counter'' st'' tt cond
                    return (c, s, instrs @ [ T.JumpIfZero(v, br_label) ])
                }
            | None -> Ok (counter'', st'', [])
        let! counter'''', st'''', body_instrs = emitTackyForStatement counter''' st''' tt body
        let! c5, st5, post_instructions =
            match post with
            | Some p ->
                result {
                    let! c, s, instrs, _ = emitTackyForExp counter'''' st'''' tt p
                    return (c, s, instrs)
                }
            | None -> Ok (counter'''', st'''', [])
        return
            (c5, st5,
             for_init_instructions
             @ (T.Label start_label :: test_condition)
             @ body_instrs
             @ (T.Label cont_label :: post_instructions)
             @ [ T.Jump start_label; T.Label br_label ])
    }

let emitFunDeclaration counter st tt = function
    | Ast.FunDecl { name = name; paramList = paramList;
                    body = Some(Ast.Block block_items) } ->
        result {
            let! isGlobal = Symbols.isGlobal name st
            let! counter', st', body_instructions =
                resultFold (fun (c, s, acc) item ->
                    result {
                        let! c', s', instrs = emitTackyForBlockItem c s tt item
                        return (c', s', acc @ instrs)
                    }) (counter, st, []) block_items
            let extra_return =
                T.Return(Some(T.Constant Const.intZero))
            return
                (counter', st',
                 Some(
                    T.Function { name = name; isGlobal = isGlobal; paramList = paramList;
                                 body = body_instructions @ [ extra_return ] }))
        }
    | _ -> Ok (counter, st, None)

let convertSymbolsToTacky st tt =
    let to_var (name, entry: Symbols.SymbolEntry) =
        match entry.attrs with
        | Symbols.StaticAttr { init = init; isGlobal = isGlobal } ->
            match init with
            | Symbols.Initial i ->
                Ok (Some(T.StaticVariable { name = name; t = entry.symType;
                                            isGlobal = isGlobal; init = i }))
            | Symbols.Tentative ->
                result {
                    let! zeroInit = Initializers.zero tt entry.symType
                    return
                        Some(
                            T.StaticVariable { name = name; t = entry.symType;
                                               isGlobal = isGlobal;
                                               init = zeroInit })
                }
            | Symbols.NoInitializer -> Ok None
        | Symbols.ConstAttr init ->
            Ok (Some(T.StaticConstant { name = name; t = entry.symType; init = init }))
        | _ -> Ok None
    resultFold (fun acc binding ->
        result {
            let! v = to_var binding
            match v with
            | Some x -> return acc @ [x]
            | None -> return acc
        }) [] (Symbols.bindings st)

let gen counter st tt (Ast.Program decls) =
    result {
        let! counter', st', tacky_fn_defs =
            resultFold (fun (c, s, acc) decl ->
                result {
                    let! c', s', r = emitFunDeclaration c s tt decl
                    match r with
                    | Some fn -> return (c', s', acc @ [fn])
                    | None -> return (c', s', acc)
                }) (counter, st, []) decls
        let! tacky_var_defs = convertSymbolsToTacky st' tt
        return (counter', st', Tacky.Program(tacky_var_defs @ tacky_fn_defs))
    }
