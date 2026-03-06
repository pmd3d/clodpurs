module Codegen

open TypeUtils
open Const
open ResultCE

let intParamPassingRegs = [ Assembly.DI; Assembly.SI; Assembly.DX; Assembly.CX; Assembly.R8; Assembly.R9 ]

let dblParamPassingRegs =
  [ Assembly.XMM0; Assembly.XMM1; Assembly.XMM2; Assembly.XMM3; Assembly.XMM4; Assembly.XMM5; Assembly.XMM6; Assembly.XMM7 ]

let zero = Assembly.Imm 0L

type private CodegenState = {
    counter: UniqueIds.Counter
    constants: Map<int64, string * int>
    classifiedStructures: Map<string, ParamClass list>
    typeTable: TypeTable.TypeTableMap
    symbols: Symbols.SymbolTableMap
    asmSymbols: AssemblySymbols.AsmSymbolTableMap
}
and ParamClass = Mem | SSE | Integer

let private nextLabel prefix state =
    let c, name = UniqueIds.makeLabel prefix state.counter
    ({ state with counter = c }, name)

let private addConstant alignmentOpt dbl state =
  let alignment = defaultArg alignmentOpt 8
  let key = System.BitConverter.DoubleToInt64Bits dbl
  match Map.tryFind key state.constants with
  | Some (name, old_alignment) ->
    let state' = { state with constants = Map.add key (name, max alignment old_alignment) state.constants }
    (state', name)
  | None ->
    let state', name = nextLabel "dbl" state
    let state'' = { state' with constants = Map.add key (name, alignment) state'.constants }
    (state'', name)

(* Get the operand type we should use to move an eightbyte of a struct. *)
let getEightbyteType eightbyte_idx total_var_size =
  let bytes_left = total_var_size - (eightbyte_idx * 8)
  match bytes_left with
  | x when x >= 8 -> Assembly.Quadword
  | 4 -> Assembly.Longword
  | 1 -> Assembly.Byte
  | x -> Assembly.ByteArray { size = x; alignment = 8 }

let addOffset n = function
  | Assembly.PseudoMem (base_, off) -> Ok (Assembly.PseudoMem (base_, off + n))
  | Assembly.Memory (r, off) -> Ok (Assembly.Memory (r, off + n))
  | Assembly.Imm _ | Assembly.Reg _ | Assembly.Pseudo _ | Assembly.Indexed _ | Assembly.Data _ ->
      Error (CompilerError.InternalError
        "Internal error: trying to copy data to or from non-memory operand")

let rec copyBytes src_val dst_val byte_count =
  if byte_count = 0 then Ok []
  else
    result {
      let operand_type, operand_size =
        if byte_count < 4 then (Assembly.Byte, 1)
        else if byte_count < 8 then (Assembly.Longword, 4)
        else (Assembly.Quadword, 8)

      let! next_src = addOffset operand_size src_val
      let! next_dst = addOffset operand_size dst_val
      let bytes_left = byte_count - operand_size
      let! rest = copyBytes next_src next_dst bytes_left
      return Assembly.Mov (operand_type, src_val, dst_val) :: rest
    }

let copyBytesToReg src_val dst_reg byte_count =
  let copy_byte i =
    result {
      let! offsetSrc = addOffset i src_val
      let mv = Assembly.Mov (Assembly.Byte, offsetSrc, Assembly.Reg dst_reg)
      if i = 0 then return [ mv ]
      else
        return
          [ mv; Assembly.Binary { op = Assembly.Shl; t = Assembly.Quadword; src = Assembly.Imm 8L; dst = Assembly.Reg dst_reg } ]
    }

  let byte_counts = List.init byte_count id |> List.rev
  resultTraverse copy_byte byte_counts |> Result.map List.concat

let copyBytesFromReg src_reg dst_val byte_count =
  let copy_byte i =
    result {
      let! offsetDst = addOffset i dst_val
      let mv = Assembly.Mov (Assembly.Byte, Assembly.Reg src_reg, offsetDst)
      if i < byte_count - 1 then
          return
            [
              mv;
              Assembly.Binary
                { op = Assembly.ShrBinop; t = Assembly.Quadword; src = Assembly.Imm 8L; dst = Assembly.Reg src_reg };
            ]
      else return [ mv ]
    }

  resultTraverse copy_byte (List.init byte_count id) |> Result.map List.concat

let private convertVal state = function
  | Tacky.Constant (Const.ConstChar c) -> Ok (state, Assembly.Imm (int64 c))
  | Tacky.Constant (Const.ConstUChar uc) -> Ok (state, Assembly.Imm (int64 uc))
  | Tacky.Constant (Const.ConstInt i) -> Ok (state, Assembly.Imm (int64 i))
  | Tacky.Constant (Const.ConstLong l) -> Ok (state, Assembly.Imm l)
  | Tacky.Constant (Const.ConstUInt u) -> Ok (state, Assembly.Imm (int64 u))
  | Tacky.Constant (Const.ConstULong ul) -> Ok (state, Assembly.Imm (int64 ul))
  | Tacky.Constant (Const.ConstDouble d) ->
      let state', name = addConstant None d state
      Ok (state', Assembly.Data (name, 0))
  | Tacky.Var v ->
      result {
        let! entry = Symbols.get v state.symbols
        if TypeUtils.isScalar entry.symType then return (state, Assembly.Pseudo v)
        else return (state, Assembly.PseudoMem (v, 0))
      }

let convertType tt = function
  | Types.Int | Types.UInt -> Ok Assembly.Longword
  | Types.Long | Types.ULong | Types.Pointer _ -> Ok Assembly.Quadword
  | Types.Char | Types.SChar | Types.UChar -> Ok Assembly.Byte
  | Types.Double -> Ok Assembly.Double
  | (Types.Array _ | Types.Structure _) as t ->
      result {
        let! sz = TypeUtils.getSize tt t
        let! align = TypeUtils.getAlignment tt t
        return Assembly.ByteArray { size = int sz; alignment = align }
      }
  | (Types.FunType _ | Types.Void) as t ->
      Error (CompilerError.InternalError
        ("Internal error, converting type to assembly: " + Types.show t))

let asmType tt st v =
  result {
    let! t = Tacky.typeOfVal st v
    return! convertType tt t
  }

let convertUnop = function
  | Tacky.Complement -> Ok Assembly.Not
  | Tacky.Negate -> Ok Assembly.Neg
  | Tacky.Not ->
      Error (CompilerError.InternalError "Internal error, can't convert TACKY not directly to assembly")

let convertBinop = function
  | Tacky.Add -> Ok Assembly.Add
  | Tacky.Subtract -> Ok Assembly.Sub
  | Tacky.Multiply -> Ok Assembly.Mult
  | Tacky.Divide ->
      Ok Assembly.DivDouble
  | Tacky.Mod | Tacky.Equal | Tacky.NotEqual | Tacky.GreaterOrEqual | Tacky.LessOrEqual | Tacky.GreaterThan
  | Tacky.LessThan ->
      Error (CompilerError.InternalError "Internal error: not a binary assembly instruction")

let convertCondCode signed = function
  | Tacky.Equal -> Ok Assembly.E
  | Tacky.NotEqual -> Ok Assembly.NE
  | Tacky.GreaterThan -> Ok (if signed then Assembly.G else Assembly.A)
  | Tacky.GreaterOrEqual -> Ok (if signed then Assembly.GE else Assembly.AE)
  | Tacky.LessThan -> Ok (if signed then Assembly.L else Assembly.B)
  | Tacky.LessOrEqual -> Ok (if signed then Assembly.LE else Assembly.BE)
  | _ -> Error (CompilerError.InternalError "Internal error: not a condition code")

let private classifyNewStructure tt tag =
  result {
    let! structDef = TypeTable.find tag tt
    let size = structDef.size
    if size > 16 then
      let eightbyte_count = (size / 8) + if size % 8 = 0 then 0 else 1
      return ListUtil.makeList eightbyte_count Mem
    else
      let rec f = function
        | Types.Structure struct_tag ->
            result {
              let! member_types = TypeTable.getMemberTypes struct_tag tt
              let! results = resultTraverse f member_types
              return List.concat results
            }
        | Types.Array (elemType, arrSize) ->
            result {
              let! elemResult = f elemType
              return List.concat (ListUtil.makeList (int arrSize) elemResult)
            }
        | t -> Ok [ t ]

      let! scalar_types = f (Types.Structure tag)
      match scalar_types with
      | [] -> return! Error (CompilerError.InternalError "Internal error: empty scalar types for structure")
      | first :: _ ->
          match ListUtil.tryLast scalar_types with
          | None -> return! Error (CompilerError.InternalError "Internal error: empty scalar types for structure")
          | Some last ->
            if size > 8 then
              let first_class = if first = Types.Double then SSE else Integer
              let last_class = if last = Types.Double then SSE else Integer
              return [ first_class; last_class ]
            else if first = Types.Double then return [ SSE ]
            else return [ Integer ]
  }

let private classifyStructure tag state =
  match Map.tryFind tag state.classifiedStructures with
  | Some classes -> Ok (state, classes)
  | None ->
      result {
        let! classes = classifyNewStructure state.typeTable tag
        let state' = { state with classifiedStructures = Map.add tag classes state.classifiedStructures }
        return (state', classes)
      }

let private classifyParamsHelper state typed_asm_vals return_on_stack =
  let int_regs_available = if return_on_stack then 5 else 6
  let process_one_param (state, int_reg_args, dbl_reg_args, stack_args)
      (tacky_t, operand) =
    result {
      let! t = convertType state.typeTable tacky_t
      let typed_operand = (t, operand)
      match tacky_t with
      | Types.Structure s ->
          let! var_name =
            match operand with
            | Assembly.PseudoMem (n, 0) -> Ok n
            | _ -> Error (CompilerError.InternalError "Bad structure operand")

          let! sz = TypeUtils.getSize state.typeTable tacky_t
          let var_size = int sz
          let! state', classes = classifyStructure s state
          let! updated_int, updated_dbl, use_stack =
            match classes with
            | [] -> Error (CompilerError.InternalError "Internal error: empty classification for structure")
            | first :: _ when first = Mem ->
              Ok (int_reg_args, dbl_reg_args, true)
            | _ ->
              let process_one_eightbyte (i, tentative_ints, tentative_dbls) cls =
                let operand = Assembly.PseudoMem (var_name, i * 8)
                match cls with
                | SSE -> Ok (i + 1, tentative_ints, operand :: tentative_dbls)
                | Integer ->
                    let eightbyte_type =
                      getEightbyteType i var_size
                    in
                    Ok ( i + 1,
                      (eightbyte_type, operand) :: tentative_ints,
                      tentative_dbls )
                | Mem ->
                    Error (CompilerError.InternalError
                      "Internal error: found eightbyte in Mem class, but first \
                       eighbyte wasn't Mem")

              result {
                let! _, tentative_ints, tentative_dbs =
                  resultFold (fun (i, tInts, tDbls) cls ->
                    process_one_eightbyte (i, tInts, tDbls) cls)
                    (0, int_reg_args, dbl_reg_args)
                    classes
                if
                  List.length tentative_ints <= int_regs_available
                  && List.length tentative_dbs <= 8
                then
                  return (tentative_ints, tentative_dbs, false)
                else return (int_reg_args, dbl_reg_args, true)
              }

          let add_stack_args stk i =
            let eightbyte_type =
              getEightbyteType i var_size
            in
            (eightbyte_type, Assembly.PseudoMem (var_name, i * 8)) :: stk

          let updated_stack_args =
            if use_stack then
              List.fold add_stack_args stack_args
                (List.mapi (fun idx _ -> idx) classes)
            else stack_args

          return (state', updated_int, updated_dbl, updated_stack_args)
      | Types.Double ->
          if List.length dbl_reg_args < 8 then
            return (state, int_reg_args, operand :: dbl_reg_args, stack_args)
          else return (state, int_reg_args, dbl_reg_args, typed_operand :: stack_args)
      | _ ->
          if List.length int_reg_args < int_regs_available then
            return (state, typed_operand :: int_reg_args, dbl_reg_args, stack_args)
          else return (state, int_reg_args, dbl_reg_args, typed_operand :: stack_args)
    }

  result {
    let! state', reversed_int, reversed_dbl, reversed_stack =
      resultFold (fun (s, ints, dbls, stk) tv ->
        process_one_param (s, ints, dbls, stk) tv) (state, [], [], []) typed_asm_vals
    return (state', List.rev reversed_int, List.rev reversed_dbl, List.rev reversed_stack)
  }

let private classifyParameters state params_ return_on_stack =
  let f state v =
    result {
      let! state', asm_v = convertVal state v
      let! t = Tacky.typeOfVal state.symbols v
      return (state', (t, asm_v))
    }
  result {
    let! state', typed_vals =
      resultFold (fun (s, acc) v ->
        result {
          let! s', tv = f s v
          return (s', acc @ [tv])
        }) (state, []) params_
    return! classifyParamsHelper state' typed_vals return_on_stack
  }

let private classifyParamTypes state type_list return_on_stack =
  result {
    let f t =
      if TypeUtils.isScalar t then (t, Assembly.Pseudo "dummy")
      else (t, Assembly.PseudoMem ("dummy", 0))

    let! state', ints, dbls, _ =
      classifyParamsHelper state (List.map f type_list) return_on_stack

    let int_regs = ListUtil.take (List.length ints) intParamPassingRegs
    let dbl_regs = ListUtil.take (List.length dbls) dblParamPassingRegs
    return (state', int_regs @ dbl_regs)
  }

let private classifyReturnHelper state ret_type asm_retval =
  match ret_type with
  | Types.Structure tag ->
      result {
        let! state', classes = classifyStructure tag state
        let! var_name =
          match asm_retval with
          | Assembly.PseudoMem (n, 0) -> Ok n
          | _ ->
              Error (CompilerError.InternalError
                "Internal error: invalid assembly operand for structure type")

        match classes with
        | [] -> return! Error (CompilerError.InternalError "Internal error: empty classification for structure")
        | first :: _ when first = Mem -> return (state', [], [], true)
        | _ ->
          let! _, i, d =
            resultFold (fun (i, ints, dbls) cls ->
              let operand = Assembly.PseudoMem (var_name, i * 8)
              match cls with
              | SSE -> Ok (i + 1, ints, dbls @ [ operand ])
              | Integer ->
                  result {
                    let! sz = TypeUtils.getSize state.typeTable ret_type
                    let eightbyte_type = getEightbyteType i (int sz)
                    return (i + 1, ints @ [ (eightbyte_type, operand) ], dbls)
                  }
              | Mem ->
                  Error (CompilerError.InternalError
                    "Internal error: found eightbyte in Mem class, but first \
                     eighbyte wasn't Mem")
            ) (0, [], []) classes
          return (state', i, d, false)
      }
  | Types.Double -> Ok (state, [], [ asm_retval ], false)
  | t ->
      result {
        let! ct = convertType state.typeTable t
        let typed_operand = (ct, asm_retval)
        return (state, [ typed_operand ], [], false)
      }

let private classifyReturnValue state retval =
  result {
    let! state', asm_v = convertVal state retval
    let! retType = Tacky.typeOfVal state.symbols retval
    return! classifyReturnHelper state' retType asm_v
  }

let private classifyReturnType state = function
  | Types.Void -> Ok (state, [], false)
  | t ->
      result {
        let asm_val =
          if TypeUtils.isScalar t then Assembly.Pseudo "dummy"
          else Assembly.PseudoMem ("dummy", 0)

        let! state', ints, dbls, return_on_stack = classifyReturnHelper state t asm_val
        if return_on_stack then return (state', [ Assembly.AX ], true)
        else
          let int_regs = ListUtil.take (List.length ints) [ Assembly.AX; Assembly.DX ]
          let dbl_regs =
            ListUtil.take (List.length dbls) [ Assembly.XMM0; Assembly.XMM1 ]
          return (state', int_regs @ dbl_regs, false)
      }

let private convertFunctionCall state f args dst =
  result {
    let! state', int_retvals, dbl_retvals, return_on_stack =
      match dst with
      | Some d -> classifyReturnValue state d
      | None -> Ok (state, [], [], false)

    let! load_dst_instruction, first_intreg_idx =
      if return_on_stack then
        match dst with
        | Some d ->
          result {
            let! _, asm_d2 = convertVal state' d
            return ([ Assembly.Lea (asm_d2, Assembly.Reg Assembly.DI) ], 1)
          }
        | None -> Error (CompilerError.InternalError "Internal error: return_on_stack but no dst")
      else Ok ([], 0)

    let! state'', int_reg_args, dbl_reg_args, stack_args =
      classifyParameters state' args return_on_stack

    let stack_padding = if List.length stack_args % 2 = 0 then 0 else 8
    let alignment_instruction =
      if stack_padding = 0 then []
      else
        [
          Assembly.Binary
            {
              op = Assembly.Sub;
              t = Assembly.Quadword;
              src = Assembly.Imm (int64 stack_padding);
              dst = Assembly.Reg Assembly.SP;
            };
        ]

    let instructions = load_dst_instruction @ alignment_instruction
    let! int_reg_instructions =
      resultTraverse (fun (idx, (arg_t, arg)) ->
        result {
          let! r =
              match List.tryItem (idx + first_intreg_idx) intParamPassingRegs with
              | Some r -> Ok r
              | None -> Error (CompilerError.InternalError "Internal error: int register index out of bounds")
          match arg_t with
          | Assembly.ByteArray { size = size } ->
              return! copyBytesToReg arg r size
          | _ -> return [ Assembly.Mov (arg_t, arg, Assembly.Reg r) ]
        }
      ) (List.mapi (fun i x -> (i, x)) int_reg_args)
    let instructions =
      instructions @ List.concat int_reg_instructions

    let! dbl_reg_instructions =
      resultTraverse (fun (idx, arg) ->
        result {
          let! r =
              match List.tryItem idx dblParamPassingRegs with
              | Some r -> Ok r
              | None -> Error (CompilerError.InternalError "Internal error: dbl register index out of bounds")
          return Assembly.Mov (Assembly.Double, arg, Assembly.Reg r)
        }
      ) (List.mapi (fun i x -> (i, x)) dbl_reg_args)

    let instructions = instructions @ dbl_reg_instructions

    let! stack_instructions =
      resultTraverse (fun (arg_t, arg) ->
        match (arg, arg_t) with
        | (Assembly.Imm _ | Assembly.Reg _), _ | _, (Assembly.Quadword | Assembly.Double) ->
            Ok [ Assembly.Push arg ]
        | _, Assembly.ByteArray { size = size } ->
            result {
              let! copyInstrs = copyBytes arg (Assembly.Memory (Assembly.SP, 0)) size
              return
                Assembly.Binary
                  { op = Assembly.Sub; t = Assembly.Quadword; src = Assembly.Imm (int64 8); dst = Assembly.Reg Assembly.SP }
                :: copyInstrs
            }
        | _ ->
            Ok [ Assembly.Mov (arg_t, arg, Assembly.Reg Assembly.AX); Assembly.Push (Assembly.Reg Assembly.AX) ]
      ) stack_args
    let instructions =
      instructions @ List.concat (stack_instructions |> List.rev)

    let instructions = instructions @ [ Assembly.Call f ]

    let bytes_to_remove = (8 * List.length stack_args) + stack_padding
    let dealloc =
      if bytes_to_remove = 0 then []
      else
        [
          Assembly.Binary
            {
              op = Assembly.Add;
              t = Assembly.Quadword;
              src = Assembly.Imm (int64 bytes_to_remove);
              dst = Assembly.Reg Assembly.SP;
            };
        ]

    let instructions = instructions @ dealloc

    let int_ret_regs = [ Assembly.AX; Assembly.DX ]
    let dbl_ret_regs = [ Assembly.XMM0; Assembly.XMM1 ]
    let! retrieve_result =
      match (dst, return_on_stack) with
      | Some _, false ->
          result {
            let! int_results =
              resultTraverse (fun (i, (t, op)) ->
                result {
                  let! r =
                      match List.tryItem i int_ret_regs with
                      | Some r -> Ok r
                      | None -> Error (CompilerError.InternalError "Internal error: int return register index out of bounds")
                  match t with
                  | Assembly.ByteArray { size = size } ->
                      return! copyBytesFromReg r op size
                  | _ -> return [ Assembly.Mov (t, Assembly.Reg r, op) ]
                }
              ) (List.mapi (fun i x -> (i, x)) int_retvals)
            let! dbl_results =
              resultTraverse (fun (i, op) ->
                result {
                  let! r =
                      match List.tryItem i dbl_ret_regs with
                      | Some r -> Ok r
                      | None -> Error (CompilerError.InternalError "Internal error: dbl return register index out of bounds")
                  return Assembly.Mov (Assembly.Double, Assembly.Reg r, op)
                }
              ) (List.mapi (fun i x -> (i, x)) dbl_retvals)

            return List.concat int_results @ dbl_results
          }
      | _ -> Ok []

    return (state'', instructions @ retrieve_result)
  }

let private convertReturnInstruction state = function
  | None -> Ok (state, [ Assembly.Ret ])
  | Some v ->
      result {
        let! state', int_retvals, dbl_retvals, return_on_stack = classifyReturnValue state v
        if return_on_stack then
          let! retType = Tacky.typeOfVal state.symbols v
          let! sz = TypeUtils.getSize state.typeTable retType
          let byte_count = int sz
          let get_ptr = Assembly.Mov (Assembly.Quadword, Assembly.Memory (Assembly.BP, -8), Assembly.Reg Assembly.AX)
          let! state'', asm_v = convertVal state' v
          let! copy_into_ptr =
            copyBytes asm_v (Assembly.Memory (Assembly.AX, 0)) byte_count
          return (state'', (get_ptr :: copy_into_ptr) @ [ Assembly.Ret ])
        else
          let state'' = state'
          let! return_ints =
            resultTraverse (fun (i, (t, op)) ->
              result {
                let! dst_reg =
                    match i with
                    | 0 -> Ok Assembly.AX
                    | 1 -> Ok Assembly.DX
                    | _ -> Error (CompilerError.InternalError "Internal error: int return register index out of bounds")
                match t with
                | Assembly.ByteArray { size = size } ->
                    return! copyBytesToReg op dst_reg size
                | _ -> return [ Assembly.Mov (t, op, Assembly.Reg dst_reg) ]
              }
            ) (List.mapi (fun i x -> (i, x)) int_retvals)
          let! return_dbls =
            resultTraverse
              (fun (i, op) ->
                  result {
                    let! r =
                      match i with
                      | 0 -> Ok Assembly.XMM0
                      | 1 -> Ok Assembly.XMM1
                      | _ -> Error (CompilerError.InternalError "Internal error: dbl return register index out of bounds")
                    return Assembly.Mov (Assembly.Double, op, Assembly.Reg r)
                  })
              (List.mapi (fun i x -> (i, x)) dbl_retvals)

          return (state'', List.concat return_ints @ return_dbls @ [ Assembly.Ret ])
      }

let private convertInstruction state = function
  | Tacky.Copy { src = src; dst = dst } ->
      result {
        let! srcType = Tacky.typeOfVal state.symbols src
        if TypeUtils.isScalar srcType then
          let! t = asmType state.typeTable state.symbols src
          let! state', asm_src = convertVal state src
          let! state'', asm_dst = convertVal state' dst
          return (state'', [ Assembly.Mov (t, asm_src, asm_dst) ])
        else
          let! state', asm_src = convertVal state src
          let! state'', asm_dst = convertVal state' dst
          let! sz = TypeUtils.getSize state.typeTable srcType
          let byte_count = int sz
          let! instrs = copyBytes asm_src asm_dst byte_count
          return (state'', instrs)
      }
  | Tacky.Return maybe_val -> convertReturnInstruction state maybe_val
  | Tacky.Unary { op = Tacky.Not; src = src; dst = dst } ->
      result {
        let! src_t = asmType state.typeTable state.symbols src
        let! dst_t = asmType state.typeTable state.symbols dst
        let! state', asm_src = convertVal state src
        let! state'', asm_dst = convertVal state' dst
        if src_t = Assembly.Double then
          return (state'',
          [
            Assembly.Binary
              { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Reg Assembly.XMM0; dst = Assembly.Reg Assembly.XMM0 };
            Assembly.Cmp (src_t, asm_src, Assembly.Reg Assembly.XMM0);
            Assembly.Mov (dst_t, zero, asm_dst);
            Assembly.SetCC (Assembly.E, asm_dst);
          ])
        else
          return (state'',
          [
            Assembly.Cmp (src_t, zero, asm_src);
            Assembly.Mov (dst_t, zero, asm_dst);
            Assembly.SetCC (Assembly.E, asm_dst);
          ])
      }
  | Tacky.Unary { op = Tacky.Negate; src = src; dst = dst } ->
      result {
        let! srcType = Tacky.typeOfVal state.symbols src
        if srcType = Types.Double then
          let! state', asm_src = convertVal state src
          let! state'', asm_dst = convertVal state' dst
          let state''', negative_zero = addConstant (Some 16) (-0.0) state''
          return (state''',
          [
            Assembly.Mov (Assembly.Double, asm_src, asm_dst);
            Assembly.Binary
              { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Data (negative_zero, 0); dst = asm_dst };
          ])
        else
          let! t = asmType state.typeTable state.symbols src
          let! asm_op = convertUnop Tacky.Negate
          let! state', asm_src = convertVal state src
          let! state'', asm_dst = convertVal state' dst
          return (state'', [ Assembly.Mov (t, asm_src, asm_dst); Assembly.Unary (asm_op, t, asm_dst) ])
      }
  | Tacky.Unary { op = op; src = src; dst = dst } ->
      result {
        let! t = asmType state.typeTable state.symbols src
        let! asm_op = convertUnop op
        let! state', asm_src = convertVal state src
        let! state'', asm_dst = convertVal state' dst
        return (state'', [ Assembly.Mov (t, asm_src, asm_dst); Assembly.Unary (asm_op, t, asm_dst) ])
      }
  | Tacky.Binary { op = op; src1 = src1; src2 = src2; dst = dst } ->
      result {
        let! src_t = asmType state.typeTable state.symbols src1
        let! dst_t = asmType state.typeTable state.symbols dst
        let! state', asm_src1 = convertVal state src1
        let! state'', asm_src2 = convertVal state' src2
        let! state''', asm_dst = convertVal state'' dst
        match op with
        | Tacky.Equal | Tacky.NotEqual | Tacky.GreaterThan | Tacky.GreaterOrEqual | Tacky.LessThan | Tacky.LessOrEqual
          ->
            let! signed =
              if src_t = Assembly.Double then Ok false
              else
                result {
                  let! t = Tacky.typeOfVal state.symbols src1
                  return! TypeUtils.isSigned t
                }
            let! cond_code = convertCondCode signed op
            return (state''',
            [
              Assembly.Cmp (src_t, asm_src2, asm_src1);
              Assembly.Mov (dst_t, zero, asm_dst);
              Assembly.SetCC (cond_code, asm_dst);
            ])
        | (Tacky.Divide | Tacky.Mod) when src_t <> Assembly.Double ->
            let result_reg = if op = Tacky.Divide then Assembly.AX else Assembly.DX
            let! src1Type = Tacky.typeOfVal state.symbols src1
            let! signed = TypeUtils.isSigned src1Type
            if signed then
              return (state''',
              [
                Assembly.Mov (src_t, asm_src1, Assembly.Reg Assembly.AX);
                Assembly.Cdq src_t;
                Assembly.Idiv (src_t, asm_src2);
                Assembly.Mov (src_t, Assembly.Reg result_reg, asm_dst);
              ])
            else
              return (state''',
              [
                Assembly.Mov (src_t, asm_src1, Assembly.Reg Assembly.AX);
                Assembly.Mov (src_t, zero, Assembly.Reg Assembly.DX);
                Assembly.Div (src_t, asm_src2);
                Assembly.Mov (src_t, Assembly.Reg result_reg, asm_dst);
              ])
        | _ ->
            let! asm_op = convertBinop op
            return (state''',
            [
              Assembly.Mov (src_t, asm_src1, asm_dst);
              Assembly.Binary { op = asm_op; t = src_t; src = asm_src2; dst = asm_dst };
            ])
      }
  | Tacky.Load loadInfo ->
      result {
        let! dstType = Tacky.typeOfVal state.symbols loadInfo.dst
        if TypeUtils.isScalar dstType then
          let! state', asm_src_ptr = convertVal state loadInfo.src_ptr
          let! state'', asm_dst = convertVal state' loadInfo.dst
          let! t = asmType state.typeTable state.symbols loadInfo.dst
          return (state'', [ Assembly.Mov (Assembly.Quadword, asm_src_ptr, Assembly.Reg Assembly.R9); Assembly.Mov (t, Assembly.Memory (Assembly.R9, 0), asm_dst) ])
        else
          let! state', asm_src_ptr = convertVal state loadInfo.src_ptr
          let! state'', asm_dst = convertVal state' loadInfo.dst
          let! sz = TypeUtils.getSize state.typeTable dstType
          let byte_count = int sz
          let! instrs = copyBytes (Assembly.Memory (Assembly.R9, 0)) asm_dst byte_count
          return (state'',
          Assembly.Mov (Assembly.Quadword, asm_src_ptr, Assembly.Reg Assembly.R9)
          :: instrs)
      }
  | Tacky.Store storeInfo ->
      result {
        let! srcType = Tacky.typeOfVal state.symbols storeInfo.src
        if TypeUtils.isScalar srcType then
          let! state', asm_src = convertVal state storeInfo.src
          let! t = asmType state.typeTable state.symbols storeInfo.src
          let! state'', asm_dst_ptr = convertVal state' storeInfo.dst_ptr
          return (state'', [ Assembly.Mov (Assembly.Quadword, asm_dst_ptr, Assembly.Reg Assembly.R9); Assembly.Mov (t, asm_src, Assembly.Memory (Assembly.R9, 0)) ])
        else
          let! state', asm_src = convertVal state storeInfo.src
          let! state'', asm_dst_ptr = convertVal state' storeInfo.dst_ptr
          let! sz = TypeUtils.getSize state.typeTable srcType
          let byte_count = int sz
          let! instrs = copyBytes asm_src (Assembly.Memory (Assembly.R9, 0)) byte_count
          return (state'',
          Assembly.Mov (Assembly.Quadword, asm_dst_ptr, Assembly.Reg Assembly.R9)
          :: instrs)
      }
  | Tacky.GetAddress { src = src; dst = dst } ->
      result {
        let! state', asm_src = convertVal state src
        let! state'', asm_dst = convertVal state' dst
        return (state'', [ Assembly.Lea (asm_src, asm_dst) ])
      }
  | Tacky.Jump target -> Ok (state, [ Assembly.Jmp target ])
  | Tacky.JumpIfZero (cond, target) ->
      result {
        let! t = asmType state.typeTable state.symbols cond
        let! state', asm_cond = convertVal state cond
        if t = Assembly.Double then
          return (state',
          [
            Assembly.Binary
              { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Reg Assembly.XMM0; dst = Assembly.Reg Assembly.XMM0 };
            Assembly.Cmp (t, asm_cond, Assembly.Reg Assembly.XMM0);
            Assembly.JmpCC (Assembly.E, target);
          ])
        else return (state', [ Assembly.Cmp (t, zero, asm_cond); Assembly.JmpCC (Assembly.E, target) ])
      }
  | Tacky.JumpIfNotZero (cond, target) ->
      result {
        let! t = asmType state.typeTable state.symbols cond
        let! state', asm_cond = convertVal state cond
        if t = Assembly.Double then
          return (state',
          [
            Assembly.Binary
              { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Reg Assembly.XMM0; dst = Assembly.Reg Assembly.XMM0 };
            Assembly.Cmp (t, asm_cond, Assembly.Reg Assembly.XMM0);
            Assembly.JmpCC (Assembly.NE, target);
          ])
        else return (state', [ Assembly.Cmp (t, zero, asm_cond); Assembly.JmpCC (Assembly.NE, target) ])
      }
  | Tacky.Label l -> Ok (state, [ Assembly.Label l ])
  | Tacky.FunCall { f = f; args = args; dst = dst } -> convertFunctionCall state f args dst
  | Tacky.SignExtend { src = src; dst = dst } ->
      result {
        let! state', asm_src = convertVal state src
        let! state'', asm_dst = convertVal state' dst
        let! srcT = asmType state.typeTable state.symbols src
        let! dstT = asmType state.typeTable state.symbols dst
        return (state'',
        [
          Assembly.Movsx
            {
              src_type = srcT;
              dst_type = dstT;
              src = asm_src;
              dst = asm_dst;
            };
        ])
      }
  | Tacky.Truncate { src = src; dst = dst } ->
      result {
        let! state', asm_src = convertVal state src
        let! state'', asm_dst = convertVal state' dst
        let! dstT = asmType state.typeTable state.symbols dst
        return (state'', [ Assembly.Mov (dstT, asm_src, asm_dst) ])
      }
  | Tacky.ZeroExtend { src = src; dst = dst } ->
      result {
        let! state', asm_src = convertVal state src
        let! state'', asm_dst = convertVal state' dst
        let! srcT = asmType state.typeTable state.symbols src
        let! dstT = asmType state.typeTable state.symbols dst
        return (state'',
        [
          Assembly.MovZeroExtend
            {
              src_type = srcT;
              dst_type = dstT;
              src = asm_src;
              dst = asm_dst;
            };
        ])
      }
  | Tacky.IntToDouble { src = src; dst = dst } ->
      result {
        let! state', asm_src = convertVal state src
        let! state'', asm_dst = convertVal state' dst
        let! t = asmType state.typeTable state.symbols src
        if t = Assembly.Byte then
          return (state'',
          [
            Assembly.Movsx
              {
                src_type = Assembly.Byte;
                dst_type = Assembly.Longword;
                src = asm_src;
                dst = Assembly.Reg Assembly.R9;
              };
            Assembly.Cvtsi2sd (Assembly.Longword, Assembly.Reg Assembly.R9, asm_dst);
          ])
        else return (state'', [ Assembly.Cvtsi2sd (t, asm_src, asm_dst) ])
      }
  | Tacky.DoubleToInt { src = src; dst = dst } ->
      result {
        let! state', asm_src = convertVal state src
        let! state'', asm_dst = convertVal state' dst
        let! t = asmType state.typeTable state.symbols dst
        if t = Assembly.Byte then
          return (state'', [ Assembly.Cvttsd2si (Assembly.Longword, asm_src, Assembly.Reg Assembly.R9); Assembly.Mov (Assembly.Byte, Assembly.Reg Assembly.R9, asm_dst) ])
        else return (state'', [ Assembly.Cvttsd2si (t, asm_src, asm_dst) ])
      }
  | Tacky.UIntToDouble { src = src; dst = dst } ->
      result {
        let! state', asm_src = convertVal state src
        let! state'', asm_dst = convertVal state' dst
        let! srcType = Tacky.typeOfVal state.symbols src
        if srcType = Types.UChar then
          return (state'',
          [
            Assembly.MovZeroExtend
              {
                src_type = Assembly.Byte;
                dst_type = Assembly.Longword;
                src = asm_src;
                dst = Assembly.Reg Assembly.R9;
              };
            Assembly.Cvtsi2sd (Assembly.Longword, Assembly.Reg Assembly.R9, asm_dst);
          ])
        else if srcType = Types.UInt then
          return (state'',
          [
            Assembly.MovZeroExtend
              {
                src_type = Assembly.Longword;
                dst_type = Assembly.Quadword;
                src = asm_src;
                dst = Assembly.Reg Assembly.R9;
              };
            Assembly.Cvtsi2sd (Assembly.Quadword, Assembly.Reg Assembly.R9, asm_dst);
          ])
        else
          let state''', out_of_bounds = nextLabel "ulong2dbl.oob" state''
          let state'''', end_lbl = nextLabel "ulong2dbl.end" state'''
          let r1, r2 = (Assembly.Reg Assembly.R8, Assembly.Reg Assembly.R9)
          return (state'''',
          [
            Assembly.Cmp (Assembly.Quadword, zero, asm_src);
            Assembly.JmpCC (Assembly.L, out_of_bounds);
            Assembly.Cvtsi2sd (Assembly.Quadword, asm_src, asm_dst);
            Assembly.Jmp end_lbl;
            Assembly.Label out_of_bounds;
            Assembly.Mov (Assembly.Quadword, asm_src, r1);
            Assembly.Mov (Assembly.Quadword, r1, r2);
            Assembly.Unary (Assembly.Shr, Assembly.Quadword, r2);
            Assembly.Binary { op = Assembly.And; t = Assembly.Quadword; src = Assembly.Imm 1L; dst = r1 };
            Assembly.Binary { op = Assembly.Or; t = Assembly.Quadword; src = r1; dst = r2 };
            Assembly.Cvtsi2sd (Assembly.Quadword, r2, asm_dst);
            Assembly.Binary { op = Assembly.Add; t = Assembly.Double; src = asm_dst; dst = asm_dst };
            Assembly.Label end_lbl;
          ])
      }
  | Tacky.DoubleToUInt { src = src; dst = dst } ->
      result {
        let! state', asm_src = convertVal state src
        let! state'', asm_dst = convertVal state' dst
        let! dstType = Tacky.typeOfVal state.symbols dst
        if dstType = Types.UChar then
          return (state'', [ Assembly.Cvttsd2si (Assembly.Longword, asm_src, Assembly.Reg Assembly.R9); Assembly.Mov (Assembly.Byte, Assembly.Reg Assembly.R9, asm_dst) ])
        else if dstType = Types.UInt then
            return (state'',
            [
              Assembly.Cvttsd2si (Assembly.Quadword, asm_src, Assembly.Reg Assembly.R9);
              Assembly.Mov (Assembly.Longword, Assembly.Reg Assembly.R9, asm_dst);
            ])
        else
          let state''', out_of_bounds = nextLabel "dbl2ulong.oob" state''
          let state'''', end_lbl = nextLabel "dbl2ulong.end" state'''
          let state5, upper_bound = addConstant None 9223372036854775808.0 state''''
          let upper_bound_as_int =
            Assembly.Imm System.Int64.MinValue
          let r, x = (Assembly.Reg Assembly.R9, Assembly.Reg Assembly.XMM7)
          return (state5,
          [
            Assembly.Cmp (Assembly.Double, Assembly.Data (upper_bound, 0), asm_src);
            Assembly.JmpCC (Assembly.AE, out_of_bounds);
            Assembly.Cvttsd2si (Assembly.Quadword, asm_src, asm_dst);
            Assembly.Jmp end_lbl;
            Assembly.Label out_of_bounds;
            Assembly.Mov (Assembly.Double, asm_src, x);
            Assembly.Binary { op = Assembly.Sub; t = Assembly.Double; src = Assembly.Data (upper_bound, 0); dst = x };
            Assembly.Cvttsd2si (Assembly.Quadword, x, asm_dst);
            Assembly.Mov (Assembly.Quadword, upper_bound_as_int, r);
            Assembly.Binary { op = Assembly.Add; t = Assembly.Quadword; src = r; dst = asm_dst };
            Assembly.Label end_lbl;
          ])
      }
  | Tacky.CopyToOffset { src = src; dst = dst; offset = offset } ->
      result {
        let! srcType = Tacky.typeOfVal state.symbols src
        if TypeUtils.isScalar srcType then
          let! state', asm_src = convertVal state src
          let! t = asmType state.typeTable state.symbols src
          return (state', [ Assembly.Mov (t, asm_src, Assembly.PseudoMem (dst, offset)) ])
        else
          let! state', asm_src = convertVal state src
          let asm_dst = Assembly.PseudoMem (dst, offset)
          let! sz = TypeUtils.getSize state.typeTable srcType
          let byte_count = int sz
          let! instrs = copyBytes asm_src asm_dst byte_count
          return (state', instrs)
      }
  | Tacky.CopyFromOffset { src = src; dst = dst; offset = offset } ->
      result {
        let! dstType = Tacky.typeOfVal state.symbols dst
        if TypeUtils.isScalar dstType then
          let! state', asm_dst = convertVal state dst
          let! t = asmType state.typeTable state.symbols dst
          return (state', [ Assembly.Mov (t, Assembly.PseudoMem (src, offset), asm_dst) ])
        else
          let asm_src = Assembly.PseudoMem (src, offset)
          let! state', asm_dst = convertVal state dst
          let! sz = TypeUtils.getSize state.typeTable dstType
          let byte_count = int sz
          let! instrs = copyBytes asm_src asm_dst byte_count
          return (state', instrs)
      }
  | Tacky.AddPtr { ptr = ptr; index = Tacky.Constant (Const.ConstLong c); scale = scale; dst = dst } ->
      result {
        let i = int c
        let! state', asm_ptr = convertVal state ptr
        let! state'', asm_dst = convertVal state' dst
        return (state'',
        [
          Assembly.Mov (Assembly.Quadword, asm_ptr, Assembly.Reg Assembly.R9);
          Assembly.Lea (Assembly.Memory (Assembly.R9, i * scale), asm_dst);
        ])
      }
  | Tacky.AddPtr { ptr = ptr; index = index; scale = scale; dst = dst } ->
      result {
        let! state', asm_ptr = convertVal state ptr
        let! state'', asm_index = convertVal state' index
        let! state''', asm_dst = convertVal state'' dst
        if scale = 1 || scale = 2 || scale = 4 || scale = 8 then
          return (state''',
          [
            Assembly.Mov (Assembly.Quadword, asm_ptr, Assembly.Reg Assembly.R8);
            Assembly.Mov (Assembly.Quadword, asm_index, Assembly.Reg Assembly.R9);
            Assembly.Lea (Assembly.Indexed { baseReg = Assembly.R8; index = Assembly.R9; scale = scale }, asm_dst);
          ])
        else
          return (state''',
          [
            Assembly.Mov (Assembly.Quadword, asm_ptr, Assembly.Reg Assembly.R8);
            Assembly.Mov (Assembly.Quadword, asm_index, Assembly.Reg Assembly.R9);
            Assembly.Binary
              {
                op = Assembly.Mult;
                t = Assembly.Quadword;
                src = Assembly.Imm (int64 scale);
                dst = Assembly.Reg Assembly.R9;
              };
            Assembly.Lea (Assembly.Indexed { baseReg = Assembly.R8; index = Assembly.R9; scale = 1 }, asm_dst);
          ])
      }

let private passParams state param_list return_on_stack =
  result {
    let! state', int_reg_params, dbl_reg_params, stack_params =
      classifyParameters state param_list return_on_stack

    let! copy_dst_ptr, remaining_int_regs =
      if return_on_stack then
        result {
          let! rest =
            match intParamPassingRegs with
            | _ :: rest -> Ok rest
            | [] -> Error (CompilerError.InternalError "Internal error: empty intParamPassingRegs")
          return
            ( [ Assembly.Mov (Assembly.Quadword, Assembly.Reg Assembly.DI, Assembly.Memory (Assembly.BP, -8)) ],
              rest )
        }
      else Ok ([], intParamPassingRegs)

    let! int_reg_instructions =
      resultTraverse (fun (idx, (param_t, param)) ->
        result {
          let! r =
              match List.tryItem idx remaining_int_regs with
              | Some r -> Ok r
              | None -> Error (CompilerError.InternalError "Internal error: int register index out of bounds")
          match param_t with
          | Assembly.ByteArray { size = size } ->
              return! copyBytesFromReg r param size
          | _ -> return [ Assembly.Mov (param_t, Assembly.Reg r, param) ]
        }
      ) (List.mapi (fun i x -> (i, x)) int_reg_params)

    let! dbl_reg_instructions =
      resultTraverse (fun (idx, param) ->
        result {
          let! r =
              match List.tryItem idx dblParamPassingRegs with
              | Some r -> Ok r
              | None -> Error (CompilerError.InternalError "Internal error: dbl register index out of bounds")
          return Assembly.Mov (Assembly.Double, Assembly.Reg r, param)
        }
      ) (List.mapi (fun i x -> (i, x)) dbl_reg_params)

    let! stack_instructions =
      resultTraverse (fun (idx, (param_t, param)) ->
        let stk = Assembly.Memory (Assembly.BP, 16 + (8 * idx))
        match param_t with
        | Assembly.ByteArray { size = size } -> copyBytes stk param size
        | _ -> Ok [ Assembly.Mov (param_t, stk, param) ]
      ) (List.mapi (fun i x -> (i, x)) stack_params)

    return (state',
    copy_dst_ptr
    @ List.concat int_reg_instructions
    @ dbl_reg_instructions
    @ List.concat stack_instructions)
  }

let private returnsOnStack state fn_name =
  result {
    let! entry = Symbols.get fn_name state.symbols
    match entry.symType with
    | Types.FunType (_, Types.Structure tag) ->
        let! state', classes = classifyStructure tag state
        return (state', match classes with Mem :: _ -> true | _ -> false)
    | Types.FunType _ -> return (state, false)
    | _ -> return! Error (CompilerError.InternalError "Internal error: not a function name")
  }

(* Special-case logic to get type/alignment of array; array variables w/ size
   >=16 bytes have alignment of 16 *)
let getVarAlignment tt = function
  | Types.Array _ as t ->
      result {
        let! sz = TypeUtils.getSize tt t
        if int sz >= 16 then return 16
        else return! TypeUtils.getAlignment tt t
      }
  | t -> TypeUtils.getAlignment tt t

let convertVarType tt = function
  | Types.Array _ as t ->
      result {
        let! sz = TypeUtils.getSize tt t
        let! align = getVarAlignment tt t
        return Assembly.ByteArray { size = int sz; alignment = align }
      }
  | other -> convertType tt other

let private convertTopLevel state = function
  | Tacky.Function { name = name; isGlobal = isGlobal; body = body; paramList = params_ } ->
      result {
        let! state', return_on_stack = returnsOnStack state name
        let params_as_tacky = List.map (fun name -> Tacky.Var name) params_
        let! state'', param_instructions = passParams state' params_as_tacky return_on_stack
        let! state''', body_instructions =
          resultFold (fun (s, acc) instr ->
            result {
              let! s', instrs = convertInstruction s instr
              return (s', acc @ instrs)
            }) (state'', []) body
        let instructions = param_instructions @ body_instructions
        return (state''', Assembly.Function { name = name; isGlobal = isGlobal; instructions = instructions })
      }
  | Tacky.StaticVariable { name = name; isGlobal = isGlobal; t = t; init = init } ->
      result {
        let! align = getVarAlignment state.typeTable t
        return (state, Assembly.StaticVariable
          { name = name; isGlobal = isGlobal; alignment = align; init = init })
      }
  | Tacky.StaticConstant { name = name; t = t; init = init } ->
      result {
        let! align = TypeUtils.getAlignment state.typeTable t
        return (state, Assembly.StaticConstant
          { name = name; alignment = align; init = init })
      }

let private convertConstant asmSymbols (key, (name, alignment)) =
  let dbl = System.BitConverter.Int64BitsToDouble key
  let asmSymbols' = AssemblySymbols.addConstant name Assembly.Double asmSymbols
  (asmSymbols', Assembly.StaticConstant
    { name = name; alignment = alignment; init = Initializers.DoubleInit dbl })

(* convert each symbol table entry to assembly symbol table equivalent*)
let private convertSymbol state name = function
  | { Symbols.symType = Types.FunType (paramTypes, retType);
      Symbols.attrs = Symbols.FunAttr { defined = defined };
    }
    when (TypeUtils.isComplete state.typeTable retType || retType = Types.Void)
         && List.forall (TypeUtils.isComplete state.typeTable) paramTypes ->
      result {
        let! state', ret_regs, return_on_stack = classifyReturnType state retType
        let! state'', param_regs = classifyParamTypes state' paramTypes return_on_stack
        let! state''', ros = returnsOnStack state'' name
        let asmSymbols' = AssemblySymbols.addFun name defined ros param_regs ret_regs state'''.asmSymbols
        return { state''' with asmSymbols = asmSymbols' }
      }
  | { Symbols.symType = Types.FunType _; Symbols.attrs = Symbols.FunAttr { defined = defined } } ->
      assert (not defined)
      let asmSymbols' = AssemblySymbols.addFun name defined false [] [] state.asmSymbols
      Ok { state with asmSymbols = asmSymbols' }
  | { Symbols.symType = t; attrs = Symbols.ConstAttr _ } ->
      result {
        let! ct = convertType state.typeTable t
        let asmSymbols' = AssemblySymbols.addConstant name ct state.asmSymbols
        return { state with asmSymbols = asmSymbols' }
      }
  | { Symbols.symType = t; attrs = Symbols.StaticAttr _ } when not (TypeUtils.isComplete state.typeTable t) ->
      let asmSymbols' = AssemblySymbols.addVar name Assembly.Byte true state.asmSymbols
      Ok { state with asmSymbols = asmSymbols' }
  | { Symbols.symType = t; attrs = Symbols.StaticAttr _ } ->
      result {
        let! vt = convertVarType state.typeTable t
        let asmSymbols' = AssemblySymbols.addVar name vt true state.asmSymbols
        return { state with asmSymbols = asmSymbols' }
      }
  | { Symbols.symType = t } ->
      result {
        let! vt = convertVarType state.typeTable t
        let asmSymbols' = AssemblySymbols.addVar name vt false state.asmSymbols
        return { state with asmSymbols = asmSymbols' }
      }

let gen counter tt symbols asmSymbols (Tacky.Program top_levels) =
  result {
    let state = { counter = counter; constants = Map.empty; classifiedStructures = Map.empty; typeTable = tt; symbols = symbols; asmSymbols = asmSymbols }
    let! state', tls =
      resultFold (fun (s, acc) tl ->
        result {
          let! s', tl' = convertTopLevel s tl
          return (s', acc @ [tl'])
        }) (state, []) top_levels
    let! state'' =
      resultFold (fun s (name, entry) -> convertSymbol s name entry) state' (Symbols.bindings state'.symbols)
    let asmSymbols', constants_list =
      state''.constants
      |> Map.toList
      |> List.fold (fun (asym, acc) entry ->
          let asym', tl = convertConstant asym entry
          (asym', acc @ [tl])) (state''.asmSymbols, [])

    let prog = Assembly.Program (constants_list @ tls)
    return (state''.counter, asmSymbols', prog)
  }
