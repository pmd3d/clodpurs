module ReplacePseudos

open Assembly
open ResultCE

(* Structure to keep track of what stack slots we've assigned so far *)
type ReplacementState = {
    current_offset : int (* last used stack slot *)
    offset_map : Map<string, int> (* map from pseudoregister to stack slots *)
}

let calculateOffset asmSymbols state name =
    result {
        let! size = AssemblySymbols.getSize name asmSymbols
        let! alignment = AssemblySymbols.getAlignment name asmSymbols
        let new_offset =
            Rounding.roundAwayFromZero alignment (state.current_offset - size)

        let new_state = {
            current_offset = new_offset
            offset_map = Map.add name new_offset state.offset_map
        }
        return (new_state, new_offset)
    }

let replaceOperand asmSymbols state = function
    (* if it's a pseudoregister, replace it with a stack slot *)
    | Pseudo s ->
        result {
            let! isStatic = AssemblySymbols.isStatic s asmSymbols
            if isStatic then return (state, Data (s, 0))
            else
                match Map.tryFind s state.offset_map with
                (* We've already assigned this operand a stack slot *)
                | Some offset -> return (state, Memory (BP, offset))
                (* We haven't already assigned it a stack slot;
                 * assign it and update state *)
                | None ->
                    let! new_state, new_offset = calculateOffset asmSymbols state s
                    return (new_state, Memory (BP, new_offset))
        }
    | PseudoMem (s, offset) ->
        result {
            let! isStatic = AssemblySymbols.isStatic s asmSymbols
            if isStatic then
                return (state, Data (s, offset))
            else
                match Map.tryFind s state.offset_map with
                (* We've already assigned this operand a stack slot *)
                | Some var_offset -> return (state, Memory (BP, offset + var_offset))
                | None ->
                    (* assign s a stack slot, and add its offset to the offset w/in s to
                       get new operand *)
                    let! new_state, new_var_offset = calculateOffset asmSymbols state s
                    return (new_state, Memory (BP, offset + new_var_offset))
        }
    (* not a pseudo, so nothing to do *)
    | other -> Ok (state, other)

let replacePseudosInInstruction asmSymbols state = function
    (* Replace src and dst of mov instruction *)
    | Mov (t, src, dst) ->
        result {
            let! state1, new_src = replaceOperand asmSymbols state src
            let! state2, new_dst = replaceOperand asmSymbols state1 dst
            let new_mov = Mov (t, new_src, new_dst)
            return (state2, new_mov)
        }
    | Movsx fields ->
        result {
            let! state1, new_src = replaceOperand asmSymbols state fields.src
            let! state2, new_dst = replaceOperand asmSymbols state1 fields.dst
            let new_movsx = Movsx { fields with src = new_src; dst = new_dst }
            return (state2, new_movsx)
        }
    | MovZeroExtend fields ->
        result {
            let! state1, new_src = replaceOperand asmSymbols state fields.src
            let! state2, new_dst = replaceOperand asmSymbols state1 fields.dst
            let new_movzx =
                MovZeroExtend { fields with src = new_src; dst = new_dst }
            return (state2, new_movzx)
        }
    | Lea (src, dst) ->
        result {
            let! state1, new_src = replaceOperand asmSymbols state src
            let! state2, new_dst = replaceOperand asmSymbols state1 dst
            let new_lea = Lea (new_src, new_dst)
            return (state2, new_lea)
        }
        (* Replace dst of unary instruction *)
    | Unary (t, op, dst) ->
        result {
            let! state1, new_dst = replaceOperand asmSymbols state dst
            let new_unary = Unary (t, op, new_dst)
            return (state1, new_unary)
        }
    | Binary { op = op; t = t; src = src; dst = dst } ->
        result {
            let! state1, new_src = replaceOperand asmSymbols state src
            let! state2, new_dst = replaceOperand asmSymbols state1 dst
            let new_binary = Binary { op = op; t = t; src = new_src; dst = new_dst }
            return (state2, new_binary)
        }
    | Cmp (t, op1, op2) ->
        result {
            let! state1, new_op1 = replaceOperand asmSymbols state op1
            let! state2, new_op2 = replaceOperand asmSymbols state1 op2
            let new_cmp = Cmp (t, new_op1, new_op2)
            return (state2, new_cmp)
        }
    | Idiv (t, op) ->
        result {
            let! state1, new_op = replaceOperand asmSymbols state op
            return (state1, Idiv (t, new_op))
        }
    | Div (t, op) ->
        result {
            let! state1, new_op = replaceOperand asmSymbols state op
            return (state1, Div (t, new_op))
        }
    (* Ret instruction has no operands, doesn't need to be rewritten *)
    | SetCC (code, op) ->
        result {
            let! state1, new_op = replaceOperand asmSymbols state op
            return (state1, SetCC (code, new_op))
        }
    | Push op ->
        result {
            let! state1, new_op = replaceOperand asmSymbols state op
            return (state1, Push new_op)
        }
    | Cvttsd2si (t, src, dst) ->
        result {
            let! state1, new_src = replaceOperand asmSymbols state src
            let! state2, new_dst = replaceOperand asmSymbols state1 dst
            let new_cvt = Cvttsd2si (t, new_src, new_dst)
            return (state2, new_cvt)
        }
    | Cvtsi2sd (t, src, dst) ->
        result {
            let! state1, new_src = replaceOperand asmSymbols state src
            let! state2, new_dst = replaceOperand asmSymbols state1 dst
            let new_cvt = Cvtsi2sd (t, new_src, new_dst)
            return (state2, new_cvt)
        }
    | (Ret | Cdq _ | Label _ | JmpCC _ | Jmp _ | Call _) as other -> Ok (state, other)
    | Pop _ -> Error (CompilerError.InternalError "Internal error")

let replacePseudosInTl asmSymbols = function
    | Function { name = name; isGlobal = isGlobal; instructions = instructions } ->
        result {
            (* should we stick returns_on_stack in the AST or symbol table? *)
            let! returnsOnStack = AssemblySymbols.returnsOnStack name asmSymbols
            let starting_offset =
                if returnsOnStack then -8 else 0

            let init_state =
                { current_offset = starting_offset; offset_map = Map.empty }

            (* rewrite each instruction, tracking current offset/stack slot map as
               we go *)
            let! final_state, rev_instructions =
                resultFold (fun (st, acc) i ->
                    result {
                        let! st', i' = replacePseudosInInstruction asmSymbols st i
                        return (st', i' :: acc)
                    }) (init_state, []) instructions

            let fixed_instructions = List.rev rev_instructions

            let! asmSymbols' = AssemblySymbols.setBytesRequired name final_state.current_offset asmSymbols
            return (asmSymbols', Function { name = name; isGlobal = isGlobal; instructions = fixed_instructions })
        }
    | static_var -> Ok (asmSymbols, static_var)

let replacePseudos asmSymbols (Program tls) =
    resultFold (fun (syms, acc) tl ->
        result {
            let! syms', tl' = replacePseudosInTl syms tl
            return (syms', acc @ [tl'])
        }) (asmSymbols, []) tls
    |> Result.map (fun (syms, tls') -> (syms, Program tls'))
