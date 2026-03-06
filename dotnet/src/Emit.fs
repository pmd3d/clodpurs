module Emit

open Assembly
open ResultCE

let result = ResultBuilder()

let resultIter f items = resultTraverse f items |> Result.map ignore

let suffix = function
    | Byte -> Ok "b"
    | Longword -> Ok "l"
    | Quadword -> Ok "q"
    | Double -> Ok "sd"
    | ByteArray _ ->
        Error
            (CompilerError.InternalError
                "found instruction w/ non-scalar operand type")

let alignDirective platform =
    match platform with
    | Settings.OS_X -> ".balign"
    | Settings.Linux -> ".align"

let showLabel platform name =
    match platform with
    | Settings.OS_X -> "_" + name
    | Settings.Linux -> name

let showLocalLabel platform label =
    match platform with
    | Settings.OS_X -> "L" + label
    | Settings.Linux -> ".L" + label

let showFunName platform asmSymbols f =
    match platform with
    | Settings.OS_X -> Ok ("_" + f)
    | Settings.Linux ->
        result {
            let! defined = AssemblySymbols.isDefined f asmSymbols
            return if defined then f else f + "@PLT"
        }

let showLongReg = function
    | AX -> Ok "%eax"
    | BX -> Ok "%ebx"
    | CX -> Ok "%ecx"
    | DX -> Ok "%edx"
    | DI -> Ok "%edi"
    | SI -> Ok "%esi"
    | R8 -> Ok "%r8d"
    | R9 -> Ok "%r9d"
    | R10 -> Ok "%r10d"
    | R11 -> Ok "%r11d"
    | R12 -> Ok "%r12d"
    | R13 -> Ok "%r13d"
    | R14 -> Ok "%r14d"
    | R15 -> Ok "%r15d"
    | SP -> Error (CompilerError.InternalError "no 32-bit RSP")
    | BP -> Error (CompilerError.InternalError "no 32-bit RBP")
    | _ ->
        Error
            (CompilerError.InternalError
                "can't store longword type in XMM register")

let showQuadwordReg = function
    | AX -> Ok "%rax"
    | BX -> Ok "%rbx"
    | CX -> Ok "%rcx"
    | DX -> Ok "%rdx"
    | DI -> Ok "%rdi"
    | SI -> Ok "%rsi"
    | R8 -> Ok "%r8"
    | R9 -> Ok "%r9"
    | R10 -> Ok "%r10"
    | R11 -> Ok "%r11"
    | R12 -> Ok "%r12"
    | R13 -> Ok "%r13"
    | R14 -> Ok "%r14"
    | R15 -> Ok "%r15"
    | SP -> Ok "%rsp"
    | BP -> Ok "%rbp"
    | _ ->
        Error
            (CompilerError.InternalError
                "can't store quadword type in XMM register")

let showDoubleReg = function
    | XMM0 -> Ok "%xmm0"
    | XMM1 -> Ok "%xmm1"
    | XMM2 -> Ok "%xmm2"
    | XMM3 -> Ok "%xmm3"
    | XMM4 -> Ok "%xmm4"
    | XMM5 -> Ok "%xmm5"
    | XMM6 -> Ok "%xmm6"
    | XMM7 -> Ok "%xmm7"
    | XMM8 -> Ok "%xmm8"
    | XMM9 -> Ok "%xmm9"
    | XMM10 -> Ok "%xmm10"
    | XMM11 -> Ok "%xmm11"
    | XMM12 -> Ok "%xmm12"
    | XMM13 -> Ok "%xmm13"
    | XMM14 -> Ok "%xmm14"
    | XMM15 -> Ok "%xmm15"
    | _ ->
        Error
            (CompilerError.InternalError
                "can't store double type in general-purpose register")

let showByteReg = function
    | AX -> Ok "%al"
    | BX -> Ok "%bl"
    | CX -> Ok "%cl"
    | DX -> Ok "%dl"
    | DI -> Ok "%dil"
    | SI -> Ok "%sil"
    | R8 -> Ok "%r8b"
    | R9 -> Ok "%r9b"
    | R10 -> Ok "%r10b"
    | R11 -> Ok "%r11b"
    | R12 -> Ok "%r12b"
    | R13 -> Ok "%r13b"
    | R14 -> Ok "%r14b"
    | R15 -> Ok "%r15b"
    | SP -> Error (CompilerError.InternalError "no one-byte RSP")
    | BP -> Error (CompilerError.InternalError "no one-byte RBP")
    | _ ->
        Error
            (CompilerError.InternalError
                "can't store byte type in XMM register")

let showOperand platform asmSymbols t = function
    | Reg r ->
        (match t with
        | Byte -> showByteReg r
        | Longword -> showLongReg r
        | Quadword -> showQuadwordReg r
        | Double -> showDoubleReg r
        | ByteArray _ ->
            Error
                (CompilerError.InternalError
                    "can't store non-scalar operand in register"))
    | Imm i -> Ok (sprintf "$%s" (string i))
    | Memory(r, 0) ->
        result {
            let! rStr = showQuadwordReg r
            return sprintf "(%s)" rStr
        }
    | Memory(r, i) ->
        result {
            let! rStr = showQuadwordReg r
            return sprintf "%d(%s)" i rStr
        }
    | Data(name, offset) ->
        result {
            let! isConst = AssemblySymbols.isConstant name asmSymbols
            let lbl =
                if isConst then
                    showLocalLabel platform name
                else showLabel platform name
            if offset = 0 then return sprintf "%s(%%rip)" lbl
            else return sprintf "%s+%d(%%rip)" lbl offset
        }
    | Indexed { baseReg = b; index = index; scale = scale } ->
        result {
            let! bStr = showQuadwordReg b
            let! indexStr = showQuadwordReg index
            return sprintf "(%s, %s, %d)" bStr indexStr scale
        }
    (* printing out pseudoregisters is only for debugging *)
    | Pseudo name -> Ok (sprintf "%%%s" name)
    | PseudoMem(name, offset) -> Ok (sprintf "%d(%%%s)" offset name)

let showByteOperand platform asmSymbols = function
    | Reg r -> showByteReg r
    | other -> showOperand platform asmSymbols Longword other

let showUnaryInstruction = function
    | Neg -> "neg"
    | Not -> "not"
    | Shr -> "shr"

let showBinaryInstruction = function
    | Add -> Ok "add"
    | Sub -> Ok "sub"
    | Mult -> Ok "imul"
    | DivDouble -> Ok "div"
    | And -> Ok "and"
    | Or -> Ok "or"
    | Shl -> Ok "shl"
    | ShrBinop -> Ok "shr"
    | Xor ->
        Error
            (CompilerError.InternalError
                "should handle xor as special case")

let showCondCode = function
    | E -> "e"
    | NE -> "ne"
    | G -> "g"
    | GE -> "ge"
    | L -> "l"
    | LE -> "le"
    | A -> "a"
    | AE -> "ae"
    | B -> "b"
    | BE -> "be"

let emitInstruction platform asmSymbols (chan: System.IO.TextWriter) = function
    | Mov(t, src, dst) ->
        result {
            let! sfx = suffix t
            let! srcStr = showOperand platform asmSymbols t src
            let! dstStr = showOperand platform asmSymbols t dst
            chan.Write(sprintf "\tmov%s %s, %s\n" sfx srcStr dstStr)
        }
    | Unary(operator, t, dst) ->
        result {
            let! sfx = suffix t
            let! dstStr = showOperand platform asmSymbols t dst
            chan.Write(
                sprintf "\t%s%s %s\n"
                    (showUnaryInstruction operator)
                    sfx dstStr)
        }
    | Binary { op = Xor; t = Double; src = src; dst = dst } ->
        result {
            let! srcStr = showOperand platform asmSymbols Double src
            let! dstStr = showOperand platform asmSymbols Double dst
            chan.Write(sprintf "\txorpd %s, %s\n" srcStr dstStr)
        }
    | Binary { op = Mult; t = Double; src = src; dst = dst } ->
        result {
            let! srcStr = showOperand platform asmSymbols Double src
            let! dstStr = showOperand platform asmSymbols Double dst
            chan.Write(sprintf "\tmulsd %s, %s\n" srcStr dstStr)
        }
    | Binary { op = op; t = t; src = src; dst = dst } ->
        result {
            let! opStr = showBinaryInstruction op
            let! sfx = suffix t
            let! srcStr = showOperand platform asmSymbols t src
            let! dstStr = showOperand platform asmSymbols t dst
            chan.Write(sprintf "\t%s%s %s, %s\n" opStr sfx srcStr dstStr)
        }
    | Cmp(Double, src, dst) ->
        result {
            let! srcStr = showOperand platform asmSymbols Double src
            let! dstStr = showOperand platform asmSymbols Double dst
            chan.Write(sprintf "\tcomisd %s, %s\n" srcStr dstStr)
        }
    | Cmp(t, src, dst) ->
        result {
            let! sfx = suffix t
            let! srcStr = showOperand platform asmSymbols t src
            let! dstStr = showOperand platform asmSymbols t dst
            chan.Write(sprintf "\tcmp%s %s, %s\n" sfx srcStr dstStr)
        }
    | Idiv(t, operand) ->
        result {
            let! sfx = suffix t
            let! opStr = showOperand platform asmSymbols t operand
            chan.Write(sprintf "\tidiv%s %s\n" sfx opStr)
        }
    | Div(t, operand) ->
        result {
            let! sfx = suffix t
            let! opStr = showOperand platform asmSymbols t operand
            chan.Write(sprintf "\tdiv%s %s\n" sfx opStr)
        }
    | Lea(src, dst) ->
        result {
            let! srcStr = showOperand platform asmSymbols Quadword src
            let! dstStr = showOperand platform asmSymbols Quadword dst
            chan.Write(sprintf "\tleaq %s, %s\n" srcStr dstStr)
        }
    | Cdq Longword ->
        chan.Write("\tcdq\n")
        Ok ()
    | Cdq Quadword ->
        chan.Write("\tcqo\n")
        Ok ()
    | Jmp lbl ->
        chan.Write(sprintf "\tjmp %s\n" (showLocalLabel platform lbl))
        Ok ()
    | JmpCC(code, lbl) ->
        chan.Write(
            sprintf "\tj%s %s\n" (showCondCode code)
                (showLocalLabel platform lbl))
        Ok ()
    | SetCC(code, operand) ->
        result {
            let! opStr = showByteOperand platform asmSymbols operand
            chan.Write(sprintf "\tset%s %s\n" (showCondCode code) opStr)
        }
    | Label lbl ->
        chan.Write(sprintf "%s:\n" (showLocalLabel platform lbl))
        Ok ()
    | Push op ->
        result {
            let! opStr = showOperand platform asmSymbols Quadword op
            chan.Write(sprintf "\tpushq %s\n" opStr)
        }
    | Pop r ->
        result {
            let! rStr = showQuadwordReg r
            chan.Write(sprintf "\tpopq %s\n" rStr)
        }
    | Call f ->
        result {
            let! fName = showFunName platform asmSymbols f
            chan.Write(sprintf "\tcall %s\n" fName)
        }
    | Movsx { src_type = src_type; dst_type = dst_type; src = src;
              dst = dst } ->
        result {
            let! srcSfx = suffix src_type
            let! dstSfx = suffix dst_type
            let! srcStr = showOperand platform asmSymbols src_type src
            let! dstStr = showOperand platform asmSymbols dst_type dst
            chan.Write(
                sprintf "\tmovs%s%s %s, %s\n" srcSfx dstSfx srcStr dstStr)
        }
    | MovZeroExtend { src_type = src_type; dst_type = dst_type;
                      src = src; dst = dst } ->
        result {
            let! srcSfx = suffix src_type
            let! dstSfx = suffix dst_type
            let! srcStr = showOperand platform asmSymbols src_type src
            let! dstStr = showOperand platform asmSymbols dst_type dst
            chan.Write(
                sprintf "\tmovz%s%s %s, %s\n" srcSfx dstSfx srcStr dstStr)
        }
    | Cvtsi2sd(t, src, dst) ->
        result {
            let! sfx = suffix t
            let! srcStr = showOperand platform asmSymbols t src
            let! dstStr = showOperand platform asmSymbols Double dst
            chan.Write(
                sprintf "\tcvtsi2sd%s %s, %s\n" sfx srcStr dstStr)
        }
    | Cvttsd2si(t, src, dst) ->
        result {
            let! sfx = suffix t
            let! srcStr = showOperand platform asmSymbols Double src
            let! dstStr = showOperand platform asmSymbols t dst
            chan.Write(
                sprintf "\tcvttsd2si%s %s, %s\n" sfx srcStr dstStr)
        }
    | Ret ->
        chan.Write(
            "\n\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n")
        Ok ()
    | Cdq(Double | Byte | ByteArray _) ->
        Error
            (CompilerError.InternalError
                "can't apply cdq to a byte or non-integer type")

let emitGlobalDirective (chan: System.IO.TextWriter) isGlobal label =
    if isGlobal then chan.Write(sprintf "\t.globl %s\n" label)

let escape s =
    let escapeChar c =
        if StringUtil.isAlnum c then string c
        (* use octal escape for everything except alphanumeric values
         * make sure to pad out octal escapes to 3 digits so we don't, e.g.
         * escape "hello 1" as "hello\401" *)
        else sprintf "\\%03o" (int c)
    String.concat ""
        (s |> Seq.map escapeChar |> Seq.toList)

let emitInit platform (chan: System.IO.TextWriter) = function
    | Initializers.IntInit i ->
        chan.Write(sprintf "\t.long %s\n" (string i))
    | Initializers.LongInit l ->
        chan.Write(sprintf "\t.quad %d\n" l)
    | Initializers.UIntInit u ->
        chan.Write(sprintf "\t.long %s\n" (string u))
    | Initializers.ULongInit l ->
        chan.Write(sprintf "\t.quad %s\n" (string l))
    | Initializers.CharInit c ->
        chan.Write(sprintf "\t.byte %s\n" (string c))
    | Initializers.UCharInit uc ->
        chan.Write(sprintf "\t.byte %s\n" (string uc))
    | Initializers.DoubleInit d ->
        chan.Write(
            sprintf "\t.quad %d\n"
                (System.BitConverter.DoubleToInt64Bits d))
    (* a partly-initialized array can include a mix of zero and non-zero
       initializers *)
    | Initializers.ZeroInit byte_count ->
        chan.Write(sprintf "\t.zero %d\n" byte_count)
    | Initializers.StringInit(s, true) ->
        chan.Write(sprintf "\t.asciz \"%s\"\n" (escape s))
    | Initializers.StringInit(s, false) ->
        chan.Write(sprintf "\t.ascii \"%s\"\n" (escape s))
    | Initializers.PointerInit lbl ->
        chan.Write(sprintf "\t.quad %s\n" (showLocalLabel platform lbl))

let emitConstant platform (chan: System.IO.TextWriter) name alignment init =
    let constantSectionName =
        match (platform, init) with
        | Settings.Linux, _ -> Ok ".section .rodata"
        | Settings.OS_X, Initializers.StringInit _ -> Ok ".cstring"
        | Settings.OS_X, _ ->
            if alignment = 8 then Ok ".literal8"
            else if alignment = 16 then Ok ".literal16"
            else
                Error
                    (CompilerError.InternalError
                        "found constant with bad alignment")
    result {
        let! sectionName = constantSectionName
        chan.Write(
            sprintf "\n\t%s\n\t%s %d\n  %s:\n"
                sectionName (alignDirective platform) alignment
                (showLocalLabel platform name))
        emitInit platform chan init
        (* macOS linker gets cranky if you write only 8 bytes to .literal16 section *)
        if sectionName = ".literal16" then
            emitInit platform chan (Initializers.LongInit 0L)
    }

let emitTl platform asmSymbols (chan: System.IO.TextWriter) = function
    | Function { name = name; isGlobal = isGlobal;
                 instructions = instructions } ->
        let label = showLabel platform name
        emitGlobalDirective chan isGlobal label
        chan.Write(
            sprintf "\n\t.text\n%s:\n\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n"
                label)
        resultIter (emitInstruction platform asmSymbols chan) instructions
    | StaticVariable { name = name; isGlobal = isGlobal; init = init;
                       alignment = alignment }
        when List.forall Initializers.isZero init ->
        let label = showLabel platform name
        emitGlobalDirective chan isGlobal label
        chan.Write(
            sprintf "\n\t.bss\n\t%s %d\n%s:\n"
                (alignDirective platform) alignment label)
        List.iter (emitInit platform chan) init
        Ok ()
    | StaticVariable { name = name; isGlobal = isGlobal; init = init;
                       alignment = alignment } ->
        let label = showLabel platform name
        emitGlobalDirective chan isGlobal label
        chan.Write(
            sprintf "\n\t.data\n\t%s %d\n%s:\n"
                (alignDirective platform) alignment label)
        List.iter (emitInit platform chan) init
        Ok ()
    | StaticConstant { name = name; alignment = alignment; init = init } ->
        emitConstant platform chan name alignment init

let emitStackNote platform (chan: System.IO.TextWriter) =
    match platform with
    | Settings.OS_X -> ()
    | Settings.Linux ->
        chan.Write("\t.section .note.GNU-stack,\"\",@progbits\n")

let emitToString platform asmSymbols (Program tls) =
    use sw = new System.IO.StringWriter()
    result {
        do! resultIter (emitTl platform asmSymbols sw) tls
        emitStackNote platform sw
        return sw.ToString()
    }
