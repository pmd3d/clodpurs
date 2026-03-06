module InstructionFixup

open Assembly
open ResultCE

let int32_max = int64 System.Int32.MaxValue
let int32_min = int64 System.Int32.MinValue
let isLarge imm = imm > int32_max || imm < int32_min

let isLargerThanUint imm =
  (* use unsigned upper-bound for positives *)
  let max_i = 4294967295L (* 2^32 - 1*)
  (* use signed 32-bit lower bound for negatives *)
  imm > max_i || imm < int32_min

let isLargerThanByte imm = imm >= 256L || imm < -128L
let isConstant = function Imm _ -> true | _ -> false

let isMemory = function
  | Memory _ | Data _ -> true
  | Indexed _ -> true
  | _ -> false

let isXmm = function
  | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10
  | XMM11 | XMM12 | XMM13 | XMM14 | XMM15 ->
      true
  | _ -> false

let fixupInstruction callee_saved_regs = function
  (* Mov can't move a value from one memory address to another *)
  | Mov (t, ((Memory _ | Data _) as src), ((Memory _ | Data _) as dst)) ->
      let scratch = if t = Double then Reg XMM14 else Reg R10
      [ Mov (t, src, scratch); Mov (t, scratch, dst) ]
  (* Mov can't move a large constant to a memory address *)
  | Mov (Quadword, (Imm i as src), ((Memory _ | Data _) as dst)) when isLarge i
    ->
      [ Mov (Quadword, src, Reg R10); Mov (Quadword, Reg R10, dst) ]
  (* Moving a quadword-size constant with a longword operand size produces
     assembler warning *)
  | Mov (Longword, Imm i, dst) when isLargerThanUint i ->
      (* reduce modulo 2^32 by zeroing out upper 32 bits
       * NOTE: can't use Int64.modulo b/c it just calculates remainder *)
      let bitmask = 0xffffffffL
      let reduced = i &&& bitmask
      [ Mov (Longword, Imm reduced, dst) ]
  (* Moving a longword-size constant with a byte operand size produces assembler
     warning *)
  | Mov (Byte, Imm i, dst) when isLargerThanByte i ->
      let reduced = int64 (sbyte i)
      [ Mov (Byte, Imm reduced, dst) ]
  (* Movsx can't handle immediate source or memory dst *)
  | Movsx
      {
        src_type = src_type;
        dst_type = dst_type;
        src = Imm _ as src;
        dst = (Memory _ | Data _) as dst;
      } ->
      [
        Mov (src_type, src, Reg R10);
        Movsx { src_type = src_type; dst_type = dst_type; src = Reg R10; dst = Reg R11 };
        Mov (dst_type, Reg R11, dst);
      ]
  | Movsx { src_type = src_type; dst_type = dst_type; src = Imm _ as src; dst = dst } ->
      [
        Mov (src_type, src, Reg R10);
        Movsx { src_type = src_type; dst_type = dst_type; src = Reg R10; dst = dst };
      ]
  | Movsx { src_type = src_type; dst_type = dst_type; src = src; dst = (Memory _ | Data _) as dst } ->
      [
        Movsx { src_type = src_type; dst_type = dst_type; src = src; dst = Reg R11 };
        Mov (dst_type, Reg R11, dst);
      ]
  | MovZeroExtend { src_type = Byte; src = Imm i; dst_type = dst_type; dst = dst } ->
      (* MovZeroExtend src can't be an immediate *)
      if isMemory dst then
        [
          Mov (Byte, Imm i, Reg R10);
          MovZeroExtend
            { src_type = Byte; src = Reg R10; dst_type = dst_type; dst = Reg R11 };
          Mov (dst_type, Reg R11, dst);
        ]
      else
        [
          Mov (Byte, Imm i, Reg R10);
          MovZeroExtend { src_type = Byte; src = Reg R10; dst_type = dst_type; dst = dst };
        ]
  | MovZeroExtend { src_type = Byte; dst_type = dst_type; src = src; dst = dst } when isMemory dst ->
      (* MovZeroExtend destination must be a register *)
      [
        MovZeroExtend { src_type = Byte; dst_type = dst_type; src = src; dst = Reg R11 };
        Mov (dst_type, Reg R11, dst);
      ]
  | MovZeroExtend { src_type = Longword; dst_type = dst_type; src = src; dst = dst } when isMemory dst
    ->
      (* to zero-extend longword to quadword, first copy into register, then
         move to destination *)
      [ Mov (Longword, src, Reg R11); Mov (dst_type, Reg R11, dst) ]
  | MovZeroExtend { src_type = Longword; src = src; dst = dst; dst_type = _ } ->
      (* if destination is already a register, zero-extend w/ a single mov
         instruction *)
      [ Mov (Longword, src, dst) ]
  (* Idiv can't operate on constants *)
  | Idiv (t, Imm i) -> [ Mov (t, Imm i, Reg R10); Idiv (t, Reg R10) ]
  | Div (t, Imm i) -> [ Mov (t, Imm i, Reg R10); Div (t, Reg R10) ]
  (* dst of lea must be a register *)
  | Lea (src, dst) when isMemory dst ->
      [ Lea (src, Reg R11); Mov (Quadword, Reg R11, dst) ]
  (* Binary operations on double require register as destination *)
  | Binary { t = Double; dst = Reg _ } as i -> [ i ]
  | Binary { op = op; t = Double; src = src; dst = dst } ->
      [
        Mov (Double, dst, Reg XMM15);
        Binary { op = op; t = Double; src = src; dst = Reg XMM15 };
        Mov (Double, Reg XMM15, dst);
      ]
  (* Add/Sub/And/Or can't take large immediates as source operands *)
  | Binary
      {
        op = (Add | Sub | And | Or) as op;
        t = Quadword;
        src = Imm i as src;
        dst = dst;
      }
    when isLarge i ->
      [
        Mov (Quadword, src, Reg R10);
        Binary { op = op; t = Quadword; src = Reg R10; dst = dst };
      ]
  (* Add/Sub can't use memory addresses for both operands *)
  | Binary
      {
        op = (Add | Sub | And | Or) as op;
        t = t;
        src = (Memory _ | Data _) as src;
        dst = (Memory _ | Data _) as dst;
      } ->
      [ Mov (t, src, Reg R10); Binary { op = op; t = t; src = Reg R10; dst = dst } ]
  (* Destination of Mult can't be in memory; src can't be a big operand *)
  | Binary
      {
        op = Mult;
        t = Quadword;
        src = Imm i as src;
        dst = (Memory _ | Data _) as dst;
      }
    when isLarge i ->
      (* rewrite both operands *)
      [
        Mov (Quadword, src, Reg R10);
        Mov (Quadword, dst, Reg R11);
        Binary { op = Mult; t = Quadword; src = Reg R10; dst = Reg R11 };
        Mov (Quadword, Reg R11, dst);
      ]
  | Binary { op = Mult; t = Quadword; src = Imm i as src; dst = dst } when isLarge i
    ->
      (* just rewrite src *)
      [
        Mov (Quadword, src, Reg R10);
        Binary { op = Mult; t = Quadword; src = Reg R10; dst = dst };
      ]
  | Binary { op = Mult; t = t; src = src; dst = (Memory _ | Data _) as dst } ->
      [
        Mov (t, dst, Reg R11);
        Binary { op = Mult; t = t; src = src; dst = Reg R11 };
        Mov (t, Reg R11, dst);
      ]
  (* destination of comisd must be a register *)
  | Cmp (Double, _, Reg _) as i -> [ i ]
  | Cmp (Double, src, dst) ->
      [ Mov (Double, dst, Reg XMM15); Cmp (Double, src, Reg XMM15) ]
  (* Both operands of cmp can't be in memory *)
  | Cmp (t, ((Memory _ | Data _) as src), ((Memory _ | Data _) as dst)) ->
      [ Mov (t, src, Reg R10); Cmp (t, Reg R10, dst) ]
  (* first operand of Cmp can't be a large constant, second can't be a constant
     at all *)
  | Cmp (Quadword, (Imm i as src), (Imm _ as dst)) when isLarge i ->
      [
        Mov (Quadword, src, Reg R10);
        Mov (Quadword, dst, Reg R11);
        Cmp (Quadword, Reg R10, Reg R11);
      ]
  | Cmp (Quadword, (Imm i as src), dst) when isLarge i ->
      [ Mov (Quadword, src, Reg R10); Cmp (Quadword, Reg R10, dst) ]
  | Cmp (t, src, Imm i) -> [ Mov (t, Imm i, Reg R11); Cmp (t, src, Reg R11) ]
  | Push (Reg r) when isXmm r ->
      [
        Binary
          { op = Sub; t = Quadword; src = Imm 8L; dst = Reg SP };
        Mov (Double, Reg r, Memory (SP, 0));
      ]
  | Push (Imm i as src) when isLarge i ->
      [ Mov (Quadword, src, Reg R10); Push (Reg R10) ]
      (* destination of cvttsd2si must be a register *)
  | Cvttsd2si (t, src, ((Memory _ | Data _) as dst)) ->
      [ Cvttsd2si (t, src, Reg R11); Mov (t, Reg R11, dst) ]
  | Cvtsi2sd (t, src, dst) as i ->
      if isConstant src && isMemory dst then
        [
          Mov (t, src, Reg R10);
          Cvtsi2sd (t, Reg R10, Reg XMM15);
          Mov (Double, Reg XMM15, dst);
        ]
      else if isConstant src then
        [ Mov (t, src, Reg R10); Cvtsi2sd (t, Reg R10, dst) ]
      else if isMemory dst then
        [ Cvtsi2sd (t, src, Reg XMM15); Mov (Double, Reg XMM15, dst) ]
      else [ i ]
  | Ret ->
      let restore_reg r = Pop r
      let restore_regs = List.map restore_reg callee_saved_regs |> List.rev
      restore_regs @ [ Ret ]
  | other -> [ other ]

let emitStackAdjustment bytes_for_locals callee_saved_count =
  let callee_saved_bytes = 8 * callee_saved_count
  let total_stack_bytes = callee_saved_bytes + bytes_for_locals
  let adjusted_stack_bytes =
    Rounding.roundAwayFromZero 16 total_stack_bytes
  let stack_adjustment =
    int64 (adjusted_stack_bytes - callee_saved_bytes)
  Binary { op = Sub; t = Quadword; src = Imm stack_adjustment; dst = Reg SP }

let fixupTl asmSymbols = function
  | Function { name = name; isGlobal = isGlobal; instructions = instructions } ->
      result {
          (* TODO bytes_required should be positive (fix this in replace_pseudos) *)
          let! bytes_req = AssemblySymbols.getBytesRequired name asmSymbols
          let stack_bytes = -bytes_req
          let! callee_saved_regs =
            AssemblySymbols.getCalleeSavedRegsUsed name asmSymbols |> Result.map Set.toList

          let save_reg r = Push (Reg r)
          let adjust_rsp =
            emitStackAdjustment stack_bytes (List.length callee_saved_regs)
          let setup_instructions =
            adjust_rsp :: List.map save_reg callee_saved_regs
          return Function
            {
              name = name;
              isGlobal = isGlobal;
              instructions =
                setup_instructions
                @ List.collect (fixupInstruction callee_saved_regs) instructions;
            }
      }
  | static_var -> Ok static_var

let fixupProgram asmSymbols (Program tls) =
  result {
      let! fixed_functions = resultTraverse (fixupTl asmSymbols) tls
      return Program fixed_functions
  }