module Codegen where

import Prelude

import Assembly (AsmBinaryOperator(..), AsmCondCode(..), AsmInstruction(..), AsmOperand(..), AsmProgram(..), AsmReg(..), AsmTopLevel(..), AsmType(..), AsmUnaryOperator(..))
import AssemblySymbols as AssemblySymbols
import AssemblySymbols (AsmSymbolTableMap)
import CompilerError (CompilerError(..))
import Const as Const
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Foldable (all, foldl, length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:), concat, reverse)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Initializers (StaticInit(..))
import ListUtil as ListUtil
import ResultUtil (resultFold, resultTraverse)
import Symbols as Symbols
import Tacky (TackyInstruction(..), TackyProgram(..), TackyTopLevel(..), TackyUnaryOperator(Complement, Negate), TackyBinaryOperator(Subtract, Multiply, Divide, Mod, Equal, NotEqual, LessThan, LessOrEqual, GreaterThan, GreaterOrEqual), TackyVal(..))
import Tacky as Tacky
import TypeTable as TypeTable
import TypeUtils as TypeUtils
import Types as Types
import UniqueIds as UniqueIds

foreign import doubleToInt64BitsImpl :: Number -> String
foreign import int64BitsToDoubleImpl :: String -> Number
foreign import int64MinValue :: Int
foreign import bigIntToInt :: BigInt.BigInt -> Int

-- Parameter classification for System V AMD64 ABI
data ParamClass = Mem | SSE | Integer

derive instance eqParamClass :: Eq ParamClass

type CodegenState =
  { counter :: UniqueIds.Counter
  , constants :: Map.Map String (Tuple String Int) -- key is int64 bits as string
  , classifiedStructures :: Map.Map String (List ParamClass)
  , typeTable :: TypeTable.TypeTableMap
  , symbols :: Symbols.SymbolTableMap
  , asmSymbols :: AsmSymbolTableMap
  }

intParamPassingRegs :: List AsmReg
intParamPassingRegs = DI : SI : DX : CX : R8 : R9 : Nil

dblParamPassingRegs :: List AsmReg
dblParamPassingRegs = XMM0 : XMM1 : XMM2 : XMM3 : XMM4 : XMM5 : XMM6 : XMM7 : Nil

zero :: AsmOperand
zero = Imm (BigInt.fromInt 0)

nextLabel :: String -> CodegenState -> Tuple CodegenState String
nextLabel prefix state =
  let (Tuple c name) = UniqueIds.makeLabel prefix state.counter
  in Tuple (state { counter = c }) name

addConstant :: Maybe Int -> Number -> CodegenState -> Tuple CodegenState String
addConstant alignmentOpt dbl state =
  let alignment = fromMaybe 8 alignmentOpt
      key = doubleToInt64BitsImpl dbl
  in case Map.lookup key state.constants of
    Just (Tuple name old_alignment) ->
      let state' = state { constants = Map.insert key (Tuple name (max alignment old_alignment)) state.constants }
      in Tuple state' name
    Nothing ->
      let Tuple state' name = nextLabel "dbl" state
          state'' = state' { constants = Map.insert key (Tuple name alignment) state'.constants }
      in Tuple state'' name

getEightbyteType :: Int -> Int -> AsmType
getEightbyteType eightbyte_idx total_var_size =
  let bytes_left = total_var_size - (eightbyte_idx * 8)
  in if bytes_left >= 8 then Quadword
     else case bytes_left of
       4 -> Longword
       1 -> Byte
       x -> ByteArray { size: x, alignment: 8 }

addOffset :: Int -> AsmOperand -> Either CompilerError AsmOperand
addOffset n = case _ of
  PseudoMem base_ off -> Right (PseudoMem base_ (off + n))
  Memory r off -> Right (Memory r (off + n))
  _ -> Left (InternalError "Internal error: trying to copy data to or from non-memory operand")

copyBytes :: AsmOperand -> AsmOperand -> Int -> Either CompilerError (List AsmInstruction)
copyBytes src_val dst_val byte_count =
  if byte_count == 0 then Right Nil
  else do
    let r = if byte_count < 4 then { t: Byte, s: 1 }
            else if byte_count < 8 then { t: Longword, s: 4 }
            else { t: Quadword, s: 8 }
    next_src <- addOffset r.s src_val
    next_dst <- addOffset r.s dst_val
    rest <- copyBytes next_src next_dst (byte_count - r.s)
    Right (Mov r.t src_val dst_val : rest)

copyBytesToReg :: AsmOperand -> AsmReg -> Int -> Either CompilerError (List AsmInstruction)
copyBytesToReg src_val dst_reg byte_count =
  let copy_byte i = do
        offsetSrc <- addOffset i src_val
        let mv = Mov Byte offsetSrc (Reg dst_reg)
        if i == 0 then Right (mv : Nil)
        else Right (mv : Binary { op: Shl, t: Quadword, src: Imm (BigInt.fromInt 8), dst: Reg dst_reg } : Nil)
      byte_counts = reverse (List.range 0 (byte_count - 1))
  in map concat (resultTraverse copy_byte byte_counts)

copyBytesFromReg :: AsmReg -> AsmOperand -> Int -> Either CompilerError (List AsmInstruction)
copyBytesFromReg src_reg dst_val byte_count =
  let copy_byte i = do
        offsetDst <- addOffset i dst_val
        let mv = Mov Byte (Reg src_reg) offsetDst
        if i < byte_count - 1 then
          Right (mv : Binary { op: ShrBinop, t: Quadword, src: Imm (BigInt.fromInt 8), dst: Reg src_reg } : Nil)
        else Right (mv : Nil)
  in map concat (resultTraverse copy_byte (List.range 0 (byte_count - 1)))

convertVal :: CodegenState -> TackyVal -> Either CompilerError (Tuple CodegenState AsmOperand)
convertVal state = case _ of
  Constant (Const.ConstChar c) -> Right (Tuple state (Imm (BigInt.fromInt c)))
  Constant (Const.ConstUChar uc) -> Right (Tuple state (Imm (BigInt.fromInt uc)))
  Constant (Const.ConstInt i) -> Right (Tuple state (Imm (BigInt.fromInt i)))
  Constant (Const.ConstLong l) -> Right (Tuple state (Imm l))
  Constant (Const.ConstUInt u) -> Right (Tuple state (Imm u))
  Constant (Const.ConstULong ul) -> Right (Tuple state (Imm ul))
  Constant (Const.ConstDouble d) ->
    let Tuple state' name = addConstant Nothing d state
    in Right (Tuple state' (Data name 0))
  Var v -> do
    entry <- Symbols.get v state.symbols
    if TypeUtils.isScalar entry.symType then Right (Tuple state (Pseudo v))
    else Right (Tuple state (PseudoMem v 0))

convertType :: TypeTable.TypeTableMap -> Types.CType -> Either CompilerError AsmType
convertType tt = case _ of
  Types.Int -> Right Longword
  Types.UInt -> Right Longword
  Types.Long -> Right Quadword
  Types.ULong -> Right Quadword
  Types.Pointer _ -> Right Quadword
  Types.Char -> Right Byte
  Types.SChar -> Right Byte
  Types.UChar -> Right Byte
  Types.Double -> Right AsmDouble
  t@(Types.Array _ _) -> do
    sz <- TypeUtils.getSize tt t
    align <- TypeUtils.getAlignment tt t
    Right (ByteArray { size: bigIntToInt sz, alignment: align })
  t@(Types.Structure _) -> do
    sz <- TypeUtils.getSize tt t
    align <- TypeUtils.getAlignment tt t
    Right (ByteArray { size: bigIntToInt sz, alignment: align })
  t -> Left (InternalError ("Internal error, converting type to assembly: " <> show t))

asmType :: TypeTable.TypeTableMap -> Symbols.SymbolTableMap -> TackyVal -> Either CompilerError AsmType
asmType tt st v = do
  t <- Tacky.typeOfVal st v
  convertType tt t

convertUnop :: TackyUnaryOperator -> Either CompilerError AsmUnaryOperator
convertUnop = case _ of
  Complement -> Right Not
  Negate -> Right Neg
  Tacky.Not -> Left (InternalError "Internal error, can't convert TACKY not directly to assembly")

convertBinop :: TackyBinaryOperator -> Either CompilerError AsmBinaryOperator
convertBinop = case _ of
  Tacky.Add -> Right Add
  Subtract -> Right Sub
  Multiply -> Right Mult
  Divide -> Right DivDouble
  _ -> Left (InternalError "Internal error: not a binary assembly instruction")

convertCondCode :: Boolean -> TackyBinaryOperator -> Either CompilerError AsmCondCode
convertCondCode signed = case _ of
  Equal -> Right E
  NotEqual -> Right NE
  GreaterThan -> Right (if signed then G else A)
  GreaterOrEqual -> Right (if signed then GE else AE)
  LessThan -> Right (if signed then L else B)
  LessOrEqual -> Right (if signed then LE else BE)
  _ -> Left (InternalError "Internal error: not a condition code")

classifyNewStructure :: TypeTable.TypeTableMap -> String -> Either CompilerError (List ParamClass)
classifyNewStructure tt tag = do
  structDef <- TypeTable.find tag tt
  let size = structDef.size
  if size > 16 then do
    let eightbyte_count = (size / 8) + if size `mod` 8 == 0 then 0 else 1
    Right (ListUtil.makeList eightbyte_count Mem)
  else do
    let f = case _ of
              Types.Structure struct_tag -> do
                member_types <- TypeTable.getMemberTypes struct_tag tt
                results <- resultTraverse f member_types
                Right (concat results)
              Types.Array elemType arrSize -> do
                elemResult <- f elemType
                Right (concat (ListUtil.makeList (bigIntToInt arrSize) elemResult))
              t -> Right (t : Nil)
    scalar_types <- f (Types.Structure tag)
    case scalar_types of
      Nil -> Left (InternalError "Internal error: empty scalar types for structure")
      first : _ ->
        case ListUtil.tryLast scalar_types of
          Nothing -> Left (InternalError "Internal error: empty scalar types for structure")
          Just last_ ->
            if size > 8 then do
              let first_class = if first == Types.Double then SSE else Integer
              let last_class = if last_ == Types.Double then SSE else Integer
              Right (first_class : last_class : Nil)
            else if first == Types.Double then Right (SSE : Nil)
            else Right (Integer : Nil)

classifyStructure :: String -> CodegenState -> Either CompilerError (Tuple CodegenState (List ParamClass))
classifyStructure tag state =
  case Map.lookup tag state.classifiedStructures of
    Just classes -> Right (Tuple state classes)
    Nothing -> do
      classes <- classifyNewStructure state.typeTable tag
      let state' = state { classifiedStructures = Map.insert tag classes state.classifiedStructures }
      Right (Tuple state' classes)

classifyParamsHelper :: CodegenState -> List (Tuple Types.CType AsmOperand) -> Boolean
  -> Either CompilerError (Tuple CodegenState (Tuple (List (Tuple AsmType AsmOperand)) (Tuple (List AsmOperand) (List (Tuple AsmType AsmOperand)))))
classifyParamsHelper state typed_asm_vals return_on_stack =
  let int_regs_available = if return_on_stack then 5 else 6
      process_one_param acc (Tuple tacky_t operand) = do
        let { st, ints: int_reg_args, dbls: dbl_reg_args, stk: stack_args } = acc
        t <- convertType st.typeTable tacky_t
        let typed_operand = Tuple t operand
        case tacky_t of
          Types.Structure s -> do
            var_name <- case operand of
              PseudoMem n 0 -> Right n
              _ -> Left (InternalError "Bad structure operand")
            sz <- TypeUtils.getSize st.typeTable tacky_t
            let var_size = bigIntToInt sz
            Tuple st' classes <- classifyStructure s st
            Tuple updated_int (Tuple updated_dbl use_stack) <-
              case classes of
                Nil -> Left (InternalError "Internal error: empty classification for structure")
                first : _ | first == Mem -> Right (Tuple int_reg_args (Tuple dbl_reg_args true))
                _ -> do
                  let process_one_eightbyte { i, tInts: tentative_ints, tDbls: tentative_dbls } cls = do
                        let eb_operand = PseudoMem var_name (i * 8)
                        case cls of
                          SSE -> Right { i: i + 1, tInts: tentative_ints, tDbls: eb_operand : tentative_dbls }
                          Integer -> do
                            let eightbyte_type = getEightbyteType i var_size
                            Right { i: i + 1, tInts: Tuple eightbyte_type eb_operand : tentative_ints, tDbls: tentative_dbls }
                          Mem -> Left (InternalError "Internal error: found eightbyte in Mem class, but first eighbyte wasn't Mem")
                  result_ <- resultFold process_one_eightbyte { i: 0, tInts: int_reg_args, tDbls: dbl_reg_args } classes
                  if length result_.tInts <= int_regs_available && length result_.tDbls <= 8
                    then Right (Tuple result_.tInts (Tuple result_.tDbls false))
                    else Right (Tuple int_reg_args (Tuple dbl_reg_args true))
            let add_stack_args stk i =
                  let eightbyte_type = getEightbyteType i var_size
                  in Tuple eightbyte_type (PseudoMem var_name (i * 8)) : stk
            let updated_stack_args =
                  if use_stack then
                    foldl add_stack_args stack_args (List.range 0 (length classes - 1))
                  else stack_args
            Right { st: st', ints: updated_int, dbls: updated_dbl, stk: updated_stack_args }
          Types.Double ->
            if length dbl_reg_args < 8 then
              Right { st, ints: int_reg_args, dbls: operand : dbl_reg_args, stk: stack_args }
            else Right { st, ints: int_reg_args, dbls: dbl_reg_args, stk: typed_operand : stack_args }
          _ ->
            if length int_reg_args < int_regs_available then
              Right { st, ints: typed_operand : int_reg_args, dbls: dbl_reg_args, stk: stack_args }
            else Right { st, ints: int_reg_args, dbls: dbl_reg_args, stk: typed_operand : stack_args }
  in do
    result_ <- resultFold process_one_param { st: state, ints: Nil, dbls: Nil, stk: Nil } typed_asm_vals
    Right (Tuple result_.st (Tuple (reverse result_.ints) (Tuple (reverse result_.dbls) (reverse result_.stk))))

classifyParameters :: CodegenState -> List TackyVal -> Boolean
  -> Either CompilerError (Tuple CodegenState (Tuple (List (Tuple AsmType AsmOperand)) (Tuple (List AsmOperand) (List (Tuple AsmType AsmOperand)))))
classifyParameters state params_ return_on_stack = do
  let f st v = do
        Tuple st' asm_v <- convertVal st v
        t <- Tacky.typeOfVal st.symbols v
        Right (Tuple st' (Tuple t asm_v))
  Tuple state' typed_vals <-
    resultFold (\(Tuple s acc) v -> do
      Tuple s' tv <- f s v
      Right (Tuple s' (acc <> (tv : Nil)))
    ) (Tuple state Nil) params_
  classifyParamsHelper state' typed_vals return_on_stack

classifyParamTypes :: CodegenState -> List Types.CType -> Boolean
  -> Either CompilerError (Tuple CodegenState (List AsmReg))
classifyParamTypes state type_list return_on_stack = do
  let f t =
        if TypeUtils.isScalar t then Tuple t (Pseudo "dummy")
        else Tuple t (PseudoMem "dummy" 0)
  Tuple state' (Tuple ints (Tuple dbls _)) <-
    classifyParamsHelper state (map f type_list) return_on_stack
  let int_regs = ListUtil.take (length ints) intParamPassingRegs
  let dbl_regs = ListUtil.take (length dbls) dblParamPassingRegs
  Right (Tuple state' (int_regs <> dbl_regs))

classifyReturnHelper :: CodegenState -> Types.CType -> AsmOperand
  -> Either CompilerError (Tuple CodegenState (Tuple (List (Tuple AsmType AsmOperand)) (Tuple (List AsmOperand) Boolean)))
classifyReturnHelper state ret_type asm_retval =
  case ret_type of
    Types.Structure tag -> do
      Tuple state' classes <- classifyStructure tag state
      var_name <- case asm_retval of
        PseudoMem n 0 -> Right n
        _ -> Left (InternalError "Internal error: invalid assembly operand for structure type")
      case classes of
        Nil -> Left (InternalError "Internal error: empty classification for structure")
        first : _ | first == Mem -> Right (Tuple state' (Tuple Nil (Tuple Nil true)))
        _ -> do
          result_ <- resultFold (\{ i, ints, dbls } cls -> do
            let eb_operand = PseudoMem var_name (i * 8)
            case cls of
              SSE -> Right { i: i + 1, ints, dbls: dbls <> (eb_operand : Nil) }
              Integer -> do
                sz <- TypeUtils.getSize state.typeTable ret_type
                let eightbyte_type = getEightbyteType i (bigIntToInt sz)
                Right { i: i + 1, ints: ints <> (Tuple eightbyte_type eb_operand : Nil), dbls }
              Mem -> Left (InternalError "Internal error: found eightbyte in Mem class, but first eighbyte wasn't Mem")
            ) { i: 0, ints: Nil, dbls: Nil } classes
          Right (Tuple state' (Tuple result_.ints (Tuple result_.dbls false)))
    Types.Double -> Right (Tuple state (Tuple Nil (Tuple (asm_retval : Nil) false)))
    t -> do
      ct <- convertType state.typeTable t
      let typed_operand = Tuple ct asm_retval
      Right (Tuple state (Tuple (typed_operand : Nil) (Tuple Nil false)))

classifyReturnValue :: CodegenState -> TackyVal
  -> Either CompilerError (Tuple CodegenState (Tuple (List (Tuple AsmType AsmOperand)) (Tuple (List AsmOperand) Boolean)))
classifyReturnValue state retval = do
  Tuple state' asm_v <- convertVal state retval
  retType <- Tacky.typeOfVal state.symbols retval
  classifyReturnHelper state' retType asm_v

classifyReturnType :: CodegenState -> Types.CType
  -> Either CompilerError (Tuple CodegenState (Tuple (List AsmReg) Boolean))
classifyReturnType state = case _ of
  Types.Void -> Right (Tuple state (Tuple Nil false))
  t -> do
    let asm_val =
          if TypeUtils.isScalar t then Pseudo "dummy"
          else PseudoMem "dummy" 0
    Tuple state' (Tuple ints (Tuple dbls return_on_stack)) <- classifyReturnHelper state t asm_val
    if return_on_stack then Right (Tuple state' (Tuple (AX : Nil) true))
    else do
      let int_regs = ListUtil.take (length ints) (AX : DX : Nil)
      let dbl_regs = ListUtil.take (length dbls) (XMM0 : XMM1 : Nil)
      Right (Tuple state' (Tuple (int_regs <> dbl_regs) false))

convertFunctionCall :: CodegenState -> String -> List TackyVal -> Maybe TackyVal
  -> Either CompilerError (Tuple CodegenState (List AsmInstruction))
convertFunctionCall state f args dst = do
  Tuple state' (Tuple int_retvals (Tuple dbl_retvals return_on_stack)) <-
    case dst of
      Just d -> classifyReturnValue state d
      Nothing -> Right (Tuple state (Tuple Nil (Tuple Nil false)))

  Tuple load_dst_instruction first_intreg_idx <-
    if return_on_stack then
      case dst of
        Just d -> do
          Tuple _ asm_d2 <- convertVal state' d
          Right (Tuple (Lea asm_d2 (Reg DI) : Nil) 1)
        Nothing -> Left (InternalError "Internal error: return_on_stack but no dst")
    else Right (Tuple Nil 0)

  Tuple state'' (Tuple int_reg_args (Tuple dbl_reg_args stack_args)) <-
    classifyParameters state' args return_on_stack

  let stack_padding = if length stack_args `mod` 2 == 0 then 0 else 8
  let alignment_instruction =
        if stack_padding == 0 then Nil
        else Binary { op: Sub, t: Quadword, src: Imm (BigInt.fromInt stack_padding), dst: Reg SP } : Nil

  let instructions = load_dst_instruction <> alignment_instruction

  int_reg_instructions <-
    resultTraverse (\(Tuple idx (Tuple arg_t arg)) -> do
      r <- case List.index intParamPassingRegs (idx + first_intreg_idx) of
        Just r_ -> Right r_
        Nothing -> Left (InternalError "Internal error: int register index out of bounds")
      case arg_t of
        ByteArray { size } -> copyBytesToReg arg r size
        _ -> Right (Mov arg_t arg (Reg r) : Nil)
    ) (mapWithIndex (\i x -> Tuple i x) int_reg_args)

  let instructions2 = instructions <> concat int_reg_instructions

  dbl_reg_instructions <-
    resultTraverse (\(Tuple idx arg) -> do
      r <- case List.index dblParamPassingRegs idx of
        Just r_ -> Right r_
        Nothing -> Left (InternalError "Internal error: dbl register index out of bounds")
      Right (Mov AsmDouble arg (Reg r))
    ) (mapWithIndex (\i x -> Tuple i x) dbl_reg_args)

  let instructions3 = instructions2 <> dbl_reg_instructions

  stack_instructions <-
    resultTraverse (\(Tuple arg_t arg) ->
      case arg_t of
        Quadword -> Right (Push arg : Nil)
        AsmDouble -> Right (Push arg : Nil)
        _ | isImmOrReg arg -> Right (Push arg : Nil)
        ByteArray { size } -> do
          copyInstrs <- copyBytes arg (Memory SP 0) size
          Right (Binary { op: Sub, t: Quadword, src: Imm (BigInt.fromInt 8), dst: Reg SP } : copyInstrs)
        _ -> Right (Mov arg_t arg (Reg AX) : Push (Reg AX) : Nil)
    ) stack_args

  let instructions4 = instructions3 <> concat (reverse stack_instructions)
  let instructions5 = instructions4 <> (Call f : Nil)

  let bytes_to_remove = (8 * length stack_args) + stack_padding
  let dealloc =
        if bytes_to_remove == 0 then Nil
        else Binary { op: Add, t: Quadword, src: Imm (BigInt.fromInt bytes_to_remove), dst: Reg SP } : Nil

  let instructions6 = instructions5 <> dealloc

  let int_ret_regs = AX : DX : Nil
  let dbl_ret_regs = XMM0 : XMM1 : Nil
  retrieve_result <-
    case dst of
      Just _ | not return_on_stack -> do
        int_results <-
          resultTraverse (\(Tuple i (Tuple t op)) -> do
            r <- case List.index int_ret_regs i of
              Just r_ -> Right r_
              Nothing -> Left (InternalError "Internal error: int return register index out of bounds")
            case t of
              ByteArray { size } -> copyBytesFromReg r op size
              _ -> Right (Mov t (Reg r) op : Nil)
          ) (mapWithIndex (\i x -> Tuple i x) int_retvals)
        dbl_results <-
          resultTraverse (\(Tuple i op) -> do
            r <- case List.index dbl_ret_regs i of
              Just r_ -> Right r_
              Nothing -> Left (InternalError "Internal error: dbl return register index out of bounds")
            Right (Mov AsmDouble (Reg r) op)
          ) (mapWithIndex (\i x -> Tuple i x) dbl_retvals)
        Right (concat int_results <> dbl_results)
      _ -> Right Nil

  Right (Tuple state'' (instructions6 <> retrieve_result))

  where
  isImmOrReg :: AsmOperand -> Boolean
  isImmOrReg (Imm _) = true
  isImmOrReg (Reg _) = true
  isImmOrReg _ = false

convertReturnInstruction :: CodegenState -> Maybe TackyVal
  -> Either CompilerError (Tuple CodegenState (List AsmInstruction))
convertReturnInstruction state = case _ of
  Nothing -> Right (Tuple state (Ret : Nil))
  Just v -> do
    Tuple state' (Tuple int_retvals (Tuple dbl_retvals return_on_stack)) <- classifyReturnValue state v
    if return_on_stack then do
      retType <- Tacky.typeOfVal state.symbols v
      sz <- TypeUtils.getSize state.typeTable retType
      let byte_count = bigIntToInt sz
      let get_ptr = Mov Quadword (Memory BP (-8)) (Reg AX)
      Tuple state'' asm_v <- convertVal state' v
      copy_into_ptr <- copyBytes asm_v (Memory AX 0) byte_count
      Right (Tuple state'' ((get_ptr : copy_into_ptr) <> (Ret : Nil)))
    else do
      let state'' = state'
      return_ints <-
        resultTraverse (\(Tuple i (Tuple t op)) -> do
          dst_reg <- case i of
            0 -> Right AX
            1 -> Right DX
            _ -> Left (InternalError "Internal error: int return register index out of bounds")
          case t of
            ByteArray { size } -> copyBytesToReg op dst_reg size
            _ -> Right (Mov t op (Reg dst_reg) : Nil)
        ) (mapWithIndex (\i x -> Tuple i x) int_retvals)
      return_dbls <-
        resultTraverse (\(Tuple i op) -> do
          r <- case i of
            0 -> Right XMM0
            1 -> Right XMM1
            _ -> Left (InternalError "Internal error: dbl return register index out of bounds")
          Right (Mov AsmDouble op (Reg r))
        ) (mapWithIndex (\i x -> Tuple i x) dbl_retvals)
      Right (Tuple state'' (concat return_ints <> return_dbls <> (Ret : Nil)))

convertInstruction :: CodegenState -> TackyInstruction
  -> Either CompilerError (Tuple CodegenState (List AsmInstruction))
convertInstruction state = case _ of
  Copy { src, dst } -> do
    srcType <- Tacky.typeOfVal state.symbols src
    if TypeUtils.isScalar srcType then do
      t <- asmType state.typeTable state.symbols src
      Tuple state' asm_src <- convertVal state src
      Tuple state'' asm_dst <- convertVal state' dst
      Right (Tuple state'' (Mov t asm_src asm_dst : Nil))
    else do
      Tuple state' asm_src <- convertVal state src
      Tuple state'' asm_dst <- convertVal state' dst
      sz <- TypeUtils.getSize state.typeTable srcType
      let byte_count = bigIntToInt sz
      instrs <- copyBytes asm_src asm_dst byte_count
      Right (Tuple state'' instrs)

  Return maybe_val -> convertReturnInstruction state maybe_val

  TUnary { op: Tacky.Not, src, dst } -> do
    src_t <- asmType state.typeTable state.symbols src
    dst_t <- asmType state.typeTable state.symbols dst
    Tuple state' asm_src <- convertVal state src
    Tuple state'' asm_dst <- convertVal state' dst
    if src_t == AsmDouble then
      Right (Tuple state''
        ( Binary { op: Xor, t: AsmDouble, src: Reg XMM0, dst: Reg XMM0 }
        : Cmp src_t asm_src (Reg XMM0)
        : Mov dst_t zero asm_dst
        : SetCC E asm_dst
        : Nil))
    else
      Right (Tuple state''
        ( Cmp src_t zero asm_src
        : Mov dst_t zero asm_dst
        : SetCC E asm_dst
        : Nil))

  TUnary { op: Negate, src, dst } -> do
    srcType <- Tacky.typeOfVal state.symbols src
    if srcType == Types.Double then do
      Tuple state' asm_src <- convertVal state src
      Tuple state'' asm_dst <- convertVal state' dst
      let Tuple state''' negative_zero = addConstant (Just 16) (-0.0) state''
      Right (Tuple state'''
        ( Mov AsmDouble asm_src asm_dst
        : Binary { op: Xor, t: AsmDouble, src: Data negative_zero 0, dst: asm_dst }
        : Nil))
    else do
      t <- asmType state.typeTable state.symbols src
      asm_op <- convertUnop Negate
      Tuple state' asm_src <- convertVal state src
      Tuple state'' asm_dst <- convertVal state' dst
      Right (Tuple state'' (Mov t asm_src asm_dst : Unary asm_op t asm_dst : Nil))

  TUnary { op, src, dst } -> do
    t <- asmType state.typeTable state.symbols src
    asm_op <- convertUnop op
    Tuple state' asm_src <- convertVal state src
    Tuple state'' asm_dst <- convertVal state' dst
    Right (Tuple state'' (Mov t asm_src asm_dst : Unary asm_op t asm_dst : Nil))

  TBinary { op, src1, src2, dst } -> do
    src_t <- asmType state.typeTable state.symbols src1
    dst_t <- asmType state.typeTable state.symbols dst
    Tuple state' asm_src1 <- convertVal state src1
    Tuple state'' asm_src2 <- convertVal state' src2
    Tuple state''' asm_dst <- convertVal state'' dst
    case op of
      _ | isComparisonOp op -> do
        signed <-
          if src_t == AsmDouble then Right false
          else do
            t <- Tacky.typeOfVal state.symbols src1
            TypeUtils.isSigned t
        cond_code <- convertCondCode signed op
        Right (Tuple state'''
          ( Cmp src_t asm_src2 asm_src1
          : Mov dst_t zero asm_dst
          : SetCC cond_code asm_dst
          : Nil))
      _ | isDivModOp op && src_t /= AsmDouble -> do
        let result_reg = if op == Divide then AX else DX
        src1Type <- Tacky.typeOfVal state.symbols src1
        signed <- TypeUtils.isSigned src1Type
        if signed then
          Right (Tuple state'''
            ( Mov src_t asm_src1 (Reg AX)
            : Cdq src_t
            : Idiv src_t asm_src2
            : Mov src_t (Reg result_reg) asm_dst
            : Nil))
        else
          Right (Tuple state'''
            ( Mov src_t asm_src1 (Reg AX)
            : Mov src_t zero (Reg DX)
            : Div src_t asm_src2
            : Mov src_t (Reg result_reg) asm_dst
            : Nil))
      _ -> do
        asm_op <- convertBinop op
        Right (Tuple state'''
          ( Mov src_t asm_src1 asm_dst
          : Binary { op: asm_op, t: src_t, src: asm_src2, dst: asm_dst }
          : Nil))

  Load loadInfo -> do
    dstType <- Tacky.typeOfVal state.symbols loadInfo.dst
    if TypeUtils.isScalar dstType then do
      Tuple state' asm_src_ptr <- convertVal state loadInfo.src_ptr
      Tuple state'' asm_dst <- convertVal state' loadInfo.dst
      t <- asmType state.typeTable state.symbols loadInfo.dst
      Right (Tuple state'' (Mov Quadword asm_src_ptr (Reg R9) : Mov t (Memory R9 0) asm_dst : Nil))
    else do
      Tuple state' asm_src_ptr <- convertVal state loadInfo.src_ptr
      Tuple state'' asm_dst <- convertVal state' loadInfo.dst
      sz <- TypeUtils.getSize state.typeTable dstType
      let byte_count = bigIntToInt sz
      instrs <- copyBytes (Memory R9 0) asm_dst byte_count
      Right (Tuple state'' (Mov Quadword asm_src_ptr (Reg R9) : instrs))

  Store storeInfo -> do
    srcType <- Tacky.typeOfVal state.symbols storeInfo.src
    if TypeUtils.isScalar srcType then do
      Tuple state' asm_src <- convertVal state storeInfo.src
      t <- asmType state.typeTable state.symbols storeInfo.src
      Tuple state'' asm_dst_ptr <- convertVal state' storeInfo.dst_ptr
      Right (Tuple state'' (Mov Quadword asm_dst_ptr (Reg R9) : Mov t asm_src (Memory R9 0) : Nil))
    else do
      Tuple state' asm_src <- convertVal state storeInfo.src
      Tuple state'' asm_dst_ptr <- convertVal state' storeInfo.dst_ptr
      sz <- TypeUtils.getSize state.typeTable srcType
      let byte_count = bigIntToInt sz
      instrs <- copyBytes asm_src (Memory R9 0) byte_count
      Right (Tuple state'' (Mov Quadword asm_dst_ptr (Reg R9) : instrs))

  GetAddress { src, dst } -> do
    Tuple state' asm_src <- convertVal state src
    Tuple state'' asm_dst <- convertVal state' dst
    Right (Tuple state'' (Lea asm_src asm_dst : Nil))

  Jump target -> Right (Tuple state (Jmp target : Nil))

  JumpIfZero cond target -> do
    t <- asmType state.typeTable state.symbols cond
    Tuple state' asm_cond <- convertVal state cond
    if t == AsmDouble then
      Right (Tuple state'
        ( Binary { op: Xor, t: AsmDouble, src: Reg XMM0, dst: Reg XMM0 }
        : Cmp t asm_cond (Reg XMM0)
        : JmpCC E target
        : Nil))
    else Right (Tuple state' (Cmp t zero asm_cond : JmpCC E target : Nil))

  JumpIfNotZero cond target -> do
    t <- asmType state.typeTable state.symbols cond
    Tuple state' asm_cond <- convertVal state cond
    if t == AsmDouble then
      Right (Tuple state'
        ( Binary { op: Xor, t: AsmDouble, src: Reg XMM0, dst: Reg XMM0 }
        : Cmp t asm_cond (Reg XMM0)
        : JmpCC NE target
        : Nil))
    else Right (Tuple state' (Cmp t zero asm_cond : JmpCC NE target : Nil))

  TLabel l -> Right (Tuple state (Label l : Nil))

  FunCall { f: fname, args, dst } -> convertFunctionCall state fname args dst

  SignExtend { src, dst } -> do
    Tuple state' asm_src <- convertVal state src
    Tuple state'' asm_dst <- convertVal state' dst
    srcT <- asmType state.typeTable state.symbols src
    dstT <- asmType state.typeTable state.symbols dst
    Right (Tuple state'' (Movsx { src_type: srcT, dst_type: dstT, src: asm_src, dst: asm_dst } : Nil))

  Truncate { src, dst } -> do
    Tuple state' asm_src <- convertVal state src
    Tuple state'' asm_dst <- convertVal state' dst
    dstT <- asmType state.typeTable state.symbols dst
    Right (Tuple state'' (Mov dstT asm_src asm_dst : Nil))

  ZeroExtend { src, dst } -> do
    Tuple state' asm_src <- convertVal state src
    Tuple state'' asm_dst <- convertVal state' dst
    srcT <- asmType state.typeTable state.symbols src
    dstT <- asmType state.typeTable state.symbols dst
    Right (Tuple state'' (MovZeroExtend { src_type: srcT, dst_type: dstT, src: asm_src, dst: asm_dst } : Nil))

  IntToDouble { src, dst } -> do
    Tuple state' asm_src <- convertVal state src
    Tuple state'' asm_dst <- convertVal state' dst
    t <- asmType state.typeTable state.symbols src
    if t == Byte then
      Right (Tuple state''
        ( Movsx { src_type: Byte, dst_type: Longword, src: asm_src, dst: Reg R9 }
        : Cvtsi2sd Longword (Reg R9) asm_dst
        : Nil))
    else Right (Tuple state'' (Cvtsi2sd t asm_src asm_dst : Nil))

  DoubleToInt { src, dst } -> do
    Tuple state' asm_src <- convertVal state src
    Tuple state'' asm_dst <- convertVal state' dst
    t <- asmType state.typeTable state.symbols dst
    if t == Byte then
      Right (Tuple state'' (Cvttsd2si Longword asm_src (Reg R9) : Mov Byte (Reg R9) asm_dst : Nil))
    else Right (Tuple state'' (Cvttsd2si t asm_src asm_dst : Nil))

  UIntToDouble { src, dst } -> do
    Tuple state' asm_src <- convertVal state src
    Tuple state'' asm_dst <- convertVal state' dst
    srcType <- Tacky.typeOfVal state.symbols src
    if srcType == Types.UChar then
      Right (Tuple state''
        ( MovZeroExtend { src_type: Byte, dst_type: Longword, src: asm_src, dst: Reg R9 }
        : Cvtsi2sd Longword (Reg R9) asm_dst
        : Nil))
    else if srcType == Types.UInt then
      Right (Tuple state''
        ( MovZeroExtend { src_type: Longword, dst_type: Quadword, src: asm_src, dst: Reg R9 }
        : Cvtsi2sd Quadword (Reg R9) asm_dst
        : Nil))
    else do
      let Tuple state3 out_of_bounds = nextLabel "ulong2dbl.oob" state''
      let Tuple state4 end_lbl = nextLabel "ulong2dbl.end" state3
      let r1 = Reg R8
      let r2 = Reg R9
      Right (Tuple state4
        ( Cmp Quadword zero asm_src
        : JmpCC L out_of_bounds
        : Cvtsi2sd Quadword asm_src asm_dst
        : Jmp end_lbl
        : Label out_of_bounds
        : Mov Quadword asm_src r1
        : Mov Quadword r1 r2
        : Unary Shr Quadword r2
        : Binary { op: And, t: Quadword, src: Imm (BigInt.fromInt 1), dst: r1 }
        : Binary { op: Or, t: Quadword, src: r1, dst: r2 }
        : Cvtsi2sd Quadword r2 asm_dst
        : Binary { op: Add, t: AsmDouble, src: asm_dst, dst: asm_dst }
        : Label end_lbl
        : Nil))

  DoubleToUInt { src, dst } -> do
    Tuple state' asm_src <- convertVal state src
    Tuple state'' asm_dst <- convertVal state' dst
    dstType <- Tacky.typeOfVal state.symbols dst
    if dstType == Types.UChar then
      Right (Tuple state'' (Cvttsd2si Longword asm_src (Reg R9) : Mov Byte (Reg R9) asm_dst : Nil))
    else if dstType == Types.UInt then
      Right (Tuple state''
        ( Cvttsd2si Quadword asm_src (Reg R9)
        : Mov Longword (Reg R9) asm_dst
        : Nil))
    else do
      let Tuple state3 out_of_bounds = nextLabel "dbl2ulong.oob" state''
      let Tuple state4 end_lbl = nextLabel "dbl2ulong.end" state3
      let Tuple state5 upper_bound = addConstant Nothing 9223372036854775808.0 state4
      let upper_bound_as_int = Imm (BigInt.fromInt int64MinValue)
      let r = Reg R9
      let x = Reg XMM7
      Right (Tuple state5
        ( Cmp AsmDouble (Data upper_bound 0) asm_src
        : JmpCC AE out_of_bounds
        : Cvttsd2si Quadword asm_src asm_dst
        : Jmp end_lbl
        : Label out_of_bounds
        : Mov AsmDouble asm_src x
        : Binary { op: Sub, t: AsmDouble, src: Data upper_bound 0, dst: x }
        : Cvttsd2si Quadword x asm_dst
        : Mov Quadword upper_bound_as_int r
        : Binary { op: Add, t: Quadword, src: r, dst: asm_dst }
        : Label end_lbl
        : Nil))

  CopyToOffset { src, dst, offset } -> do
    srcType <- Tacky.typeOfVal state.symbols src
    if TypeUtils.isScalar srcType then do
      Tuple state' asm_src <- convertVal state src
      t <- asmType state.typeTable state.symbols src
      Right (Tuple state' (Mov t asm_src (PseudoMem dst offset) : Nil))
    else do
      Tuple state' asm_src <- convertVal state src
      let asm_dst = PseudoMem dst offset
      sz <- TypeUtils.getSize state.typeTable srcType
      let byte_count = bigIntToInt sz
      instrs <- copyBytes asm_src asm_dst byte_count
      Right (Tuple state' instrs)

  CopyFromOffset { src, dst, offset } -> do
    dstType <- Tacky.typeOfVal state.symbols dst
    if TypeUtils.isScalar dstType then do
      Tuple state' asm_dst <- convertVal state dst
      t <- asmType state.typeTable state.symbols dst
      Right (Tuple state' (Mov t (PseudoMem src offset) asm_dst : Nil))
    else do
      let asm_src = PseudoMem src offset
      Tuple state' asm_dst <- convertVal state dst
      sz <- TypeUtils.getSize state.typeTable dstType
      let byte_count = bigIntToInt sz
      instrs <- copyBytes asm_src asm_dst byte_count
      Right (Tuple state' instrs)

  AddPtr { ptr, index: Constant (Const.ConstLong c), scale, dst } -> do
    let i = bigIntToInt c
    Tuple state' asm_ptr <- convertVal state ptr
    Tuple state'' asm_dst <- convertVal state' dst
    Right (Tuple state''
      ( Mov Quadword asm_ptr (Reg R9)
      : Lea (Memory R9 (i * scale)) asm_dst
      : Nil))

  AddPtr { ptr, index, scale, dst } -> do
    Tuple state' asm_ptr <- convertVal state ptr
    Tuple state'' asm_index <- convertVal state' index
    Tuple state''' asm_dst <- convertVal state'' dst
    if scale == 1 || scale == 2 || scale == 4 || scale == 8 then
      Right (Tuple state'''
        ( Mov Quadword asm_ptr (Reg R8)
        : Mov Quadword asm_index (Reg R9)
        : Lea (Indexed { baseReg: R8, index: R9, scale }) asm_dst
        : Nil))
    else
      Right (Tuple state'''
        ( Mov Quadword asm_ptr (Reg R8)
        : Mov Quadword asm_index (Reg R9)
        : Binary { op: Mult, t: Quadword, src: Imm (BigInt.fromInt scale), dst: Reg R9 }
        : Lea (Indexed { baseReg: R8, index: R9, scale: 1 }) asm_dst
        : Nil))

  where
  isComparisonOp :: TackyBinaryOperator -> Boolean
  isComparisonOp = case _ of
    Equal -> true
    NotEqual -> true
    GreaterThan -> true
    GreaterOrEqual -> true
    LessThan -> true
    LessOrEqual -> true
    _ -> false

  isDivModOp :: TackyBinaryOperator -> Boolean
  isDivModOp = case _ of
    Divide -> true
    Mod -> true
    _ -> false

passParams :: CodegenState -> List TackyVal -> Boolean
  -> Either CompilerError (Tuple CodegenState (List AsmInstruction))
passParams state param_list return_on_stack = do
  Tuple state' (Tuple int_reg_params (Tuple dbl_reg_params stack_params)) <-
    classifyParameters state param_list return_on_stack

  Tuple copy_dst_ptr remaining_int_regs <-
    if return_on_stack then do
      rest <- case intParamPassingRegs of
        _ : rest_ -> Right rest_
        Nil -> Left (InternalError "Internal error: empty intParamPassingRegs")
      Right (Tuple (Mov Quadword (Reg DI) (Memory BP (-8)) : Nil) rest)
    else Right (Tuple Nil intParamPassingRegs)

  int_reg_instructions <-
    resultTraverse (\(Tuple idx (Tuple param_t param)) -> do
      r <- case List.index remaining_int_regs idx of
        Just r_ -> Right r_
        Nothing -> Left (InternalError "Internal error: int register index out of bounds")
      case param_t of
        ByteArray { size } -> copyBytesFromReg r param size
        _ -> Right (Mov param_t (Reg r) param : Nil)
    ) (mapWithIndex (\i x -> Tuple i x) int_reg_params)

  dbl_reg_instructions <-
    resultTraverse (\(Tuple idx param) -> do
      r <- case List.index dblParamPassingRegs idx of
        Just r_ -> Right r_
        Nothing -> Left (InternalError "Internal error: dbl register index out of bounds")
      Right (Mov AsmDouble (Reg r) param)
    ) (mapWithIndex (\i x -> Tuple i x) dbl_reg_params)

  stack_instructions <-
    resultTraverse (\(Tuple idx (Tuple param_t param)) -> do
      let stk = Memory BP (16 + (8 * idx))
      case param_t of
        ByteArray { size } -> copyBytes stk param size
        _ -> Right (Mov param_t stk param : Nil)
    ) (mapWithIndex (\i x -> Tuple i x) stack_params)

  Right (Tuple state'
    ( copy_dst_ptr
    <> concat int_reg_instructions
    <> dbl_reg_instructions
    <> concat stack_instructions))

returnsOnStack :: CodegenState -> String -> Either CompilerError (Tuple CodegenState Boolean)
returnsOnStack state fn_name = do
  entry <- Symbols.get fn_name state.symbols
  case entry.symType of
    Types.FunType _ (Types.Structure tag) -> do
      Tuple state' classes <- classifyStructure tag state
      Right (Tuple state' (case classes of
        Mem : _ -> true
        _ -> false))
    Types.FunType _ _ -> Right (Tuple state false)
    _ -> Left (InternalError "Internal error: not a function name")

getVarAlignment :: TypeTable.TypeTableMap -> Types.CType -> Either CompilerError Int
getVarAlignment tt = case _ of
  t@(Types.Array _ _) -> do
    sz <- TypeUtils.getSize tt t
    if bigIntToInt sz >= 16 then Right 16
    else TypeUtils.getAlignment tt t
  t -> TypeUtils.getAlignment tt t

convertVarType :: TypeTable.TypeTableMap -> Types.CType -> Either CompilerError AsmType
convertVarType tt = case _ of
  t@(Types.Array _ _) -> do
    sz <- TypeUtils.getSize tt t
    align <- getVarAlignment tt t
    Right (ByteArray { size: bigIntToInt sz, alignment: align })
  other -> convertType tt other

convertTopLevel :: CodegenState -> TackyTopLevel
  -> Either CompilerError (Tuple CodegenState AsmTopLevel)
convertTopLevel state = case _ of
  TFunction { name, isGlobal, body, paramList: params_ } -> do
    Tuple state' return_on_stack <- returnsOnStack state name
    let params_as_tacky = map (\n -> Var n) params_
    Tuple state'' param_instructions <- passParams state' params_as_tacky return_on_stack
    Tuple state''' body_instructions <-
      resultFold (\(Tuple s acc) instr -> do
        Tuple s' instrs <- convertInstruction s instr
        Right (Tuple s' (acc <> instrs))
      ) (Tuple state'' Nil) body
    let instructions = param_instructions <> body_instructions
    Right (Tuple state''' (Function { name, isGlobal, instructions }))
  TStaticVariable { name, isGlobal, t, init } -> do
    align <- getVarAlignment state.typeTable t
    Right (Tuple state (StaticVariable { name, isGlobal, alignment: align, init }))
  TStaticConstant { name, t, init } -> do
    align <- TypeUtils.getAlignment state.typeTable t
    Right (Tuple state (StaticConstant { name, alignment: align, init }))

convertConstant :: AsmSymbolTableMap -> Tuple String (Tuple String Int) -> Tuple AsmSymbolTableMap AsmTopLevel
convertConstant asmSymbols (Tuple key (Tuple name alignment)) =
  let dbl = int64BitsToDoubleImpl key
      asmSymbols' = AssemblySymbols.addConstant name AsmDouble asmSymbols
  in Tuple asmSymbols' (StaticConstant { name, alignment, init: DoubleInit dbl })

convertSymbol :: CodegenState -> String -> Symbols.SymbolEntry -> Either CompilerError CodegenState
convertSymbol state name entry =
  case entry of
    { symType: Types.FunType paramTypes retType, attrs: Symbols.FunAttr { defined } }
      | (TypeUtils.isComplete state.typeTable retType || retType == Types.Void)
        && all (TypeUtils.isComplete state.typeTable) paramTypes -> do
      Tuple state' (Tuple ret_regs return_on_stack) <- classifyReturnType state retType
      Tuple state'' param_regs <- classifyParamTypes state' paramTypes return_on_stack
      Tuple state''' ros <- returnsOnStack state'' name
      let asmSymbols' = AssemblySymbols.addFun name defined ros param_regs ret_regs state'''.asmSymbols
      Right (state''' { asmSymbols = asmSymbols' })
    { symType: Types.FunType _ _, attrs: Symbols.FunAttr { defined } } -> do
      -- assert (not defined) -- skip assertion
      let _ = defined
      let asmSymbols' = AssemblySymbols.addFun name false false Nil Nil state.asmSymbols
      Right (state { asmSymbols = asmSymbols' })
    { symType: t, attrs: Symbols.ConstAttr _ } -> do
      ct <- convertType state.typeTable t
      let asmSymbols' = AssemblySymbols.addConstant name ct state.asmSymbols
      Right (state { asmSymbols = asmSymbols' })
    { symType: t, attrs: Symbols.StaticAttr _ }
      | not (TypeUtils.isComplete state.typeTable t) -> do
      let asmSymbols' = AssemblySymbols.addVar name Byte true state.asmSymbols
      Right (state { asmSymbols = asmSymbols' })
    { symType: t, attrs: Symbols.StaticAttr _ } -> do
      vt <- convertVarType state.typeTable t
      let asmSymbols' = AssemblySymbols.addVar name vt true state.asmSymbols
      Right (state { asmSymbols = asmSymbols' })
    { symType: t } -> do
      vt <- convertVarType state.typeTable t
      let asmSymbols' = AssemblySymbols.addVar name vt false state.asmSymbols
      Right (state { asmSymbols = asmSymbols' })

gen :: UniqueIds.Counter -> TypeTable.TypeTableMap -> Symbols.SymbolTableMap -> AsmSymbolTableMap -> TackyProgram
  -> Either CompilerError (Tuple UniqueIds.Counter (Tuple AsmSymbolTableMap AsmProgram))
gen counter tt symbols asmSymbols (TProgram top_levels) = do
  let state = { counter, constants: Map.empty, classifiedStructures: Map.empty, typeTable: tt, symbols, asmSymbols }
  Tuple state' tls <-
    resultFold (\(Tuple s acc) tl -> do
      Tuple s' tl' <- convertTopLevel s tl
      Right (Tuple s' (acc <> (tl' : Nil)))
    ) (Tuple state Nil) top_levels
  state'' <-
    resultFold (\s (Tuple name entry_) -> convertSymbol s name entry_) state' (Symbols.bindings state'.symbols)
  let Tuple asmSymbols' constants_list =
        foldl (\(Tuple asym acc) entry_ ->
          let Tuple asym' tl = convertConstant asym entry_
          in Tuple asym' (acc <> (tl : Nil))
        ) (Tuple state''.asmSymbols Nil) ((Map.toUnfoldable state''.constants) :: List _)
  let prog = Program (constants_list <> tls)
  Right (Tuple state''.counter (Tuple asmSymbols' prog))
