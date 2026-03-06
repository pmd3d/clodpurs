module TackyGen where

import Prelude

import Ast (UnaryOperator(..), BinaryOperator(..))
import Bytes as Bytes
import CompilerError (CompilerError(..))
import Const as Const
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), concatMap, (:))
import Data.Map as Map
import Data.Char (fromCharCode)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Initializers as Initializers
import ResultUtil (resultFold, resultFold2)
import Symbols as Symbols
import Tacky as T
import TypeTable as TypeTable
import TypedAst as Ast
import Types as Types
import UniqueIds as UniqueIds
import TypeUtils as TypeUtils

breakLabel :: String -> String
breakLabel id = "break." <> id

continueLabel :: String -> String
continueLabel id = "continue." <> id

-- use this as the "result" of void expressions that don't return a result
dummyOperand :: T.TackyVal
dummyOperand = T.Constant Const.intZero

createTmp :: UniqueIds.Counter -> Symbols.SymbolTableMap -> Types.CType
          -> { counter :: UniqueIds.Counter, st :: Symbols.SymbolTableMap, name :: String }
createTmp counter st t =
  let (Tuple counter' name) = UniqueIds.makeTemporary counter
      st' = Symbols.addAutomaticVar name t st
  in { counter: counter', st: st', name: name }

getPtrScale :: TypeTable.TypeTableMap -> Types.CType -> Either CompilerError Int
getPtrScale tt = case _ of
  Types.Pointer referenced -> do
    size <- TypeUtils.getSize tt referenced
    pure (bigIntToInt size)
  t ->
    Left (InternalError ("tried to get scale of non-pointer type: " <> show t))

getMemberOffset :: TypeTable.TypeTableMap -> String -> Types.CType -> Either CompilerError Int
getMemberOffset tt memberName = case _ of
  Types.Structure tag ->
    case TypeTable.tryFind tag tt of
      Just entry ->
        case Map.lookup memberName entry.members of
          Just m -> Right m.offset
          Nothing ->
            Left (InternalError ("failed to find member " <> memberName <> " in structure " <> tag))
      Nothing ->
        Left (InternalError ("failed to find structure " <> tag <> " in type table"))
  t ->
    Left (InternalError ("tried to get offset of member " <> memberName <> " within non-structure type " <> show t))

getMemberPointerOffset :: TypeTable.TypeTableMap -> String -> Types.CType -> Either CompilerError Int
getMemberPointerOffset tt memberName = case _ of
  Types.Pointer t -> getMemberOffset tt memberName t
  t ->
    Left (InternalError ("trying to get member through pointer but " <> show t <> " is not a pointer type"))

convertOp :: UnaryOperator -> T.TackyUnaryOperator
convertOp = case _ of
  Complement -> T.Complement
  Negate -> T.Negate
  Not -> T.Not

convertBinop :: BinaryOperator -> Either CompilerError T.TackyBinaryOperator
convertBinop = case _ of
  Add -> Right T.Add
  Subtract -> Right T.Subtract
  Multiply -> Right T.Multiply
  Divide -> Right T.Divide
  Mod -> Right T.Mod
  Equal -> Right T.Equal
  NotEqual -> Right T.NotEqual
  LessThan -> Right T.LessThan
  LessOrEqual -> Right T.LessOrEqual
  GreaterThan -> Right T.GreaterThan
  GreaterOrEqual -> Right T.GreaterOrEqual
  And -> Left (InternalError "cannot convert these directly to TACKY binops")
  Or -> Left (InternalError "cannot convert these directly to TACKY binops")

evalSize :: TypeTable.TypeTableMap -> Types.CType -> Either CompilerError T.TackyVal
evalSize tt t =
  map (\size -> T.Constant (Const.ConstULong size)) (TypeUtils.getSize tt t)

-- an expression result that may or may not be lvalue converted
data ExpResult
  = PlainOperand T.TackyVal
  | DereferencedPointer T.TackyVal
  | SubObject String Int

type ExpEmitResult =
  { counter :: UniqueIds.Counter
  , st :: Symbols.SymbolTableMap
  , instructions :: List T.TackyInstruction
  , result :: ExpResult
  }

type ValEmitResult =
  { counter :: UniqueIds.Counter
  , st :: Symbols.SymbolTableMap
  , instructions :: List T.TackyInstruction
  , result :: T.TackyVal
  }

type StmtEmitResult =
  { counter :: UniqueIds.Counter
  , st :: Symbols.SymbolTableMap
  , instructions :: List T.TackyInstruction
  }

emitTackyForExp :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                -> Ast.Exp -> Either CompilerError ExpEmitResult
emitTackyForExp counter st tt exp =
  let e = exp.e
      t = exp.t
  in case e of
    Ast.Constant c -> Right { counter, st, instructions: Nil, result: PlainOperand (T.Constant c) }
    Ast.Var v -> Right { counter, st, instructions: Nil, result: PlainOperand (T.Var v) }
    Ast.EString s ->
      let (Tuple counter' (Tuple str_id st')) = Symbols.addStringWithCounter counter s st
      in Right { counter: counter', st: st', instructions: Nil, result: PlainOperand (T.Var str_id) }
    Ast.Cast target_type inner ->
      emitCastExpression counter st tt target_type inner
    Ast.Unary op inner -> emitUnaryExpression counter st tt t op inner
    Ast.Binary And e1 e2 -> emitAndExpression counter st tt e1 e2
    Ast.Binary Or e1 e2 -> emitOrExpression counter st tt e1 e2
    Ast.Binary Add e1 e2
      | TypeUtils.isPointer t -> emitPointerAddition counter st tt t e1 e2
    Ast.Binary Subtract ptr index
      | TypeUtils.isPointer t -> emitSubtractionFromPointer counter st tt t ptr index
    Ast.Binary Subtract e1 e2
      | TypeUtils.isPointer e1.t -> emitPointerDiff counter st tt t e1 e2
    Ast.Binary op e1 e2 -> emitBinaryExpression counter st tt t op e1 e2
    Ast.Assignment lhs rhs -> emitAssignment counter st tt lhs rhs
    Ast.Conditional condition then_result else_result ->
      emitConditionalExpression counter st tt t condition then_result else_result
    Ast.FunCall f args -> emitFunCall counter st tt t f args
    Ast.Dereference inner -> emitDereference counter st tt inner
    Ast.AddrOf inner -> emitAddrOf counter st tt t inner
    Ast.Subscript ptr index ->
      emitSubscript counter st tt t ptr index
    Ast.SizeOfT szt -> do
      sz <- evalSize tt szt
      pure { counter, st, instructions: Nil, result: PlainOperand sz }
    Ast.SizeOf inner -> do
      sz <- evalSize tt inner.t
      pure { counter, st, instructions: Nil, result: PlainOperand sz }
    Ast.Dot strct mbr ->
      emitDotOperator counter st tt t strct mbr
    Ast.Arrow strct mbr ->
      emitArrowOperator counter st tt t strct mbr

emitUnaryExpression :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                    -> Types.CType -> UnaryOperator -> Ast.Exp
                    -> Either CompilerError ExpEmitResult
emitUnaryExpression counter st tt t op inner = do
  r <- emitTackyAndConvert counter st tt inner
  let tmp = createTmp r.counter r.st t
  let dst = T.Var tmp.name
  let tacky_op = convertOp op
  let instructions =
        r.instructions <> (T.TUnary { op: tacky_op, src: r.result, dst: dst } : Nil)
  pure { counter: tmp.counter, st: tmp.st, instructions, result: PlainOperand dst }

emitCastExpression :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                   -> Types.CType -> Ast.Exp -> Either CompilerError ExpEmitResult
emitCastExpression counter st tt target_type inner = do
  r <- emitTackyAndConvert counter st tt inner
  let inner_type = inner.t
  if inner_type == target_type || target_type == Types.Void then
    pure { counter: r.counter, st: r.st, instructions: r.instructions, result: PlainOperand r.result }
  else do
    let tmp = createTmp r.counter r.st target_type
    let dst = T.Var tmp.name
    cast_instruction <- case target_type, inner_type of
      Types.Double, _ -> do
        signed <- TypeUtils.isSigned inner_type
        if signed then pure (T.IntToDouble { src: r.result, dst: dst })
        else pure (T.UIntToDouble { src: r.result, dst: dst })
      _, Types.Double -> do
        signed <- TypeUtils.isSigned target_type
        if signed then pure (T.DoubleToInt { src: r.result, dst: dst })
        else pure (T.DoubleToUInt { src: r.result, dst: dst })
      _, _ -> do
        targetSize <- TypeUtils.getSize tt target_type
        innerSize <- TypeUtils.getSize tt inner_type
        if targetSize == innerSize then pure (T.Copy { src: r.result, dst: dst })
        else if targetSize < innerSize then pure (T.Truncate { src: r.result, dst: dst })
        else do
          signed <- TypeUtils.isSigned inner_type
          if signed then pure (T.SignExtend { src: r.result, dst: dst })
          else pure (T.ZeroExtend { src: r.result, dst: dst })
    let instructions = r.instructions <> (cast_instruction : Nil)
    pure { counter: tmp.counter, st: tmp.st, instructions, result: PlainOperand dst }

emitPointerAddition :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                    -> Types.CType -> Ast.Exp -> Ast.Exp -> Either CompilerError ExpEmitResult
emitPointerAddition counter st tt t e1 e2 = do
  r1 <- emitTackyAndConvert counter st tt e1
  r2 <- emitTackyAndConvert r1.counter r1.st tt e2
  let tmp = createTmp r2.counter r2.st t
  let dst = T.Var tmp.name
  let ptr_and_index = if t == e1.t then { ptr: r1.result, index: r2.result } else { ptr: r2.result, index: r1.result }
  scale <- getPtrScale tt t
  let instructions =
        r1.instructions <> r2.instructions
        <> (T.AddPtr { ptr: ptr_and_index.ptr, index: ptr_and_index.index, scale: scale, dst: dst } : Nil)
  pure { counter: tmp.counter, st: tmp.st, instructions, result: PlainOperand dst }

emitSubscript :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
              -> Types.CType -> Ast.Exp -> Ast.Exp -> Either CompilerError ExpEmitResult
emitSubscript counter st tt t e1 e2 = do
  r <- emitPointerAddition counter st tt (Types.Pointer t) e1 e2
  case r.result of
    PlainOperand dst -> pure { counter: r.counter, st: r.st, instructions: r.instructions, result: DereferencedPointer dst }
    _ -> Left (InternalError "expected result of pointer addition to be lvalue converted")

emitSubtractionFromPointer :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                           -> Types.CType -> Ast.Exp -> Ast.Exp -> Either CompilerError ExpEmitResult
emitSubtractionFromPointer counter st tt t ptr_e idx_e = do
  r1 <- emitTackyAndConvert counter st tt ptr_e
  r2 <- emitTackyAndConvert r1.counter r1.st tt idx_e
  let tmp1 = createTmp r2.counter r2.st t
  let dst = T.Var tmp1.name
  let tmp2 = createTmp tmp1.counter tmp1.st Types.Long
  let negated_index = T.Var tmp2.name
  scale <- getPtrScale tt t
  pure { counter: tmp2.counter, st: tmp2.st
       , instructions:
           r1.instructions
           <> r2.instructions
           <> (T.TUnary { op: T.Negate, src: r2.result, dst: negated_index }
               : T.AddPtr { ptr: r1.result, index: negated_index, scale: scale, dst: dst }
               : Nil)
       , result: PlainOperand dst }

emitPointerDiff :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                -> Types.CType -> Ast.Exp -> Ast.Exp -> Either CompilerError ExpEmitResult
emitPointerDiff counter st tt t e1 e2 = do
  r1 <- emitTackyAndConvert counter st tt e1
  r2 <- emitTackyAndConvert r1.counter r1.st tt e2
  let tmp1 = createTmp r2.counter r2.st Types.Long
  let ptr_diff = T.Var tmp1.name
  let tmp2 = createTmp tmp1.counter tmp1.st t
  let dst = T.Var tmp2.name
  ptrScale <- getPtrScale tt e1.t
  let scale = T.Constant (Const.ConstLong (BigInt.fromInt ptrScale))
  pure { counter: tmp2.counter, st: tmp2.st
       , instructions:
           r1.instructions
           <> r2.instructions
           <> (T.TBinary { op: T.Subtract, src1: r1.result, src2: r2.result, dst: ptr_diff }
               : T.TBinary { op: T.Divide, src1: ptr_diff, src2: scale, dst: dst }
               : Nil)
       , result: PlainOperand dst }

emitBinaryExpression :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                     -> Types.CType -> BinaryOperator -> Ast.Exp -> Ast.Exp
                     -> Either CompilerError ExpEmitResult
emitBinaryExpression counter st tt t op e1 e2 = do
  r1 <- emitTackyAndConvert counter st tt e1
  r2 <- emitTackyAndConvert r1.counter r1.st tt e2
  let tmp = createTmp r2.counter r2.st t
  let dst = T.Var tmp.name
  tacky_op <- convertBinop op
  let instructions =
        r1.instructions
        <> r2.instructions
        <> (T.TBinary { op: tacky_op, src1: r1.result, src2: r2.result, dst: dst } : Nil)
  pure { counter: tmp.counter, st: tmp.st, instructions, result: PlainOperand dst }

emitAndExpression :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                  -> Ast.Exp -> Ast.Exp -> Either CompilerError ExpEmitResult
emitAndExpression counter st tt e1 e2 = do
  r1 <- emitTackyAndConvert counter st tt e1
  r2 <- emitTackyAndConvert r1.counter r1.st tt e2
  let (Tuple c3 false_label) = UniqueIds.makeLabel "and_false" r2.counter
  let (Tuple c4 end_label) = UniqueIds.makeLabel "and_end" c3
  let tmp = createTmp c4 r2.st Types.Int
  let dst = T.Var tmp.name
  let instructions =
        r1.instructions
        <> (T.JumpIfZero r1.result false_label : Nil)
        <> r2.instructions
        <> (T.JumpIfZero r2.result false_label
            : T.Copy { src: T.Constant Const.intOne, dst: dst }
            : T.Jump end_label
            : T.TLabel false_label
            : T.Copy { src: T.Constant Const.intZero, dst: dst }
            : T.TLabel end_label
            : Nil)
  pure { counter: tmp.counter, st: tmp.st, instructions, result: PlainOperand dst }

emitOrExpression :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                 -> Ast.Exp -> Ast.Exp -> Either CompilerError ExpEmitResult
emitOrExpression counter st tt e1 e2 = do
  r1 <- emitTackyAndConvert counter st tt e1
  r2 <- emitTackyAndConvert r1.counter r1.st tt e2
  let (Tuple c3 true_label) = UniqueIds.makeLabel "or_true" r2.counter
  let (Tuple c4 end_label) = UniqueIds.makeLabel "or_end" c3
  let tmp = createTmp c4 r2.st Types.Int
  let dst = T.Var tmp.name
  let instructions =
        r1.instructions
        <> (T.JumpIfNotZero r1.result true_label : r2.instructions)
        <> (T.JumpIfNotZero r2.result true_label
            : T.Copy { src: T.Constant Const.intZero, dst: dst }
            : T.Jump end_label
            : T.TLabel true_label
            : T.Copy { src: T.Constant Const.intOne, dst: dst }
            : T.TLabel end_label
            : Nil)
  pure { counter: tmp.counter, st: tmp.st, instructions, result: PlainOperand dst }

emitAssignment :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
               -> Ast.Exp -> Ast.Exp -> Either CompilerError ExpEmitResult
emitAssignment counter st tt lhs rhs = do
  lhs_r <- emitTackyForExp counter st tt lhs
  rhs_r <- emitTackyAndConvert lhs_r.counter lhs_r.st tt rhs
  let instructions = lhs_r.instructions <> rhs_r.instructions
  case lhs_r.result of
    PlainOperand o ->
      pure { counter: rhs_r.counter, st: rhs_r.st
           , instructions: instructions <> (T.Copy { src: rhs_r.result, dst: o } : Nil)
           , result: PlainOperand o }
    DereferencedPointer ptr ->
      pure { counter: rhs_r.counter, st: rhs_r.st
           , instructions: instructions <> (T.Store { src: rhs_r.result, dst_ptr: ptr } : Nil)
           , result: PlainOperand rhs_r.result }
    SubObject baseObj offset ->
      pure { counter: rhs_r.counter, st: rhs_r.st
           , instructions: instructions <> (T.CopyToOffset { src: rhs_r.result, offset: offset, dst: baseObj } : Nil)
           , result: PlainOperand rhs_r.result }

emitConditionalExpression :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                          -> Types.CType -> Ast.Exp -> Ast.Exp -> Ast.Exp
                          -> Either CompilerError ExpEmitResult
emitConditionalExpression counter st tt t condition e1 e2 = do
  cr <- emitTackyAndConvert counter st tt condition
  r1 <- emitTackyAndConvert cr.counter cr.st tt e1
  r2 <- emitTackyAndConvert r1.counter r1.st tt e2
  let (Tuple c4 e2_label) = UniqueIds.makeLabel "conditional_else" r2.counter
  let (Tuple c5 end_label) = UniqueIds.makeLabel "conditional_end" c4
  let dst_info =
        if t == Types.Void then { counter: c5, st: r2.st, dst: dummyOperand }
        else
          let tmp = createTmp c5 r2.st t
          in { counter: tmp.counter, st: tmp.st, dst: T.Var tmp.name }
  let common_instructions =
        cr.instructions <> (T.JumpIfZero cr.result e2_label : r1.instructions)
  let remaining_instructions =
        if t == Types.Void then
          (T.Jump end_label : T.TLabel e2_label : r2.instructions)
          <> (T.TLabel end_label : Nil)
        else
          T.Copy { src: r1.result, dst: dst_info.dst }
          : T.Jump end_label
          : T.TLabel e2_label
          : r2.instructions
          <> (T.Copy { src: r2.result, dst: dst_info.dst } : T.TLabel end_label : Nil)
  pure { counter: dst_info.counter, st: dst_info.st
       , instructions: common_instructions <> remaining_instructions
       , result: PlainOperand dst_info.dst }

emitFunCall :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
            -> Types.CType -> String -> List Ast.Exp -> Either CompilerError ExpEmitResult
emitFunCall counter st tt t f args = do
  let dst_info =
        if t == Types.Void then { counter, st, dst: Nothing }
        else
          let tmp = createTmp counter st t
          in { counter: tmp.counter, st: tmp.st, dst: Just (T.Var tmp.name) }
  arg_result <- resultFold (\(Tuple (Tuple c s) acc) arg -> do
      r <- emitTackyAndConvert c s tt arg
      pure (Tuple (Tuple r.counter r.st) (acc <> (Tuple r.instructions r.result : Nil)))
    ) (Tuple (Tuple dst_info.counter dst_info.st) Nil) args
  let (Tuple (Tuple counter'' st'') arg_results) = arg_result
  let arg_instructions = concatMap fst arg_results
  let arg_vals = map snd arg_results
  let instructions =
        arg_instructions
        <> (T.FunCall { f: f, args: arg_vals, dst: dst_info.dst } : Nil)
  let dst_val = fromMaybe dummyOperand dst_info.dst
  pure { counter: counter'', st: st'', instructions, result: PlainOperand dst_val }

emitDereference :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                -> Ast.Exp -> Either CompilerError ExpEmitResult
emitDereference counter st tt inner = do
  r <- emitTackyAndConvert counter st tt inner
  pure { counter: r.counter, st: r.st, instructions: r.instructions, result: DereferencedPointer r.result }

emitDotOperator :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                -> Types.CType -> Ast.Exp -> String -> Either CompilerError ExpEmitResult
emitDotOperator counter st tt t strct mbr = do
  member_offset <- getMemberOffset tt mbr strct.t
  r <- emitTackyForExp counter st tt strct
  case r.result of
    PlainOperand (T.Var v) ->
      pure { counter: r.counter, st: r.st, instructions: r.instructions, result: SubObject v member_offset }
    SubObject baseObj offset ->
      pure { counter: r.counter, st: r.st, instructions: r.instructions, result: SubObject baseObj (offset + member_offset) }
    DereferencedPointer ptr ->
      if member_offset == 0 then pure { counter: r.counter, st: r.st, instructions: r.instructions, result: DereferencedPointer ptr }
      else do
        let tmp = createTmp r.counter r.st (Types.Pointer t)
        let dst = T.Var tmp.name
        let index = T.Constant (Const.ConstLong (BigInt.fromInt member_offset))
        let add_ptr_instr = T.AddPtr { ptr: ptr, index: index, scale: 1, dst: dst }
        pure { counter: tmp.counter, st: tmp.st, instructions: r.instructions <> (add_ptr_instr : Nil), result: DereferencedPointer dst }
    PlainOperand (T.Constant _) ->
      Left (InternalError "found dot operator applied to constant")

emitArrowOperator :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                  -> Types.CType -> Ast.Exp -> String -> Either CompilerError ExpEmitResult
emitArrowOperator counter st tt t strct mbr = do
  member_offset <- getMemberPointerOffset tt mbr strct.t
  r <- emitTackyAndConvert counter st tt strct
  if member_offset == 0 then pure { counter: r.counter, st: r.st, instructions: r.instructions, result: DereferencedPointer r.result }
  else do
    let tmp = createTmp r.counter r.st (Types.Pointer t)
    let dst = T.Var tmp.name
    let index = T.Constant (Const.ConstLong (BigInt.fromInt member_offset))
    let add_ptr_instr = T.AddPtr { ptr: r.result, index: index, scale: 1, dst: dst }
    pure { counter: tmp.counter, st: tmp.st, instructions: r.instructions <> (add_ptr_instr : Nil), result: DereferencedPointer dst }

emitAddrOf :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
           -> Types.CType -> Ast.Exp -> Either CompilerError ExpEmitResult
emitAddrOf counter st tt t inner = do
  r <- emitTackyForExp counter st tt inner
  case r.result of
    PlainOperand o -> do
      let tmp = createTmp r.counter r.st t
      let dst = T.Var tmp.name
      pure { counter: tmp.counter, st: tmp.st
           , instructions: r.instructions <> (T.GetAddress { src: o, dst: dst } : Nil)
           , result: PlainOperand dst }
    DereferencedPointer ptr ->
      pure { counter: r.counter, st: r.st, instructions: r.instructions, result: PlainOperand ptr }
    SubObject baseObj offset -> do
      let tmp = createTmp r.counter r.st t
      let dst = T.Var tmp.name
      let get_addr = T.GetAddress { src: T.Var baseObj, dst: dst }
      if offset == 0 then
        pure { counter: tmp.counter, st: tmp.st
             , instructions: r.instructions <> (get_addr : Nil)
             , result: PlainOperand dst }
      else do
        let index = T.Constant (Const.ConstLong (BigInt.fromInt offset))
        pure { counter: tmp.counter, st: tmp.st
             , instructions: r.instructions
                 <> (get_addr
                     : T.AddPtr { ptr: dst, index: index, scale: 1, dst: dst }
                     : Nil)
             , result: PlainOperand dst }

emitTackyAndConvert :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                    -> Ast.Exp -> Either CompilerError ValEmitResult
emitTackyAndConvert counter st tt e = do
  r <- emitTackyForExp counter st tt e
  case r.result of
    PlainOperand o ->
      pure { counter: r.counter, st: r.st, instructions: r.instructions, result: o }
    DereferencedPointer ptr -> do
      let tmp = createTmp r.counter r.st e.t
      let dst = T.Var tmp.name
      pure { counter: tmp.counter, st: tmp.st
           , instructions: r.instructions <> (T.Load { src_ptr: ptr, dst: dst } : Nil)
           , result: dst }
    SubObject baseObj offset -> do
      let tmp = createTmp r.counter r.st e.t
      let dst = T.Var tmp.name
      pure { counter: tmp.counter, st: tmp.st
           , instructions: r.instructions <> (T.CopyFromOffset { src: baseObj, offset: offset, dst: dst } : Nil)
           , result: dst }

emitStringInit :: String -> Int -> Bytes.Bytes -> List T.TackyInstruction
emitStringInit dst offset s =
  let len = Bytes.length s
  in
    if len == 0 then Nil
    else if len >= 8 then
      let l = Bytes.getInt64Le s 0
          instr = T.CopyToOffset { src: T.Constant (Const.ConstLong l), dst: dst, offset: offset }
          rest = Bytes.sub s 8 (len - 8)
      in instr : emitStringInit dst (offset + 8) rest
    else if len >= 4 then
      let i = Bytes.getInt32Le s 0
          instr = T.CopyToOffset { src: T.Constant (Const.ConstInt i), dst: dst, offset: offset }
          rest = Bytes.sub s 4 (len - 4)
      in instr : emitStringInit dst (offset + 4) rest
    else
      let c = Bytes.getInt8 s 0
          instr = T.CopyToOffset { src: T.Constant (Const.ConstChar c), dst: dst, offset: offset }
          rest = Bytes.sub s 1 (len - 1)
      in instr : emitStringInit dst (offset + 1) rest

emitCompoundInit :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                 -> String -> Int -> Ast.Initializer -> Either CompilerError StmtEmitResult
emitCompoundInit counter st tt name offset = case _ of
  Ast.SingleInit { e: Ast.EString s, t: Types.Array _ size } -> do
    let str_bytes = Bytes.ofString s
    let nullChar = fromMaybe ' ' (fromCharCode 0)
    let padding_bytes = Bytes.make (bigIntToInt size - String.length s) nullChar
    Right { counter, st, instructions: emitStringInit name offset (Bytes.cat str_bytes padding_bytes) }
  Ast.SingleInit e -> do
    r <- emitTackyAndConvert counter st tt e
    pure { counter: r.counter, st: r.st
         , instructions: r.instructions <> (T.CopyToOffset { src: r.result, dst: name, offset: offset } : Nil) }
  Ast.CompoundInit (Types.Array elem_type _) inits -> do
    elem_size <- TypeUtils.getSize tt elem_type
    resultFold (\{ counter: c, st: s, instructions: acc_instrs } (Tuple idx elem_init) -> do
        let new_offset = offset + (idx * bigIntToInt elem_size)
        r <- emitCompoundInit c s tt name new_offset elem_init
        pure { counter: r.counter, st: r.st, instructions: acc_instrs <> r.instructions }
      ) { counter, st, instructions: Nil } (mapWithIndex (\i init -> Tuple i init) inits)
  Ast.CompoundInit (Types.Structure tag) inits -> do
    members <- TypeTable.getMembers tag tt
    resultFold2 (\{ counter: c, st: s, instructions: acc_instrs } memb init -> do
        let mem_offset = offset + memb.offset
        r <- emitCompoundInit c s tt name mem_offset init
        pure { counter: r.counter, st: r.st, instructions: acc_instrs <> r.instructions }
      ) { counter, st, instructions: Nil } members inits
  Ast.CompoundInit _ _ ->
    Left (InternalError "compound init has non-array type!")

emitTackyForStatement :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                      -> Ast.Statement -> Either CompilerError StmtEmitResult
emitTackyForStatement counter st tt = case _ of
  Ast.Return e -> do
    ret <- case e of
      Just expr -> do
        r <- emitTackyAndConvert counter st tt expr
        pure { counter: r.counter, st: r.st, instructions: r.instructions, retVal: Just r.result }
      Nothing -> pure { counter, st, instructions: Nil, retVal: Nothing }
    pure { counter: ret.counter, st: ret.st
         , instructions: ret.instructions <> (T.Return ret.retVal : Nil) }
  Ast.Expression e -> do
    r <- emitTackyForExp counter st tt e
    pure { counter: r.counter, st: r.st, instructions: r.instructions }
  Ast.If condition then_clause else_clause ->
    emitTackyForIfStatement counter st tt condition then_clause else_clause
  Ast.Compound (Ast.Block items) ->
    resultFold (\{ counter: c, st: s, instructions: acc } item -> do
        r <- emitTackyForBlockItem c s tt item
        pure { counter: r.counter, st: r.st, instructions: acc <> r.instructions }
      ) { counter, st, instructions: Nil } items
  Ast.Break id -> Right { counter, st, instructions: T.Jump (breakLabel id) : Nil }
  Ast.Continue id -> Right { counter, st, instructions: T.Jump (continueLabel id) : Nil }
  Ast.DoWhile body condition id ->
    emitTackyForDoLoop counter st tt body condition id
  Ast.While condition body id ->
    emitTackyForWhileLoop counter st tt condition body id
  Ast.For init condition post body id ->
    emitTackyForForLoop counter st tt init condition post body id
  Ast.Null -> Right { counter, st, instructions: Nil }

emitTackyForBlockItem :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                      -> Ast.BlockItem -> Either CompilerError StmtEmitResult
emitTackyForBlockItem counter st tt = case _ of
  Ast.Stmt s -> emitTackyForStatement counter st tt s
  Ast.Decl d -> emitLocalDeclaration counter st tt d

emitLocalDeclaration :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                     -> Ast.Declaration -> Either CompilerError StmtEmitResult
emitLocalDeclaration counter st tt = case _ of
  Ast.VarDecl { storageClass: Just _ } -> Right { counter, st, instructions: Nil }
  Ast.VarDecl vd -> emitVarDeclaration counter st tt vd
  Ast.FunDecl _ -> Right { counter, st, instructions: Nil }
  Ast.StructDecl _ -> Right { counter, st, instructions: Nil }

emitVarDeclaration :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                   -> Ast.VariableDeclaration -> Either CompilerError StmtEmitResult
emitVarDeclaration counter st tt vd = case vd.init of
  Just (Ast.SingleInit { e: Ast.EString _, t: Types.Array _ _ }) ->
    emitCompoundInit counter st tt vd.name 0 (fromMaybe (Ast.SingleInit { e: Ast.Constant Const.intZero, t: Types.Int }) vd.init)
  Just (Ast.SingleInit e) -> do
    r <- emitAssignment counter st tt { e: Ast.Var vd.name, t: vd.varType } e
    pure { counter: r.counter, st: r.st, instructions: r.instructions }
  Just compound_init ->
    emitCompoundInit counter st tt vd.name 0 compound_init
  Nothing ->
    Right { counter, st, instructions: Nil }

emitTackyForIfStatement :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                        -> Ast.Exp -> Ast.Statement -> Maybe Ast.Statement
                        -> Either CompilerError StmtEmitResult
emitTackyForIfStatement counter st tt condition then_clause = case _ of
  Nothing -> do
    let (Tuple counter' end_label) = UniqueIds.makeLabel "if_end" counter
    cr <- emitTackyAndConvert counter' st tt condition
    tr <- emitTackyForStatement cr.counter cr.st tt then_clause
    pure { counter: tr.counter, st: tr.st
         , instructions:
             cr.instructions
             <> (T.JumpIfZero cr.result end_label : tr.instructions)
             <> (T.TLabel end_label : Nil) }
  Just else_clause -> do
    let (Tuple counter' else_label) = UniqueIds.makeLabel "else" counter
    let (Tuple counter'' end_label) = UniqueIds.makeLabel "" counter'
    cr <- emitTackyAndConvert counter'' st tt condition
    tr <- emitTackyForStatement cr.counter cr.st tt then_clause
    er <- emitTackyForStatement tr.counter tr.st tt else_clause
    pure { counter: er.counter, st: er.st
         , instructions:
             cr.instructions
             <> (T.JumpIfZero cr.result else_label : tr.instructions)
             <> (T.Jump end_label
                 : T.TLabel else_label
                 : er.instructions)
             <> (T.TLabel end_label : Nil) }

emitTackyForDoLoop :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                   -> Ast.Statement -> Ast.Exp -> String -> Either CompilerError StmtEmitResult
emitTackyForDoLoop counter st tt body condition id = do
  let (Tuple counter' start_label) = UniqueIds.makeLabel "do_loop_start" counter
  let cont_label = continueLabel id
  let br_label = breakLabel id
  br <- emitTackyForStatement counter' st tt body
  cr <- emitTackyAndConvert br.counter br.st tt condition
  pure { counter: cr.counter, st: cr.st
       , instructions:
           (T.TLabel start_label : br.instructions)
           <> (T.TLabel cont_label : cr.instructions)
           <> (T.JumpIfNotZero cr.result start_label : T.TLabel br_label : Nil) }

emitTackyForWhileLoop :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                      -> Ast.Exp -> Ast.Statement -> String -> Either CompilerError StmtEmitResult
emitTackyForWhileLoop counter st tt condition body id = do
  let cont_label = continueLabel id
  let br_label = breakLabel id
  cr <- emitTackyAndConvert counter st tt condition
  br <- emitTackyForStatement cr.counter cr.st tt body
  pure { counter: br.counter, st: br.st
       , instructions:
           (T.TLabel cont_label : cr.instructions)
           <> (T.JumpIfZero cr.result br_label : br.instructions)
           <> (T.Jump cont_label : T.TLabel br_label : Nil) }

emitTackyForForLoop :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                    -> Ast.ForInit -> Maybe Ast.Exp -> Maybe Ast.Exp -> Ast.Statement -> String
                    -> Either CompilerError StmtEmitResult
emitTackyForForLoop counter st tt init condition post body id = do
  let (Tuple counter' start_label) = UniqueIds.makeLabel "for_start" counter
  let cont_label = continueLabel id
  let br_label = breakLabel id
  init_r <- case init of
    Ast.InitDecl d -> emitVarDeclaration counter' st tt d
    Ast.InitExp e -> case e of
      Just expr -> do
        r <- emitTackyForExp counter' st tt expr
        pure { counter: r.counter, st: r.st, instructions: r.instructions }
      Nothing -> Right { counter: counter', st, instructions: Nil }
  cond_r <- case condition of
    Just cond -> do
      r <- emitTackyAndConvert init_r.counter init_r.st tt cond
      pure { counter: r.counter, st: r.st, instructions: r.instructions <> (T.JumpIfZero r.result br_label : Nil) }
    Nothing -> Right { counter: init_r.counter, st: init_r.st, instructions: Nil }
  body_r <- emitTackyForStatement cond_r.counter cond_r.st tt body
  post_r <- case post of
    Just p -> do
      r <- emitTackyForExp body_r.counter body_r.st tt p
      pure { counter: r.counter, st: r.st, instructions: r.instructions }
    Nothing -> Right { counter: body_r.counter, st: body_r.st, instructions: Nil }
  pure { counter: post_r.counter, st: post_r.st
       , instructions:
           init_r.instructions
           <> (T.TLabel start_label : cond_r.instructions)
           <> body_r.instructions
           <> (T.TLabel cont_label : post_r.instructions)
           <> (T.Jump start_label : T.TLabel br_label : Nil) }

emitFunDeclaration :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                   -> Ast.Declaration
                   -> Either CompilerError { counter :: UniqueIds.Counter, st :: Symbols.SymbolTableMap, result :: Maybe T.TackyTopLevel }
emitFunDeclaration counter st tt = case _ of
  Ast.FunDecl { name, paramList, body: Just (Ast.Block block_items) } -> do
    isGlobal_ <- Symbols.isGlobal name st
    body_r <- resultFold (\{ counter: c, st: s, instructions: acc } item -> do
        r <- emitTackyForBlockItem c s tt item
        pure { counter: r.counter, st: r.st, instructions: acc <> r.instructions }
      ) { counter, st, instructions: Nil } block_items
    let extra_return = T.Return (Just (T.Constant Const.intZero))
    pure { counter: body_r.counter, st: body_r.st
         , result: Just (T.TFunction { name, isGlobal: isGlobal_, paramList
                                      , body: body_r.instructions <> (extra_return : Nil) }) }
  _ -> Right { counter, st, result: Nothing }

convertSymbolsToTacky :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap
                      -> Either CompilerError (List T.TackyTopLevel)
convertSymbolsToTacky st tt =
  let to_var (Tuple name entry) =
        case entry.attrs of
          Symbols.StaticAttr { init: init_, isGlobal: isGlobal_ } ->
            case init_ of
              Symbols.Initial i ->
                Right (Just (T.TStaticVariable { name, t: entry.symType, isGlobal: isGlobal_, init: i }))
              Symbols.Tentative -> do
                zeroInit <- Initializers.zero tt entry.symType
                pure (Just (T.TStaticVariable { name, t: entry.symType, isGlobal: isGlobal_, init: zeroInit }))
              Symbols.NoInitializer -> Right Nothing
          Symbols.ConstAttr init_ ->
            Right (Just (T.TStaticConstant { name, t: entry.symType, init: init_ }))
          _ -> Right Nothing
  in resultFold (\acc binding -> do
      v <- to_var binding
      case v of
        Just x -> pure (acc <> (x : Nil))
        Nothing -> pure acc
    ) Nil (Symbols.bindings st)

gen :: UniqueIds.Counter -> Symbols.SymbolTableMap -> TypeTable.TypeTableMap
    -> Ast.TypedProgram
    -> Either CompilerError { counter :: UniqueIds.Counter, st :: Symbols.SymbolTableMap, program :: T.TackyProgram }
gen counter st tt (Ast.Program decls) = do
  fn_r <- resultFold (\{ counter: c, st: s, fns } decl -> do
      r <- emitFunDeclaration c s tt decl
      case r.result of
        Just fn -> pure { counter: r.counter, st: r.st, fns: fns <> (fn : Nil) }
        Nothing -> pure { counter: r.counter, st: r.st, fns }
    ) { counter, st, fns: Nil } decls
  tacky_var_defs <- convertSymbolsToTacky fn_r.st tt
  pure { counter: fn_r.counter, st: fn_r.st, program: T.TProgram (tacky_var_defs <> fn_r.fns) }

-- Helpers

bigIntToInt :: BigInt -> Int
bigIntToInt n = fromMaybe 0 (BigInt.toInt n)
