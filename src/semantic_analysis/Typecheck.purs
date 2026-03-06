module Typecheck where

import Prelude

import Ast as Ast
import CompilerError (CompilerError(..))
import Const as Const
import ConstConvert as ConstConvert
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.List (List(..), length)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Initializers as Initializers
import ListUtil as ListUtil
import ResultUtil (resultFold, resultFold2, resultTraverse, resultTraverse2)
import Rounding as Rounding
import Symbols as Symbols
import TypeTable as TypeTable
import TypeUtils (getAlignment, getSize, getType, isArithmetic, isCharacter, isComplete, isCompletePointer, isInteger, isPointer, isScalar, isSigned, setType)
import TypedAst as TypedAst
import Types (CType(..))
import UniqueIds as UniqueIds

type TypecheckState =
  { counter :: UniqueIds.Counter
  , st :: Symbols.SymbolTableMap
  , tt :: TypeTable.TypeTableMap
  }

isLvalue :: TypedAst.Exp -> Boolean
isLvalue { e } = case e of
  TypedAst.Dereference _ -> true
  TypedAst.Subscript _ _ -> true
  TypedAst.Var _ -> true
  TypedAst.EString _ -> true
  TypedAst.Arrow _ _ -> true
  TypedAst.Dot strct _ -> isLvalue strct
  _ -> false

validateType :: TypeTable.TypeTableMap -> CType -> Either CompilerError Unit
validateType tt = case _ of
  Array elem_type _ ->
    if isComplete tt elem_type then validateType tt elem_type
    else Left (TypeError "Array of incomplete type")
  Pointer t -> validateType tt t
  FunType param_types ret_type -> do
    _ <- resultTraverse (validateType tt) param_types
    validateType tt ret_type
  Char -> Right unit
  SChar -> Right unit
  UChar -> Right unit
  Int -> Right unit
  Long -> Right unit
  UInt -> Right unit
  ULong -> Right unit
  Double -> Right unit
  Void -> Right unit
  Structure _ -> Right unit

validateStructDefinition :: TypeTable.TypeTableMap -> Ast.StructDeclaration -> Either CompilerError Unit
validateStructDefinition tt { tag, members } =
  if TypeTable.mem tag tt then Left (TypeError "Structure was already declared")
  else do
    let validateMember member_names { memberName: member_name, memberType: member_type } = do
          if Set.member member_name member_names then
            Left (TypeError ("Duplicate declaration of member " <> member_name <> " in structure " <> tag))
          else do
            validateType tt member_type
            case member_type of
              FunType _ _ ->
                Left (TypeError "Can't declare structure member with function type")
              _ ->
                if isComplete tt member_type then Right unit
                else Left (TypeError "Cannot declare structure member with incomplete type")
            Right (Set.insert member_name member_names)
    _ <- resultFold validateMember Set.empty members
    Right unit

typecheckStructDecl :: TypeTable.TypeTableMap -> Ast.StructDeclaration -> Either CompilerError (Tuple TypeTable.TypeTableMap TypedAst.StructDeclaration)
typecheckStructDecl tt sd@{ tag, members } = do
  if members /= Nil then
    validateStructDefinition tt sd
  else Right unit
  tt' <-
    if members /= Nil then do
      let build_member_def (Tuple (Tuple current_size current_alignment) current_members) { memberName: member_name, memberType: member_type } = do
            member_alignment <- getAlignment tt member_type
            let offset = Rounding.roundAwayFromZero member_alignment current_size
            let member_entry = { memberType: member_type, offset: offset }
            let new_alignment = max current_alignment member_alignment
            member_size <- getSize tt member_type
            let new_size = offset + fromMaybe 0 (BigInt.toInt member_size)
            let new_members = Map.insert member_name member_entry current_members
            Right (Tuple (Tuple new_size new_alignment) new_members)
      Tuple (Tuple unpadded_size alignment) member_defs <-
        resultFold build_member_def (Tuple (Tuple 0 1) Map.empty) members
      let size = Rounding.roundAwayFromZero alignment unpadded_size
      let struct_def = { alignment: alignment, size: size, members: member_defs }
      Right (TypeTable.addStructDefinition tag struct_def tt)
    else
      Right tt
  let cvt { memberName: member_name, memberType: member_type } =
        { memberName: member_name, memberType: member_type }
  Right (Tuple tt' { tag: tag, members: map cvt members })

convertTo :: TypedAst.Exp -> CType -> TypedAst.Exp
convertTo e target_type =
  let cast = TypedAst.Cast target_type e
  in setType cast target_type

getCommonType :: TypeTable.TypeTableMap -> CType -> CType -> Either CompilerError CType
getCommonType tt t1' t2' =
  let t1 = if isCharacter t1' then Int else t1'
      t2 = if isCharacter t2' then Int else t2'
  in
    if t1 == t2 then Right t1
    else if t1 == Double || t2 == Double then Right Double
    else do
      size1 <- getSize tt t1
      size2 <- getSize tt t2
      if size1 == size2 then do
        signed1 <- isSigned t1
        Right (if signed1 then t2 else t1)
      else if size1 > size2 then Right t1
      else Right t2

isZeroInt :: Const.ConstValue -> Boolean
isZeroInt = case _ of
  Const.ConstInt i | i == 0 -> true
  Const.ConstLong l | l == BigInt.fromInt 0 -> true
  Const.ConstUInt u | u == BigInt.fromInt 0 -> true
  Const.ConstULong ul | ul == BigInt.fromInt 0 -> true
  _ -> false

isNullPointerConstant :: TypedAst.InnerExp -> Boolean
isNullPointerConstant = case _ of
  TypedAst.Constant c -> isZeroInt c
  _ -> false

getCommonPointerType :: TypedAst.Exp -> TypedAst.Exp -> Either CompilerError CType
getCommonPointerType e1 e2 =
  if e1.t == e2.t then Right e1.t
  else if isNullPointerConstant e1.e then Right e2.t
  else if isNullPointerConstant e2.e then Right e1.t
  else if (e1.t == Pointer Void && isPointer e2.t) || (e2.t == Pointer Void && isPointer e1.t) then
    Right (Pointer Void)
  else Left (TypeError "Expressions have incompatible types")

convertByAssignment :: TypedAst.Exp -> CType -> Either CompilerError TypedAst.Exp
convertByAssignment e target_type =
  if e.t == target_type then Right e
  else if isArithmetic e.t && isArithmetic target_type then
    Right (convertTo e target_type)
  else if isNullPointerConstant e.e && isPointer target_type then
    Right (convertTo e target_type)
  else if (target_type == Pointer Void && isPointer e.t) || (isPointer target_type && e.t == Pointer Void) then
    Right (convertTo e target_type)
  else Left (TypeError "Cannot convert type for asignment")

typecheckVar :: Symbols.SymbolTableMap -> String -> Either CompilerError TypedAst.Exp
typecheckVar st v = do
  entry <- Symbols.get v st
  let v_type = entry.symType
  let e = TypedAst.Var v
  case v_type of
    FunType _ _ -> Left (TypeError "Tried to use function name as variable ")
    _ -> Right (setType e v_type)

typecheckConst :: Const.ConstValue -> Either CompilerError TypedAst.Exp
typecheckConst c =
  let e = TypedAst.Constant c
  in Right (setType e (Const.typeOfConst c))

optTypecheckResult :: forall a b. (a -> Either CompilerError b) -> Maybe a -> Either CompilerError (Maybe b)
optTypecheckResult f = case _ of
  Just e -> map Just (f e)
  Nothing -> Right Nothing

typecheckString :: String -> Either CompilerError TypedAst.Exp
typecheckString s =
  let e = TypedAst.EString s
      t = Array Char (BigInt.fromInt (String.length s + 1))
  in Right (setType e t)

typecheckExp :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckExp st tt = case _ of
  Ast.Var v -> typecheckVar st v
  Ast.Constant c -> typecheckConst c
  Ast.EString s -> typecheckString s
  Ast.Cast target_type inner -> typecheckCast st tt target_type inner
  Ast.Unary Ast.Not inner -> typecheckNot st tt inner
  Ast.Unary Ast.Complement inner -> typecheckComplement st tt inner
  Ast.Unary Ast.Negate inner -> typecheckNegate st tt inner
  Ast.Binary op e1 e2 ->
    case op of
      Ast.And -> typecheckLogical st tt op e1 e2
      Ast.Or -> typecheckLogical st tt op e1 e2
      Ast.Add -> typecheckAddition st tt e1 e2
      Ast.Subtract -> typecheckSubtraction st tt e1 e2
      Ast.Multiply -> typecheckMultiplicative st tt op e1 e2
      Ast.Divide -> typecheckMultiplicative st tt op e1 e2
      Ast.Mod -> typecheckMultiplicative st tt op e1 e2
      Ast.Equal -> typecheckEquality st tt op e1 e2
      Ast.NotEqual -> typecheckEquality st tt op e1 e2
      Ast.GreaterThan -> typecheckComparison st tt op e1 e2
      Ast.GreaterOrEqual -> typecheckComparison st tt op e1 e2
      Ast.LessThan -> typecheckComparison st tt op e1 e2
      Ast.LessOrEqual -> typecheckComparison st tt op e1 e2
  Ast.Assignment lhs rhs -> typecheckAssignment st tt lhs rhs
  Ast.Conditional condition then_result else_result ->
    typecheckConditional st tt condition then_result else_result
  Ast.FunCall f args -> typecheckFunCall st tt f args
  Ast.Dereference inner -> typecheckDereference st tt inner
  Ast.AddrOf inner -> typecheckAddrOf st tt inner
  Ast.Subscript ptr index -> typecheckSubscript st tt ptr index
  Ast.SizeOfT t -> typecheckSizeOfT st tt t
  Ast.SizeOf e -> typecheckSizeOf st tt e
  Ast.Dot strct memberName -> typecheckDotOperator st tt strct memberName
  Ast.Arrow strct memberName -> typecheckArrowOperator st tt strct memberName

typecheckCast :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> CType -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckCast st tt target_type inner = do
  validateType tt target_type
  typed_inner <- typecheckAndConvert st tt inner
  case Tuple target_type typed_inner.t of
    Tuple Double (Pointer _) ->
      Left (TypeError "Cannot cast between pointer and double")
    Tuple (Pointer _) Double ->
      Left (TypeError "Cannot cast between pointer and double")
    Tuple Void _ ->
      let cast_exp = TypedAst.Cast Void typed_inner
      in Right (setType cast_exp Void)
    _ ->
      if not (isScalar target_type) then
        Left (TypeError "Can only cast to scalar types or void")
      else if not (isScalar typed_inner.t) then
        Left (TypeError "Can only cast scalar expressions to non-void type")
      else
        let cast_exp = TypedAst.Cast target_type typed_inner
        in Right (setType cast_exp target_type)

typecheckScalar :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckScalar st tt e = do
  typed_e <- typecheckAndConvert st tt e
  if isScalar typed_e.t then Right typed_e
  else Left (TypeError "A scalar operand is required")

typecheckNot :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckNot st tt inner = do
  typed_inner <- typecheckScalar st tt inner
  let not_exp = TypedAst.Unary Ast.Not typed_inner
  Right (setType not_exp Int)

typecheckComplement :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckComplement st tt inner = do
  typed_inner <- typecheckAndConvert st tt inner
  if not (isInteger typed_inner.t) then
    Left (TypeError "Bitwise complement only valid for integer types")
  else do
    let typed_inner' =
          if isCharacter typed_inner.t then convertTo typed_inner Int
          else typed_inner
    let complement_exp = TypedAst.Unary Ast.Complement typed_inner'
    Right (setType complement_exp typed_inner'.t)

typecheckNegate :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckNegate st tt inner = do
  typed_inner <- typecheckAndConvert st tt inner
  if isArithmetic typed_inner.t then do
    let typed_inner' =
          if isCharacter typed_inner.t then convertTo typed_inner Int
          else typed_inner
    let negate_exp = TypedAst.Unary Ast.Negate typed_inner'
    Right (setType negate_exp typed_inner'.t)
  else Left (TypeError "Can only negate arithmetic types")

typecheckLogical :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.BinaryOperator -> Ast.Exp -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckLogical st tt op e1 e2 = do
  typed_e1 <- typecheckScalar st tt e1
  typed_e2 <- typecheckScalar st tt e2
  let typed_binexp = TypedAst.Binary op typed_e1 typed_e2
  Right (setType typed_binexp Int)

typecheckAddition :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckAddition st tt e1 e2 = do
  typed_e1 <- typecheckAndConvert st tt e1
  typed_e2 <- typecheckAndConvert st tt e2
  if isArithmetic typed_e1.t && isArithmetic typed_e2.t then do
    common_type <- getCommonType tt typed_e1.t typed_e2.t
    let converted_e1 = convertTo typed_e1 common_type
    let converted_e2 = convertTo typed_e2 common_type
    let add_exp = TypedAst.Binary Ast.Add converted_e1 converted_e2
    Right (setType add_exp common_type)
  else if isCompletePointer tt typed_e1.t && isInteger typed_e2.t then do
    let converted_e2 = convertTo typed_e2 Long
    let add_exp = TypedAst.Binary Ast.Add typed_e1 converted_e2
    Right (setType add_exp typed_e1.t)
  else if isCompletePointer tt typed_e2.t && isInteger typed_e1.t then do
    let converted_e1 = convertTo typed_e1 Long
    let add_exp = TypedAst.Binary Ast.Add converted_e1 typed_e2
    Right (setType add_exp typed_e2.t)
  else Left (TypeError "invalid operands for addition")

typecheckSubtraction :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckSubtraction st tt e1 e2 = do
  typed_e1 <- typecheckAndConvert st tt e1
  typed_e2 <- typecheckAndConvert st tt e2
  if isArithmetic typed_e1.t && isArithmetic typed_e2.t then do
    common_type <- getCommonType tt typed_e1.t typed_e2.t
    let converted_e1 = convertTo typed_e1 common_type
    let converted_e2 = convertTo typed_e2 common_type
    let sub_exp = TypedAst.Binary Ast.Subtract converted_e1 converted_e2
    Right (setType sub_exp common_type)
  else if isCompletePointer tt typed_e1.t && isInteger typed_e2.t then do
    let converted_e2 = convertTo typed_e2 Long
    let sub_exp = TypedAst.Binary Ast.Subtract typed_e1 converted_e2
    Right (setType sub_exp typed_e1.t)
  else if isCompletePointer tt typed_e1.t && typed_e1.t == typed_e2.t then do
    let sub_exp = TypedAst.Binary Ast.Subtract typed_e1 typed_e2
    Right (setType sub_exp Long)
  else Left (TypeError "Invalid operands for subtraction")

typecheckMultiplicative :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.BinaryOperator -> Ast.Exp -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckMultiplicative st tt op e1 e2 = do
  typed_e1 <- typecheckAndConvert st tt e1
  typed_e2 <- typecheckAndConvert st tt e2
  if isArithmetic typed_e1.t && isArithmetic typed_e2.t then do
    common_type <- getCommonType tt typed_e1.t typed_e2.t
    let converted_e1 = convertTo typed_e1 common_type
    let converted_e2 = convertTo typed_e2 common_type
    let binary_exp = TypedAst.Binary op converted_e1 converted_e2
    case op of
      Ast.Mod | common_type == Double -> Left (TypeError "Can't apply % to double")
      Ast.Multiply -> Right (setType binary_exp common_type)
      Ast.Divide -> Right (setType binary_exp common_type)
      Ast.Mod -> Right (setType binary_exp common_type)
      _ -> Left (InternalError (show op <> " isn't a multiplicative operator"))
  else Left (TypeError "Can only multiply arithmetic types")

typecheckEquality :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.BinaryOperator -> Ast.Exp -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckEquality st tt op e1 e2 = do
  typed_e1 <- typecheckAndConvert st tt e1
  typed_e2 <- typecheckAndConvert st tt e2
  common_type <-
    if isPointer typed_e1.t || isPointer typed_e2.t then
      getCommonPointerType typed_e1 typed_e2
    else if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
      getCommonType tt typed_e1.t typed_e2.t
    else Left (TypeError "Invalid operands for equality")
  let converted_e1 = convertTo typed_e1 common_type
  let converted_e2 = convertTo typed_e2 common_type
  let binary_exp = TypedAst.Binary op converted_e1 converted_e2
  Right (setType binary_exp Int)

typecheckComparison :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.BinaryOperator -> Ast.Exp -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckComparison st tt op e1 e2 = do
  typed_e1 <- typecheckAndConvert st tt e1
  typed_e2 <- typecheckAndConvert st tt e2
  common_type <-
    if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
      getCommonType tt typed_e1.t typed_e2.t
    else if isPointer typed_e1.t && typed_e1.t == typed_e2.t then Right typed_e1.t
    else Left (TypeError "invalid types for comparions")
  let converted_e1 = convertTo typed_e1 common_type
  let converted_e2 = convertTo typed_e2 common_type
  let binary_exp = TypedAst.Binary op converted_e1 converted_e2
  Right (setType binary_exp Int)

typecheckAssignment :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckAssignment st tt lhs rhs = do
  typed_lhs <- typecheckAndConvert st tt lhs
  if isLvalue typed_lhs then do
    let lhs_type = getType typed_lhs
    typed_rhs <- typecheckAndConvert st tt rhs
    converted_rhs <- convertByAssignment typed_rhs lhs_type
    let assign_exp = TypedAst.Assignment typed_lhs converted_rhs
    Right (setType assign_exp lhs_type)
  else Left (TypeError "left hand side of assignment is invalid lvalue")

typecheckConditional :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Ast.Exp -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckConditional st tt condition then_exp else_exp = do
  typed_condition <- typecheckScalar st tt condition
  typed_then <- typecheckAndConvert st tt then_exp
  typed_else <- typecheckAndConvert st tt else_exp
  result_type <-
    if typed_then.t == Void && typed_else.t == Void then Right Void
    else if isPointer typed_then.t || isPointer typed_else.t then
      getCommonPointerType typed_then typed_else
    else if isArithmetic typed_then.t && isArithmetic typed_else.t then
      getCommonType tt typed_then.t typed_else.t
    else if typed_then.t == typed_else.t then Right typed_then.t
    else Left (TypeError "Invalid operands for conditional")
  let converted_then = convertTo typed_then result_type
  let converted_else = convertTo typed_else result_type
  let conditional_exp = TypedAst.Conditional typed_condition converted_then converted_else
  Right (setType conditional_exp result_type)

typecheckFunCall :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> String -> List Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckFunCall st tt f args = do
  f_entry <- Symbols.get f st
  let f_type = f_entry.symType
  case f_type of
    FunType param_types ret_type ->
      if length param_types /= length args then
        Left (TypeError "Function called with wrong number of arguments")
      else do
        let process_arg arg param_t = do
              typed_arg <- typecheckAndConvert st tt arg
              convertByAssignment typed_arg param_t
        converted_args <- resultTraverse2 process_arg args param_types
        let call_exp = TypedAst.FunCall f converted_args
        Right (setType call_exp ret_type)
    _ -> Left (TypeError "Tried to use variable as function name")

typecheckDereference :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckDereference st tt inner = do
  typed_inner <- typecheckAndConvert st tt inner
  case getType typed_inner of
    Pointer Void -> Left (TypeError "Can't dereference pointer to void")
    Pointer referenced_t ->
      let deref_exp = TypedAst.Dereference typed_inner
      in Right (setType deref_exp referenced_t)
    _ -> Left (TypeError "Tried to dereference non-pointer")

typecheckAddrOf :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckAddrOf st tt inner = do
  typed_inner <- typecheckExp st tt inner
  if isLvalue typed_inner then do
    let inner_t = getType typed_inner
    let addr_exp = TypedAst.AddrOf typed_inner
    Right (setType addr_exp (Pointer inner_t))
  else Left (TypeError "Cannot take address of non-value")

typecheckSubscript :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckSubscript st tt e1 e2 = do
  typed_e1 <- typecheckAndConvert st tt e1
  typed_e2 <- typecheckAndConvert st tt e2
  Tuple ptr_type (Tuple converted_e1 converted_e2) <-
    if isCompletePointer tt typed_e1.t && isInteger typed_e2.t then
      Right (Tuple typed_e1.t (Tuple typed_e1 (convertTo typed_e2 Long)))
    else if isCompletePointer tt typed_e2.t && isInteger typed_e1.t then
      Right (Tuple typed_e2.t (Tuple (convertTo typed_e1 Long) typed_e2))
    else Left (TypeError "Invalid types for subscript operation")
  result_type <- case ptr_type of
    Pointer referenced -> Right referenced
    _ -> Left (InternalError "typechecking subscript")
  let subscript_exp = TypedAst.Subscript converted_e1 converted_e2
  Right (setType subscript_exp result_type)

typecheckSizeOfT :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> CType -> Either CompilerError TypedAst.Exp
typecheckSizeOfT _ tt t = do
  validateType tt t
  if isComplete tt t then do
    let sizeof_exp = TypedAst.SizeOfT t
    Right (setType sizeof_exp ULong)
  else Left (TypeError "Can't apply sizeof to incomplete type")

typecheckSizeOf :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckSizeOf st tt inner = do
  typed_inner <- typecheckExp st tt inner
  if isComplete tt typed_inner.t then do
    let sizeof_exp = TypedAst.SizeOf typed_inner
    Right (setType sizeof_exp ULong)
  else Left (TypeError "Can't apply sizeof to incomplete type")

typecheckAndConvert :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> Either CompilerError TypedAst.Exp
typecheckAndConvert st tt e = do
  typed_e <- typecheckExp st tt e
  case typed_e.t of
    Structure _ | not (isComplete tt typed_e.t) ->
      Left (TypeError "Incomplete structure type not permitted here")
    Array elem_type _ ->
      let addr_exp = TypedAst.AddrOf typed_e
      in Right (setType addr_exp (Pointer elem_type))
    _ -> Right typed_e

typecheckDotOperator :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> String -> Either CompilerError TypedAst.Exp
typecheckDotOperator st tt strct memberName = do
  typed_strct <- typecheckAndConvert st tt strct
  case typed_strct.t of
    Structure tag -> do
      struct_def <- TypeTable.find tag tt
      case Map.lookup memberName struct_def.members of
        Just entry ->
          let dot_exp = TypedAst.Dot typed_strct memberName
          in Right (setType dot_exp entry.memberType)
        Nothing ->
          Left (TypeError ("Struct type " <> tag <> " has no member " <> memberName))
    _ ->
      Left (TypeError "Dot operator can only be applied to expressions with structure type")

typecheckArrowOperator :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> Ast.Exp -> String -> Either CompilerError TypedAst.Exp
typecheckArrowOperator st tt strct_ptr memberName = do
  typed_strct_ptr <- typecheckAndConvert st tt strct_ptr
  case typed_strct_ptr.t of
    Pointer (Structure tag) -> do
      struct_def <- TypeTable.find tag tt
      case Map.lookup memberName struct_def.members of
        Just entry ->
          let arrow_exp = TypedAst.Arrow typed_strct_ptr memberName
          in Right (setType arrow_exp entry.memberType)
        Nothing ->
          Left (TypeError ("Struct type " <> tag <> " is incomplete or has no member " <> memberName))
    _ -> Left (TypeError "Arrow operator can only be applied to pointers to structure")

staticInitHelper :: Tuple UniqueIds.Counter Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> CType -> Ast.Initializer -> Either CompilerError (Tuple (Tuple UniqueIds.Counter Symbols.SymbolTableMap) (List Initializers.StaticInit))
staticInitHelper (Tuple counter st) tt var_type init =
  case Tuple var_type init of
    Tuple (Array elem_type size) (Ast.SingleInit (Ast.EString s)) ->
      if isCharacter elem_type then
        case fromMaybe 0 (BigInt.toInt size) - String.length s of
          0 -> Right (Tuple (Tuple counter st) (Cons (Initializers.StringInit s false) Nil))
          1 -> Right (Tuple (Tuple counter st) (Cons (Initializers.StringInit s true) Nil))
          n | n > 0 ->
            Right (Tuple (Tuple counter st) (Cons (Initializers.StringInit s true) (Cons (Initializers.ZeroInit (n - 1)) Nil)))
          _ -> Left (TypeError "string is too long for initializer")
      else
        Left (TypeError "Can't initialize array of non-character type with string literal")
    Tuple (Array _ _) (Ast.SingleInit _) ->
      Left (TypeError "Can't initialize array from scalar value")
    Tuple (Pointer Char) (Ast.SingleInit (Ast.EString s)) ->
      let Tuple counter' (Tuple str_id st') = Symbols.addStringWithCounter counter s st
      in Right (Tuple (Tuple counter' st') (Cons (Initializers.PointerInit str_id) Nil))
    Tuple _ (Ast.SingleInit (Ast.EString _)) ->
      Left (TypeError "String literal can only initialize char *")
    Tuple (Structure tag) (Ast.CompoundInit inits) -> do
      struct_def <- TypeTable.find tag tt
      members <- TypeTable.getMembers tag tt
      if length inits > length members then
        Left (TypeError "Too many elements in struct initializer")
      else do
        let handle_member (Tuple cs (Tuple current_offset current_inits)) memb init' = do
              let padding =
                    if current_offset < memb.offset then
                      Cons (Initializers.ZeroInit (memb.offset - current_offset)) Nil
                    else Nil
              Tuple cs' more_static_inits <- staticInitHelper cs tt memb.memberType init'
              let new_inits = current_inits <> padding <> more_static_inits
              member_size <- getSize tt memb.memberType
              let new_offset = memb.offset + fromMaybe 0 (BigInt.toInt member_size)
              Right (Tuple cs' (Tuple new_offset new_inits))
        let initialized_members = ListUtil.take (length inits) members
        Tuple cs' (Tuple initialized_size explicit_initializers) <-
          resultFold2 handle_member (Tuple (Tuple counter st) (Tuple 0 Nil)) initialized_members inits
        let trailing_padding =
              if initialized_size < struct_def.size then
                Cons (Initializers.ZeroInit (struct_def.size - initialized_size)) Nil
              else Nil
        Right (Tuple cs' (explicit_initializers <> trailing_padding))
    Tuple (Structure _) (Ast.SingleInit _) ->
      Left (TypeError " Can't initialize static structure with scalar value")
    Tuple _ (Ast.SingleInit (Ast.Constant c)) | isZeroInt c -> do
      zero_inits <- Initializers.zero tt var_type
      Right (Tuple (Tuple counter st) zero_inits)
    Tuple (Pointer _) _ -> Left (TypeError "invalid static initializer for pointer")
    Tuple _ (Ast.SingleInit (Ast.Constant c)) ->
      if isArithmetic var_type then do
        converted <- lmap InternalError (ConstConvert.constConvert var_type c)
        let init_val = case converted of
              Const.ConstChar ch -> Initializers.CharInit ch
              Const.ConstInt i -> Initializers.IntInit i
              Const.ConstLong l -> Initializers.LongInit l
              Const.ConstUChar uc -> Initializers.UCharInit uc
              Const.ConstUInt ui -> Initializers.UIntInit ui
              Const.ConstULong ul -> Initializers.ULongInit ul
              Const.ConstDouble d -> Initializers.DoubleInit d
        Right (Tuple (Tuple counter st) (Cons init_val Nil))
      else
        Left (InternalError ("should have already rejected initializer with type " <> show var_type))
    Tuple _ (Ast.SingleInit _) -> Left (TypeError "non-constant initializer")
    Tuple (Array elem_type size) (Ast.CompoundInit inits) -> do
      Tuple cs' static_inits_nested <-
        resultFold
          (\(Tuple cs acc) init' ->
            map (\(Tuple cs'' inits') -> Tuple cs'' (acc <> (Cons inits' Nil)))
              (staticInitHelper cs tt elem_type init'))
          (Tuple (Tuple counter st) Nil) inits
      let static_inits = join static_inits_nested
      padding <- case fromMaybe 0 (BigInt.toInt size) - length inits of
        0 -> Right Nil
        n | n > 0 -> do
          elem_size <- getSize tt elem_type
          let zero_bytes = fromMaybe 0 (BigInt.toInt elem_size) * n
          Right (Cons (Initializers.ZeroInit zero_bytes) Nil)
        _ -> Left (TypeError "Too many values in static initializer")
      Right (Tuple cs' (static_inits <> padding))
    Tuple _ (Ast.CompoundInit _) ->
      Left (TypeError "Can't use compound initializer for object with scalar type")

toStaticInit :: Tuple UniqueIds.Counter Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> CType -> Ast.Initializer -> Either CompilerError (Tuple (Tuple UniqueIds.Counter Symbols.SymbolTableMap) Symbols.InitialValue)
toStaticInit cs tt var_type init =
  map (\(Tuple cs' inits) -> Tuple cs' (Symbols.Initial inits)) (staticInitHelper cs tt var_type init)

makeZeroInit :: TypeTable.TypeTableMap -> CType -> Either CompilerError TypedAst.Initializer
makeZeroInit tt t =
  let scalar c = Right (TypedAst.SingleInit { e: TypedAst.Constant c, t: t })
  in case t of
    Array elem_type size -> do
      elems <- resultTraverse (\_ -> makeZeroInit tt elem_type) (ListUtil.makeList (fromMaybe 0 (BigInt.toInt size)) unit)
      Right (TypedAst.CompoundInit t elems)
    Structure tag -> do
      members <- TypeTable.getMembers tag tt
      member_inits <- resultTraverse (\m -> makeZeroInit tt m.memberType) members
      Right (TypedAst.CompoundInit t member_inits)
    Char -> scalar (Const.ConstChar 0)
    SChar -> scalar (Const.ConstChar 0)
    Int -> scalar (Const.ConstInt 0)
    UChar -> scalar (Const.ConstUChar 0)
    UInt -> scalar (Const.ConstUInt (BigInt.fromInt 0))
    Long -> scalar (Const.ConstLong (BigInt.fromInt 0))
    ULong -> scalar (Const.ConstULong (BigInt.fromInt 0))
    Pointer _ -> scalar (Const.ConstULong (BigInt.fromInt 0))
    Double -> scalar (Const.ConstDouble 0.0)
    FunType _ _ -> Left (InternalError ("can't create zero initializer with type " <> show t))
    Void -> Left (InternalError ("can't create zero initializer with type " <> show t))

typecheckInit :: Symbols.SymbolTableMap -> TypeTable.TypeTableMap -> CType -> Ast.Initializer -> Either CompilerError TypedAst.Initializer
typecheckInit st tt target_type init =
  case Tuple target_type init of
    Tuple (Array elem_type size) (Ast.SingleInit (Ast.EString s)) ->
      if not (isCharacter elem_type) then
        Left (TypeError "Can't initialize non-character type with string literal")
      else if String.length s > fromMaybe 0 (BigInt.toInt size) then
        Left (TypeError "Too many characters in string literal")
      else Right (TypedAst.SingleInit (setType (TypedAst.EString s) target_type))
    Tuple (Structure tag) (Ast.CompoundInit init_list) -> do
      members <- TypeTable.getMembers tag tt
      if length init_list > length members then
        Left (TypeError "Too many elements in structure initializer")
      else do
        let Tuple initialized_members uninitialized_members =
              ListUtil.takeDrop (length init_list) members
        typechecked_members <-
          resultTraverse2 (\memb init' -> typecheckInit st tt memb.memberType init') initialized_members init_list
        padding <-
          resultTraverse (\m -> makeZeroInit tt m.memberType) uninitialized_members
        Right (TypedAst.CompoundInit target_type (typechecked_members <> padding))
    Tuple _ (Ast.SingleInit e) -> do
      typechecked_e <- typecheckAndConvert st tt e
      cast_exp <- convertByAssignment typechecked_e target_type
      Right (TypedAst.SingleInit cast_exp)
    Tuple (Array elem_type size) (Ast.CompoundInit inits) ->
      if length inits > fromMaybe 0 (BigInt.toInt size) then
        Left (TypeError "too many values in initializer ")
      else do
        typechecked_inits <- resultTraverse (typecheckInit st tt elem_type) inits
        padding <-
          resultTraverse
            (\_ -> makeZeroInit tt elem_type)
            (ListUtil.makeList (fromMaybe 0 (BigInt.toInt size) - length inits) unit)
        Right (TypedAst.CompoundInit target_type (typechecked_inits <> padding))
    Tuple _ (Ast.CompoundInit _) -> Left (TypeError "Can't initializer scalar value from compound initializer")

typecheckBlock :: TypecheckState -> CType -> Ast.Block -> Either CompilerError (Tuple TypecheckState TypedAst.Block)
typecheckBlock state ret_type (Ast.Block b) =
  map (\(Tuple state' items) -> Tuple state' (TypedAst.Block items))
    (resultFold
      (\(Tuple state' acc) item ->
        map (\(Tuple state'' item') -> Tuple state'' (acc <> Cons item' Nil))
          (typecheckBlockItem state' ret_type item))
      (Tuple state Nil) b)

typecheckBlockItem :: TypecheckState -> CType -> Ast.BlockItem -> Either CompilerError (Tuple TypecheckState TypedAst.BlockItem)
typecheckBlockItem state ret_type = case _ of
  Ast.Stmt s ->
    map (\(Tuple state' s') -> Tuple state' (TypedAst.Stmt s'))
      (typecheckStatement state ret_type s)
  Ast.Decl d ->
    map (\(Tuple state' d') -> Tuple state' (TypedAst.Decl d'))
      (typecheckLocalDecl state d)

typecheckStatement :: TypecheckState -> CType -> Ast.Statement -> Either CompilerError (Tuple TypecheckState TypedAst.Statement)
typecheckStatement state ret_type = case _ of
  Ast.Return (Just e) ->
    if ret_type == Void then
      Left (TypeError "function with void return type cannot return a value")
    else do
      typed_e <- typecheckAndConvert state.st state.tt e
      converted_e <- convertByAssignment typed_e ret_type
      Right (Tuple state (TypedAst.Return (Just converted_e)))
  Ast.Return Nothing ->
    if ret_type == Void then Right (Tuple state (TypedAst.Return Nothing))
    else Left (TypeError "Function with non-void return type must return a value")
  Ast.Expression e ->
    map (\e' -> Tuple state (TypedAst.Expression e'))
      (typecheckAndConvert state.st state.tt e)
  Ast.If condition thenClause elseClause -> do
    typedCond <- typecheckScalar state.st state.tt condition
    Tuple state' typedThen <- typecheckStatement state ret_type thenClause
    Tuple state'' typedElse <- case elseClause of
      Just s ->
        map (\(Tuple st' s') -> Tuple st' (Just s'))
          (typecheckStatement state' ret_type s)
      Nothing -> Right (Tuple state' Nothing)
    Right (Tuple state'' (TypedAst.If typedCond typedThen typedElse))
  Ast.Compound block ->
    map (\(Tuple state' b') -> Tuple state' (TypedAst.Compound b'))
      (typecheckBlock state ret_type block)
  Ast.While condition body id -> do
    typedCond <- typecheckScalar state.st state.tt condition
    Tuple state' typedBody <- typecheckStatement state ret_type body
    Right (Tuple state' (TypedAst.While typedCond typedBody id))
  Ast.DoWhile body condition id -> do
    Tuple state' typedBody <- typecheckStatement state ret_type body
    typedCond <- typecheckScalar state'.st state'.tt condition
    Right (Tuple state' (TypedAst.DoWhile typedBody typedCond id))
  Ast.For init condition post body id -> do
    Tuple state' typechecked_for_init <- case init of
      Ast.InitDecl d | isJust d.storageClass ->
        Left (TypeError "Storage class not permitted on declaration in for loop header")
      Ast.InitDecl d ->
        map (\(Tuple st' d') -> Tuple st' (TypedAst.InitDecl d'))
          (typecheckLocalVarDecl state d)
      Ast.InitExp e ->
        map (\e' -> Tuple state (TypedAst.InitExp e'))
          (optTypecheckResult (typecheckAndConvert state.st state.tt) e)
    typedCond <- optTypecheckResult (typecheckScalar state'.st state'.tt) condition
    typedPost <- optTypecheckResult (typecheckAndConvert state'.st state'.tt) post
    Tuple state'' typedBody <- typecheckStatement state' ret_type body
    Right (Tuple state'' (TypedAst.For typechecked_for_init typedCond typedPost typedBody id))
  Ast.Null -> Right (Tuple state TypedAst.Null)
  Ast.Break s -> Right (Tuple state (TypedAst.Break s))
  Ast.Continue s -> Right (Tuple state (TypedAst.Continue s))

typecheckLocalDecl :: TypecheckState -> Ast.Declaration -> Either CompilerError (Tuple TypecheckState TypedAst.Declaration)
typecheckLocalDecl state = case _ of
  Ast.VarDecl vd ->
    map (\(Tuple state' vd') -> Tuple state' (TypedAst.VarDecl vd'))
      (typecheckLocalVarDecl state vd)
  Ast.FunDecl fd ->
    map (\(Tuple state' fd') -> Tuple state' (TypedAst.FunDecl fd'))
      (typecheckFnDecl state fd)
  Ast.StructDecl sd ->
    map (\(Tuple tt' sd') -> Tuple (state { tt = tt' }) (TypedAst.StructDecl sd'))
      (typecheckStructDecl state.tt sd)

typecheckLocalVarDecl :: TypecheckState -> Ast.VariableDeclaration -> Either CompilerError (Tuple TypecheckState TypedAst.VariableDeclaration)
typecheckLocalVarDecl state { name, varType, init: init, storageClass } = do
  if varType == Void then Left (TypeError "No void declarations")
  else validateType state.tt varType
  case storageClass of
    Just Ast.Extern -> do
      if isJust init then
        Left (TypeError "initializer on local extern declaration")
      else Right unit
      case Symbols.getOpt name state.st of
        Just { symType: t } ->
          if t /= varType then
            Left (TypeError "Variable redeclared with different type")
          else Right unit
        Nothing -> Right unit
      let st' = case Symbols.getOpt name state.st of
            Just _ -> state.st
            Nothing -> Symbols.addStaticVar name varType true Symbols.NoInitializer state.st
      Right (Tuple (state { st = st' })
            { name: name
            , init: Nothing
            , storageClass: storageClass
            , varType: varType })
    _ | not (isComplete state.tt varType) ->
      Left (TypeError "Cannot define a variable with an incomplete type")
    Just Ast.Static -> do
      zero_inits <- Initializers.zero state.tt varType
      let zero_init = Symbols.Initial zero_inits
      Tuple (Tuple counter' st') static_init <-
        case init of
          Just i -> toStaticInit (Tuple state.counter state.st) state.tt varType i
          Nothing -> Right (Tuple (Tuple state.counter state.st) zero_init)
      let st'' = Symbols.addStaticVar name varType false static_init st'
      Right (Tuple (state { counter = counter', st = st'' })
            { name: name
            , init: Nothing
            , storageClass: storageClass
            , varType: varType })
    Nothing -> do
      let st' = Symbols.addAutomaticVar name varType state.st
      let state' = state { st = st' }
      typechecked_init <- optTypecheckResult (typecheckInit state'.st state'.tt varType) init
      Right (Tuple state'
            { name: name
            , varType: varType
            , storageClass: storageClass
            , init: typechecked_init })

typecheckFnDecl :: TypecheckState -> Ast.FunctionDeclaration -> Either CompilerError (Tuple TypecheckState TypedAst.FunctionDeclaration)
typecheckFnDecl state fd = do
  let name = fd.name
  let funType = fd.funType
  let paramList = fd.paramList
  let body = fd.body
  let storageClass = fd.storageClass
  validateType state.tt funType
  let adjustParamType = case _ of
        Array elem_type _ -> Right (Pointer elem_type)
        Void -> Left (TypeError "No void params allowed")
        t -> Right t
  Tuple param_ts (Tuple return_t funType') <- case funType of
    FunType _ (Array _ _) ->
      Left (TypeError "A function cannot return an array")
    FunType param_types ret_type -> do
      param_types' <- resultTraverse adjustParamType param_types
      Right (Tuple param_types' (Tuple ret_type (FunType param_types' ret_type)))
    _ ->
      Left (InternalError "function has non-function type")
  let has_body = isJust body
  if has_body && not ((return_t == Void || isComplete state.tt return_t) && allComplete state.tt param_ts) then
    Left (TypeError "Can't define a function with incomplete return type or parameter type")
  else do
    let isGlobal = storageClass /= Just Ast.Static
    let checkAgainstPrevious { symType: prev_t, attrs } =
          if prev_t /= funType' then
            Left (TypeError ("Redeclared function " <> name <> " with a different type"))
          else case attrs of
            Symbols.FunAttr { isGlobal: prev_global, defined: prev_defined } ->
              if prev_defined && has_body then
                Left (TypeError ("Defined body of function " <> name <> "twice"))
              else if prev_global && storageClass == Just Ast.Static then
                Left (TypeError "Static function declaration follows non-static")
              else
                let defined = has_body || prev_defined
                in Right (Tuple defined prev_global)
            _ ->
              Left (InternalError "symbol has function type but not function attributes")
    let old_decl = Symbols.getOpt name state.st
    Tuple defined isGlobal' <- case old_decl of
      Just old_d -> checkAgainstPrevious old_d
      Nothing -> Right (Tuple has_body isGlobal)
    let st' = Symbols.addFun name funType' isGlobal' defined state.st
    let st'' =
          if has_body then
            foldlWithParams st' paramList param_ts
          else st'
    let state' = state { st = st'' }
    Tuple state'' body' <- case body of
      Just b -> do
        Tuple state'' b' <- typecheckBlock state' return_t b
        Right (Tuple state'' (Just b'))
      Nothing -> Right (Tuple state' Nothing)
    let fd' =
          { funType: funType'
          , name: name
          , paramList: paramList
          , body: body'
          , storageClass: storageClass
          }
    Right (Tuple state'' fd')
  where
  allComplete :: TypeTable.TypeTableMap -> List CType -> Boolean
  allComplete _ Nil = true
  allComplete tt' (Cons t rest) = isComplete tt' t && allComplete tt' rest

  foldlWithParams :: Symbols.SymbolTableMap -> List String -> List CType -> Symbols.SymbolTableMap
  foldlWithParams s Nil _ = s
  foldlWithParams s _ Nil = s
  foldlWithParams s (Cons p ps) (Cons t ts) = foldlWithParams (Symbols.addAutomaticVar p t s) ps ts

typecheckFileScopeVarDecl :: TypecheckState -> Ast.VariableDeclaration -> Either CompilerError (Tuple TypecheckState TypedAst.VariableDeclaration)
typecheckFileScopeVarDecl state { name, varType, init: init, storageClass } = do
  if varType == Void then Left (TypeError "void variables not allowed")
  else validateType state.tt varType
  let default_init =
        if storageClass == Just Ast.Extern then Symbols.NoInitializer else Symbols.Tentative
  Tuple (Tuple counter' st') static_init <- case init of
    Just i -> toStaticInit (Tuple state.counter state.st) state.tt varType i
    Nothing -> Right (Tuple (Tuple state.counter state.st) default_init)
  if not (isComplete state.tt varType || static_init == Symbols.NoInitializer) then
    Left (TypeError "Can't define a variable with an incomplete type ")
  else do
    let current_global = storageClass /= Just Ast.Static
    let old_decl = Symbols.getOpt name st'
    let checkAgainstPrevious { symType: t, attrs } =
          if t /= varType then Left (TypeError "Variable redeclared with different type")
          else case attrs of
            Symbols.StaticAttr { isGlobal: prev_global, init: prev_init } -> do
              isGlobal' <-
                if storageClass == Just Ast.Extern then Right prev_global
                else if current_global == prev_global then Right current_global
                else Left (TypeError "Conflicting variable linkage")
              init' <- case Tuple prev_init static_init of
                Tuple (Symbols.Initial _) (Symbols.Initial _) ->
                  Left (TypeError "Conflicting global variable definition")
                Tuple (Symbols.Initial _) _ -> Right prev_init
                Tuple Symbols.Tentative Symbols.Tentative -> Right Symbols.Tentative
                Tuple Symbols.Tentative Symbols.NoInitializer -> Right Symbols.Tentative
                Tuple _ (Symbols.Initial _) -> Right static_init
                Tuple Symbols.NoInitializer _ -> Right static_init
              Right (Tuple isGlobal' init')
            _ ->
              Left (InternalError "file-scope variable previously declared as local variable or function")
    Tuple isGlobal'' init' <- case old_decl of
      Just old_d -> checkAgainstPrevious old_d
      Nothing -> Right (Tuple current_global static_init)
    let st'' = Symbols.addStaticVar name varType isGlobal'' init' st'
    Right (Tuple (state { counter = counter', st = st'' })
          { name: name
          , varType: varType
          , init: Nothing
          , storageClass: storageClass })

typecheckGlobalDecl :: TypecheckState -> Ast.Declaration -> Either CompilerError (Tuple TypecheckState TypedAst.Declaration)
typecheckGlobalDecl state = case _ of
  Ast.FunDecl fd ->
    map (\(Tuple state' d) -> Tuple state' (TypedAst.FunDecl d))
      (typecheckFnDecl state fd)
  Ast.VarDecl vd ->
    map (\(Tuple state' d) -> Tuple state' (TypedAst.VarDecl d))
      (typecheckFileScopeVarDecl state vd)
  Ast.StructDecl sd ->
    map (\(Tuple tt' d) -> Tuple (state { tt = tt' }) (TypedAst.StructDecl d))
      (typecheckStructDecl state.tt sd)

typecheck :: TypecheckState -> Ast.UntypedProgram -> Either CompilerError (Tuple TypecheckState TypedAst.TypedProgram)
typecheck state (Ast.Program decls) =
  map (\(Tuple state' decls') -> Tuple state' (TypedAst.Program decls'))
    (resultFold
      (\(Tuple state' acc) decl ->
        map (\(Tuple state'' d) -> Tuple state'' (acc <> Cons d Nil))
          (typecheckGlobalDecl state' decl))
      (Tuple state Nil) decls)
