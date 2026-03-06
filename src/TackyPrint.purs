module TackyPrint where

import Prelude

import Const as Const
import Data.BigInt as BigInt
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Initializers (StaticInit)
import Tacky (TackyBinaryOperator(..), TackyInstruction(..), TackyProgram(..), TackyTopLevel(..), TackyUnaryOperator(..), TackyVal(..))
import Types as Types
import UniqueIds as UniqueIds

ppPrintList :: forall a. String -> (a -> String) -> List a -> String
ppPrintList sep ppItem items = go items true
  where
  go Nil _ = ""
  go (Cons item rest) isFirst =
    (if isFirst then "" else sep) <> ppItem item <> go rest false

ppInitList :: List StaticInit -> String
ppInitList init_list =
  "{" <> ppPrintList ", " show init_list <> "}"

ppUnaryOperator :: TackyUnaryOperator -> String
ppUnaryOperator = case _ of
  Complement -> "~"
  Negate -> "-"
  Not -> "!"

ppBinaryOperator :: Boolean -> TackyBinaryOperator -> String
ppBinaryOperator escape_brackets = case _ of
  Add -> "+"
  Subtract -> "-"
  Multiply -> "*"
  Divide -> "/"
  Mod -> "%"
  Equal -> "=="
  NotEqual -> "!="
  LessThan -> if escape_brackets then "&lt;" else "<"
  LessOrEqual -> if escape_brackets then "&lt;=" else "<="
  GreaterThan -> if escape_brackets then "&gt;" else ">"
  GreaterOrEqual -> if escape_brackets then "&gt;=" else ">="

constToString :: Const.ConstValue -> String
constToString = case _ of
  Const.ConstInt i -> show i
  Const.ConstLong l -> BigInt.toString l <> "l"
  Const.ConstUInt ui -> BigInt.toString ui <> "u"
  Const.ConstULong ul -> BigInt.toString ul <> "ul"
  Const.ConstDouble d -> show d
  Const.ConstChar c -> show c
  Const.ConstUChar uc -> show uc

ppTackyVal :: TackyVal -> String
ppTackyVal = case _ of
  Constant i -> constToString i
  Var s -> s

ppTackyValList :: List TackyVal -> String
ppTackyValList = ppPrintList ", " ppTackyVal

ppStringList :: List String -> String
ppStringList = ppPrintList ", " identity

ppInstruction :: Boolean -> TackyInstruction -> String
ppInstruction escape_brackets = case _ of
  Return Nothing -> "Return"
  Return (Just v) ->
    "Return(" <> ppTackyVal v <> ")"
  TUnary { op, src, dst } ->
    ppTackyVal dst <> " = " <> ppUnaryOperator op <> ppTackyVal src
  TBinary { op, src1, src2, dst } ->
    ppTackyVal dst <> " = " <> ppTackyVal src1 <> " "
      <> ppBinaryOperator escape_brackets op <> " " <> ppTackyVal src2
  Copy { src, dst } ->
    ppTackyVal dst <> " = " <> ppTackyVal src
  Jump s -> "Jump(" <> s <> ")"
  JumpIfZero cond target ->
    "JumpIfZero(" <> ppTackyVal cond <> ", " <> target <> ")"
  JumpIfNotZero cond target ->
    "JumpIfNotZero(" <> ppTackyVal cond <> ", " <> target <> ")"
  TLabel s -> "\n" <> s <> ":"
  FunCall { f, args, dst: Nothing } ->
    f <> "(" <> ppTackyValList args <> ")"
  FunCall { f, args, dst: Just dst } ->
    ppTackyVal dst <> " = " <> f <> "(" <> ppTackyValList args <> ")"
  SignExtend { src, dst } ->
    ppTackyVal dst <> " = SignExtend(" <> ppTackyVal src <> ")"
  ZeroExtend { src, dst } ->
    ppTackyVal dst <> " = ZeroExtend(" <> ppTackyVal src <> ")"
  Truncate { src, dst } ->
    ppTackyVal dst <> " = Truncate(" <> ppTackyVal src <> ")"
  DoubleToInt { src, dst } ->
    ppTackyVal dst <> " = DoubleToInt(" <> ppTackyVal src <> ")"
  DoubleToUInt { src, dst } ->
    ppTackyVal dst <> " = DoubleToUInt(" <> ppTackyVal src <> ")"
  IntToDouble { src, dst } ->
    ppTackyVal dst <> " = IntToDouble(" <> ppTackyVal src <> ")"
  UIntToDouble { src, dst } ->
    ppTackyVal dst <> " = UIntToDouble(" <> ppTackyVal src <> ")"
  GetAddress { src, dst } ->
    ppTackyVal dst <> " = GetAddress(" <> ppTackyVal src <> ")"
  Load v ->
    ppTackyVal v.dst <> " = Load(" <> ppTackyVal v.src_ptr <> ")"
  Store v ->
    "*(" <> ppTackyVal v.dst_ptr <> ") = " <> ppTackyVal v.src
  AddPtr { ptr, index, scale, dst } ->
    ppTackyVal dst <> " = " <> ppTackyVal ptr <> " + "
      <> ppTackyVal index <> " * " <> show scale
  CopyToOffset { src, dst, offset } ->
    dst <> "[offset = " <> show offset <> "] = " <> ppTackyVal src
  CopyFromOffset { src, offset, dst } ->
    ppTackyVal dst <> " = " <> src <> "[offset = " <> show offset <> "]"

ppFunctionDefinition :: Boolean -> Boolean -> String -> List String -> List TackyInstruction -> String
ppFunctionDefinition escape_brackets isGlobal name paramList body =
  let header = (if isGlobal then "global " else "")
        <> name <> "(" <> ppStringList paramList <> "):\n"
      bodyStr = ppPrintList "\n" (\i -> "    " <> ppInstruction escape_brackets i) body
  in header <> "    " <> bodyStr

ppTl :: Boolean -> TackyTopLevel -> String
ppTl escape_brackets = case _ of
  TFunction { name, isGlobal, paramList, body } ->
    ppFunctionDefinition escape_brackets isGlobal name paramList body
  TStaticVariable { isGlobal, name, init, t } ->
    (if isGlobal then "global " else "")
      <> show t <> " " <> name <> " = " <> ppInitList init
  TStaticConstant { name, init, t } ->
    "const " <> show t <> " " <> name <> " = " <> show init

ppProgram :: Boolean -> TackyProgram -> String
ppProgram escape_brackets (TProgram tls) =
  ppPrintList "\n\n" (ppTl escape_brackets) tls <> "\n"

debugPrintTacky :: Boolean -> UniqueIds.Counter -> String -> TackyProgram -> Tuple UniqueIds.Counter (Maybe (Tuple String String))
debugPrintTacky debug counter src_filename tacky_prog =
  if debug then
    let base = getFileNameWithoutExtension src_filename
        (Tuple counter' lbl) = UniqueIds.makeLabel base counter
        tacky_file = lbl <> ".debug.tacky"
        content = ppProgram false tacky_prog
    in Tuple counter' (Just (Tuple tacky_file content))
  else
    Tuple counter Nothing

-- Simple file path utilities
getFileNameWithoutExtension :: String -> String
getFileNameWithoutExtension path =
  let name = getFileName path
  in case lastIndexOf '.' name of
    Nothing -> name
    Just idx -> CU.take idx name

getFileName :: String -> String
getFileName path =
  case lastIndexOf '/' path of
    Nothing -> path
    Just idx -> CU.drop (idx + 1) path

lastIndexOf :: Char -> String -> Maybe Int
lastIndexOf c s = go (CU.length s - 1)
  where
  go i
    | i < 0 = Nothing
    | otherwise = case CU.charAt i s of
        Just ch | ch == c -> Just i
        _ -> go (i - 1)
