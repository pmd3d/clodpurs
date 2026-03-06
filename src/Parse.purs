module Parse where

import Prelude

import Ast as Ast
import Const as Const
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.List (List(..), (:), elem, any, nub, sortBy, partition, unzip)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Char (toCharCode, fromCharCode)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), fst, snd)
import TokStream as TokStream
import Tokens (Token(..))
import Types as Types

-- Peek helpers

peekOpt :: TokStream.TokStream -> Maybe Token
peekOpt = TokStream.peek

peekEq :: Token -> TokStream.TokStream -> Boolean
peekEq tok tokens = peekOpt tokens == Just tok

peekIs :: (Token -> Boolean) -> TokStream.TokStream -> Boolean
peekIs pred tokens = case peekOpt tokens of
  Just t -> pred t
  Nothing -> false

-- Error formatting

data Expected
  = Tok Token
  | Name String

ppExpected :: Expected -> String
ppExpected (Tok tk) = show tk
ppExpected (Name s) = s

formatError :: Expected -> Token -> String
formatError expected actual =
  "Expected " <> ppExpected expected <> " but found " <> show actual

-- Core token operations

takeToken :: TokStream.TokStream -> Either String (Tuple Token TokStream.TokStream)
takeToken = TokStream.takeToken

expect :: Token -> TokStream.TokStream -> Either String TokStream.TokStream
expect expected tokens = do
  Tuple actual tokens' <- takeToken tokens
  if actual /= expected then
    Left (formatError (Tok expected) actual)
  else
    Right tokens'

-- Pure helpers

unescape :: String -> String
unescape s =
  let
    chrStr :: Int -> String
    chrStr n = case fromCharCode n of
      Just c -> SCU.singleton c
      Nothing -> ""

    escapes =
      [ Tuple "\\'" (chrStr 39)
      , Tuple "\\\"" (chrStr 34)
      , Tuple "\\?" (chrStr 63)
      , Tuple "\\\\" (chrStr 92)
      , Tuple "\\a" (chrStr 7)
      , Tuple "\\b" (chrStr 8)
      , Tuple "\\f" (chrStr 12)
      , Tuple "\\n" (chrStr 10)
      , Tuple "\\r" (chrStr 13)
      , Tuple "\\t" (chrStr 9)
      , Tuple "\\v" (chrStr 11)
      ]

    findEscape :: String -> List (Tuple String String) -> Maybe (Tuple String String)
    findEscape _ Nil = Nothing
    findEscape remaining (Cons (Tuple escSeq unescaped) rest) =
      if startsWith escSeq remaining then Just (Tuple escSeq unescaped)
      else findEscape remaining rest

    startsWith :: String -> String -> Boolean
    startsWith prefix str =
      SCU.take (SCU.length prefix) str == prefix

    unescapeNext :: String -> String
    unescapeNext remaining =
      if remaining == "" then ""
      else case findEscape remaining (List.fromFoldable escapes) of
        Just (Tuple escSeq unescaped) ->
          unescaped <> unescapeNext (SCU.drop (SCU.length escSeq) remaining)
        Nothing ->
          SCU.take 1 remaining <> unescapeNext (SCU.drop 1 remaining)
  in
    unescapeNext s

isIdent :: Token -> Boolean
isIdent (Identifier _) = true
isIdent _ = false

isTypeSpecifier :: Token -> Boolean
isTypeSpecifier KWInt = true
isTypeSpecifier KWLong = true
isTypeSpecifier KWUnsigned = true
isTypeSpecifier KWSigned = true
isTypeSpecifier KWDouble = true
isTypeSpecifier KWChar = true
isTypeSpecifier KWVoid = true
isTypeSpecifier KWStruct = true
isTypeSpecifier _ = false

isSpecifier :: Token -> Boolean
isSpecifier KWStatic = true
isSpecifier KWExtern = true
isSpecifier other = isTypeSpecifier other

getPrecedence :: Token -> Maybe Int
getPrecedence Star = Just 50
getPrecedence Slash = Just 50
getPrecedence Percent = Just 50
getPrecedence Plus = Just 45
getPrecedence Hyphen = Just 45
getPrecedence LessThan = Just 35
getPrecedence LessOrEqual = Just 35
getPrecedence GreaterThan = Just 35
getPrecedence GreaterOrEqual = Just 35
getPrecedence DoubleEqual = Just 30
getPrecedence NotEqual = Just 30
getPrecedence LogicalAnd = Just 10
getPrecedence LogicalOr = Just 5
getPrecedence QuestionMark = Just 3
getPrecedence EqualSign = Just 1
getPrecedence _ = Nothing

-- Pure functions that can fail

parseStorageClass :: Token -> Either String Ast.StorageClass
parseStorageClass KWExtern = Right Ast.Extern
parseStorageClass KWStatic = Right Ast.Static
parseStorageClass other = Left (formatError (Name "a storage class specifier") other)

parseType :: List Token -> Either String Types.CType
parseType specifierList =
  let sorted = sortBy compare specifierList
  in case sorted of
    Cons (Identifier tag) Nil -> Right (Types.Structure tag)
    Cons KWVoid Nil -> Right Types.Void
    Cons KWDouble Nil -> Right Types.Double
    Cons KWChar Nil -> Right Types.Char
    Cons KWChar (Cons KWSigned Nil) -> Right Types.SChar
    Cons KWChar (Cons KWUnsigned Nil) -> Right Types.UChar
    _ ->
      if sorted == Nil
        || sortBy compare (nub sorted) /= sortBy compare sorted
        || elem KWDouble sorted
        || elem KWChar sorted
        || elem KWVoid sorted
        || any isIdent sorted
        || (elem KWSigned sorted && elem KWUnsigned sorted)
      then Left "Invalid type specifier"
      else if elem KWUnsigned sorted && elem KWLong sorted then
        Right Types.ULong
      else if elem KWUnsigned sorted then
        Right Types.UInt
      else if elem KWLong sorted then
        Right Types.Long
      else
        Right Types.Int

parseSignedConstant :: Token -> Either String Const.ConstValue
parseSignedConstant token = do
  Tuple v isInt <- case token of
    ConstInt i -> Right (Tuple i true)
    ConstLong l -> Right (Tuple l false)
    other -> Left (formatError (Name "a signed integer constant") other)
  let maxLong = BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 63) - BigInt.fromInt 1
  let maxInt = BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 31) - BigInt.fromInt 1
  if v > maxLong then
    Left "Constant is too large to represent as an int or long"
  else if isInt && v <= maxInt then
    Right (Const.ConstInt (bigIntToInt v))
  else
    Right (Const.ConstLong v)

parseUnsignedConstant :: Token -> Either String Const.ConstValue
parseUnsignedConstant token = do
  Tuple v isUint <- case token of
    ConstUInt ui -> Right (Tuple ui true)
    ConstULong ul -> Right (Tuple ul false)
    other -> Left (formatError (Name "an unsigned integer constant") other)
  let maxULong = BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 64) - BigInt.fromInt 1
  let maxUInt = BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 32) - BigInt.fromInt 1
  if v > maxULong then
    Left "Constant is too large to represent as an unsigned int or unsigned long"
  else if isUint && v <= maxUInt then
    Right (Const.ConstUInt v)
  else
    Right (Const.ConstULong v)

parseCharConst :: String -> Either String Const.ConstValue
parseCharConst token =
  let unescaped = unescape token
  in if SCU.length unescaped == 1 then
    case toCharCode <$> SCU.charAt 0 unescaped of
      Just chCode -> Right (Const.ConstInt chCode)
      Nothing -> Left "multi-character constant tokens not supported"
  else
    Left "multi-character constant tokens not supported"

parseTypeAndStorageClass :: List Token -> Either String (Tuple Types.CType (Maybe Ast.StorageClass))
parseTypeAndStorageClass specifierList = do
  let partitioned = partition (\t -> isTypeSpecifier t || isIdent t) specifierList
  typ <- parseType partitioned.yes
  storageClass <- case partitioned.no of
    Nil -> Right Nothing
    Cons sc Nil -> do
      parsed <- parseStorageClass sc
      Right (Just parsed)
    _ -> Left "Internal error - not a storage class"
  Right (Tuple typ storageClass)

-- Helper to convert BigInt to Int (truncating)
bigIntToInt :: BigInt -> Int
bigIntToInt bi = case BigInt.toInt bi of
  Just i -> i
  Nothing -> 0

-- Parsing functions

parseId :: TokStream.TokStream -> Either String (Tuple String TokStream.TokStream)
parseId tokens = do
  Tuple tok tokens' <- takeToken tokens
  case tok of
    Identifier x -> Right (Tuple x tokens')
    other -> Left (formatError (Name "an identifier") other)

parseTypeSpecifier :: TokStream.TokStream -> Either String (Tuple Token TokStream.TokStream)
parseTypeSpecifier tokens = do
  Tuple spec tokens' <- takeToken tokens
  case spec of
    KWStruct -> do
      Tuple expectedTag tokens'' <- takeToken tokens'
      if isIdent expectedTag then Right (Tuple expectedTag tokens'')
      else Left (formatError (Name "a structure tag") expectedTag)
    _ ->
      if isTypeSpecifier spec then Right (Tuple spec tokens')
      else Left (formatError (Name "a type specifier") spec)

parseTypeSpecifierList :: TokStream.TokStream -> Either String (Tuple (List Token) TokStream.TokStream)
parseTypeSpecifierList tokens = do
  Tuple spec tokens' <- parseTypeSpecifier tokens
  if peekIs isTypeSpecifier tokens' then do
    Tuple rest tokens'' <- parseTypeSpecifierList tokens'
    Right (Tuple (spec : rest) tokens'')
  else
    Right (Tuple (spec : Nil) tokens')

parseSpecifier :: TokStream.TokStream -> Either String (Tuple Token TokStream.TokStream)
parseSpecifier tokens =
  case peekOpt tokens of
    Just spec | isTypeSpecifier spec -> parseTypeSpecifier tokens
    Just spec | isSpecifier spec -> takeToken tokens
    Just spec -> Left (formatError (Name "a type or storage-class specifier") spec)
    Nothing -> Left "Unexpected end of file"

parseSpecifierList :: TokStream.TokStream -> Either String (Tuple (List Token) TokStream.TokStream)
parseSpecifierList tokens = do
  Tuple spec tokens' <- parseSpecifier tokens
  if peekIs isSpecifier tokens' then do
    Tuple rest tokens'' <- parseSpecifierList tokens'
    Right (Tuple (spec : rest) tokens'')
  else
    Right (Tuple (spec : Nil) tokens')

parseConst :: TokStream.TokStream -> Either String (Tuple Const.ConstValue TokStream.TokStream)
parseConst tokens = do
  Tuple constTok tokens' <- takeToken tokens
  constVal <- case constTok of
    ConstInt _ -> parseSignedConstant constTok
    ConstLong _ -> parseSignedConstant constTok
    ConstUInt _ -> parseUnsignedConstant constTok
    ConstULong _ -> parseUnsignedConstant constTok
    ConstDouble d -> Right (Const.ConstDouble d)
    ConstChar c -> parseCharConst c
    other -> Left (formatError (Name "a constant token") other)
  Right (Tuple constVal tokens')

parseDim :: TokStream.TokStream -> Either String (Tuple BigInt TokStream.TokStream)
parseDim tokens = do
  tokens' <- expect OpenBracket tokens
  Tuple c tokens'' <- parseConst tokens'
  dim <- case c of
    Const.ConstDouble _ -> Left "Floating-point array dimensions not allowed"
    Const.ConstChar i -> Right (BigInt.fromInt i)
    Const.ConstInt i -> Right (BigInt.fromInt i)
    Const.ConstLong l -> Right l
    Const.ConstUChar i -> Right (BigInt.fromInt i)
    Const.ConstUInt u -> Right u
    Const.ConstULong ul -> Right ul
  tokens''' <- expect CloseBracket tokens''
  Right (Tuple dim tokens''')

parseString :: TokStream.TokStream -> Either String (Tuple String TokStream.TokStream)
parseString tokens = do
  Tuple tok tokens' <- takeToken tokens
  case tok of
    StringLiteral s -> Right (Tuple (unescape s) tokens')
    other -> Left (formatError (Name "a string literal") other)

-- Abstract declarator types

data AbstractDeclarator
  = AbstractPointer AbstractDeclarator
  | AbstractArray AbstractDeclarator BigInt
  | AbstractBase

parseAbstractArrayDeclSuffix :: AbstractDeclarator -> TokStream.TokStream -> Either String (Tuple AbstractDeclarator TokStream.TokStream)
parseAbstractArrayDeclSuffix baseDecl tokens = do
  Tuple dim tokens' <- parseDim tokens
  let newDecl = AbstractArray baseDecl dim
  if peekEq OpenBracket tokens' then
    parseAbstractArrayDeclSuffix newDecl tokens'
  else
    Right (Tuple newDecl tokens')

parseAbstractDeclarator :: TokStream.TokStream -> Either String (Tuple AbstractDeclarator TokStream.TokStream)
parseAbstractDeclarator tokens =
  case peekOpt tokens of
    Just Star -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple inner tokens'' <- case peekOpt tokens' of
        Just Star -> parseAbstractDeclarator tokens'
        Just OpenParen -> parseAbstractDeclarator tokens'
        Just OpenBracket -> parseAbstractDeclarator tokens'
        _ -> Right (Tuple AbstractBase tokens')
      Right (Tuple (AbstractPointer inner) tokens'')
    _ -> parseDirectAbstractDeclarator tokens

parseDirectAbstractDeclarator :: TokStream.TokStream -> Either String (Tuple AbstractDeclarator TokStream.TokStream)
parseDirectAbstractDeclarator tokens =
  if peekEq OpenParen tokens then do
    Tuple _ tokens' <- takeToken tokens
    Tuple decl tokens'' <- parseAbstractDeclarator tokens'
    tokens''' <- expect CloseParen tokens''
    if peekEq OpenBracket tokens''' then
      parseAbstractArrayDeclSuffix decl tokens'''
    else
      Right (Tuple decl tokens''')
  else
    parseAbstractArrayDeclSuffix AbstractBase tokens

processAbstractDeclarator :: AbstractDeclarator -> Types.CType -> Types.CType
processAbstractDeclarator decl baseType =
  case decl of
    AbstractBase -> baseType
    AbstractPointer inner ->
      let derivedType = Types.Pointer baseType
      in processAbstractDeclarator inner derivedType
    AbstractArray inner size ->
      let derivedType = Types.Array baseType size
      in processAbstractDeclarator inner derivedType

parseUnop :: TokStream.TokStream -> Either String (Tuple Ast.UnaryOperator TokStream.TokStream)
parseUnop tokens = do
  Tuple tok tokens' <- takeToken tokens
  op <- case tok of
    Tilde -> Right Ast.Complement
    Hyphen -> Right Ast.Negate
    Bang -> Right Ast.Not
    other -> Left (formatError (Name "a unary operator") other)
  Right (Tuple op tokens')

parseBinop :: TokStream.TokStream -> Either String (Tuple Ast.BinaryOperator TokStream.TokStream)
parseBinop tokens = do
  Tuple tok tokens' <- takeToken tokens
  op <- case tok of
    Plus -> Right Ast.Add
    Hyphen -> Right Ast.Subtract
    Star -> Right Ast.Multiply
    Slash -> Right Ast.Divide
    Percent -> Right Ast.Mod
    LogicalAnd -> Right Ast.And
    LogicalOr -> Right Ast.Or
    DoubleEqual -> Right Ast.Equal
    NotEqual -> Right Ast.NotEqual
    LessThan -> Right Ast.LessThan
    LessOrEqual -> Right Ast.LessOrEqual
    GreaterThan -> Right Ast.GreaterThan
    GreaterOrEqual -> Right Ast.GreaterOrEqual
    other -> Left (formatError (Name "a binary operator") other)
  Right (Tuple op tokens')

parseTypeName :: TokStream.TokStream -> Either String (Tuple Types.CType TokStream.TokStream)
parseTypeName tokens = do
  Tuple typeSpecifiers tokens' <- parseTypeSpecifierList tokens
  baseType <- parseType typeSpecifiers
  case peekOpt tokens' of
    Just CloseParen -> Right (Tuple baseType tokens')
    _ -> do
      Tuple abstractDecl tokens'' <- parseAbstractDeclarator tokens'
      Right (Tuple (processAbstractDeclarator abstractDecl baseType) tokens'')

parsePrimaryExp :: TokStream.TokStream -> Either String (Tuple Ast.Exp TokStream.TokStream)
parsePrimaryExp tokens =
  case peekOpt tokens of
    Just (ConstInt _) -> do
      Tuple c tokens' <- parseConst tokens
      Right (Tuple (Ast.Constant c) tokens')
    Just (ConstLong _) -> do
      Tuple c tokens' <- parseConst tokens
      Right (Tuple (Ast.Constant c) tokens')
    Just (ConstUInt _) -> do
      Tuple c tokens' <- parseConst tokens
      Right (Tuple (Ast.Constant c) tokens')
    Just (ConstULong _) -> do
      Tuple c tokens' <- parseConst tokens
      Right (Tuple (Ast.Constant c) tokens')
    Just (ConstDouble _) -> do
      Tuple c tokens' <- parseConst tokens
      Right (Tuple (Ast.Constant c) tokens')
    Just (ConstChar _) -> do
      Tuple c tokens' <- parseConst tokens
      Right (Tuple (Ast.Constant c) tokens')
    Just (Identifier _) -> do
      Tuple id tokens' <- parseId tokens
      if peekEq OpenParen tokens' then do
        Tuple _ tokens'' <- takeToken tokens'
        Tuple args tokens''' <-
          if peekEq CloseParen tokens'' then
            Right (Tuple Nil tokens'')
          else
            parseArgumentList tokens''
        tokens'''' <- expect CloseParen tokens'''
        Right (Tuple (Ast.FunCall id args) tokens'''')
      else
        Right (Tuple (Ast.Var id) tokens')
    Just OpenParen -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple e tokens'' <- parseExp 0 tokens'
      tokens''' <- expect CloseParen tokens''
      Right (Tuple e tokens''')
    Just (StringLiteral _) -> do
      Tuple s tokens' <- parseStringLoop tokens
      Right (Tuple (Ast.EString s) tokens')
    Just t -> Left (formatError (Name "a primary expression") t)
    Nothing -> Left "Unexpected end of file"

parseStringLoop :: TokStream.TokStream -> Either String (Tuple String TokStream.TokStream)
parseStringLoop tokens = do
  Tuple s tokens' <- parseString tokens
  case peekOpt tokens' of
    Just (StringLiteral _) -> do
      Tuple rest tokens'' <- parseStringLoop tokens'
      Right (Tuple (s <> rest) tokens'')
    _ -> Right (Tuple s tokens')

parseArgumentList :: TokStream.TokStream -> Either String (Tuple (List Ast.Exp) TokStream.TokStream)
parseArgumentList tokens = do
  Tuple arg tokens' <- parseExp 0 tokens
  if peekEq Comma tokens' then do
    Tuple _ tokens'' <- takeToken tokens'
    Tuple rest tokens''' <- parseArgumentList tokens''
    Right (Tuple (arg : rest) tokens''')
  else
    Right (Tuple (arg : Nil) tokens')

parsePostfixExp :: TokStream.TokStream -> Either String (Tuple Ast.Exp TokStream.TokStream)
parsePostfixExp tokens = do
  Tuple primary tokens' <- parsePrimaryExp tokens
  postfixLoop primary tokens'

postfixLoop :: Ast.Exp -> TokStream.TokStream -> Either String (Tuple Ast.Exp TokStream.TokStream)
postfixLoop e tokens =
  case peekOpt tokens of
    Just OpenBracket -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple subscript tokens'' <- parseExp 0 tokens'
      tokens''' <- expect CloseBracket tokens''
      postfixLoop (Ast.Subscript e subscript) tokens'''
    Just Dot -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple memberName tokens'' <- parseId tokens'
      postfixLoop (Ast.Dot e memberName) tokens''
    Just Arrow -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple memberName tokens'' <- parseId tokens'
      postfixLoop (Ast.Arrow e memberName) tokens''
    _ -> Right (Tuple e tokens)

parseUnaryExp :: TokStream.TokStream -> Either String (Tuple Ast.Exp TokStream.TokStream)
parseUnaryExp tokens =
  case TokStream.npeek 3 tokens of
    Cons Star _ -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple innerExp tokens'' <- parseCastExp tokens'
      Right (Tuple (Ast.Dereference innerExp) tokens'')
    Cons Ampersand _ -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple innerExp tokens'' <- parseCastExp tokens'
      Right (Tuple (Ast.AddrOf innerExp) tokens'')
    Cons Hyphen _ -> do
      Tuple operator tokens' <- parseUnop tokens
      Tuple innerExp tokens'' <- parseCastExp tokens'
      Right (Tuple (Ast.Unary operator innerExp) tokens'')
    Cons Tilde _ -> do
      Tuple operator tokens' <- parseUnop tokens
      Tuple innerExp tokens'' <- parseCastExp tokens'
      Right (Tuple (Ast.Unary operator innerExp) tokens'')
    Cons Bang _ -> do
      Tuple operator tokens' <- parseUnop tokens
      Tuple innerExp tokens'' <- parseCastExp tokens'
      Right (Tuple (Ast.Unary operator innerExp) tokens'')
    Cons KWSizeOf (Cons OpenParen (Cons t Nil)) | isTypeSpecifier t -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple _ tokens'' <- takeToken tokens'
      Tuple typ tokens''' <- parseTypeName tokens''
      tokens'''' <- expect CloseParen tokens'''
      Right (Tuple (Ast.SizeOfT typ) tokens'''')
    Cons KWSizeOf _ -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple innerExp tokens'' <- parseUnaryExp tokens'
      Right (Tuple (Ast.SizeOf innerExp) tokens'')
    _ -> parsePostfixExp tokens

parseCastExp :: TokStream.TokStream -> Either String (Tuple Ast.Exp TokStream.TokStream)
parseCastExp tokens =
  case TokStream.npeek 2 tokens of
    Cons OpenParen (Cons t Nil) | isTypeSpecifier t -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple targetType tokens'' <- parseTypeName tokens'
      tokens''' <- expect CloseParen tokens''
      Tuple innerExp tokens'''' <- parseCastExp tokens'''
      Right (Tuple (Ast.Cast targetType innerExp) tokens'''')
    _ -> parseUnaryExp tokens

parseConditionalMiddle :: TokStream.TokStream -> Either String (Tuple Ast.Exp TokStream.TokStream)
parseConditionalMiddle tokens = do
  tokens' <- expect QuestionMark tokens
  Tuple e tokens'' <- parseExp 0 tokens'
  tokens''' <- expect Colon tokens''
  Right (Tuple e tokens''')

parseExp :: Int -> TokStream.TokStream -> Either String (Tuple Ast.Exp TokStream.TokStream)
parseExp minPrec tokens = do
  Tuple initialFactor tokens' <- parseCastExp tokens
  parseExpLoop initialFactor tokens'
  where
  parseExpLoop :: Ast.Exp -> TokStream.TokStream -> Either String (Tuple Ast.Exp TokStream.TokStream)
  parseExpLoop left tokens' =
    case peekOpt tokens' of
      Just next ->
        case getPrecedence next of
          Just prec | prec >= minPrec -> do
            Tuple left' tokens'' <-
              if next == EqualSign then do
                Tuple _ ts <- takeToken tokens'
                Tuple right ts' <- parseExp prec ts
                Right (Tuple (Ast.Assignment left right) ts')
              else if next == QuestionMark then do
                Tuple middle ts <- parseConditionalMiddle tokens'
                Tuple right ts' <- parseExp prec ts
                Right (Tuple (Ast.Conditional left middle right) ts')
              else do
                Tuple operator ts <- parseBinop tokens'
                Tuple right ts' <- parseExp (prec + 1) ts
                Right (Tuple (Ast.Binary operator left right) ts')
            parseExpLoop left' tokens''
          _ -> Right (Tuple left tokens')
      Nothing -> Right (Tuple left tokens')

parseOptionalExp :: Token -> TokStream.TokStream -> Either String (Tuple (Maybe Ast.Exp) TokStream.TokStream)
parseOptionalExp delim tokens =
  if peekEq delim tokens then do
    Tuple _ tokens' <- takeToken tokens
    Right (Tuple Nothing tokens')
  else do
    Tuple e tokens' <- parseExp 0 tokens
    tokens'' <- expect delim tokens'
    Right (Tuple (Just e) tokens'')

-- Declarator types

data Declarator
  = Ident String
  | PointerDeclarator Declarator
  | ArrayDeclarator Declarator BigInt
  | FunDeclarator (List ParamInfo) Declarator

data ParamInfo = Param Types.CType Declarator

parseArrayDeclSuffix :: Declarator -> TokStream.TokStream -> Either String (Tuple Declarator TokStream.TokStream)
parseArrayDeclSuffix baseDecl tokens = do
  Tuple dim tokens' <- parseDim tokens
  let newDecl = ArrayDeclarator baseDecl dim
  if peekEq OpenBracket tokens' then
    parseArrayDeclSuffix newDecl tokens'
  else
    Right (Tuple newDecl tokens')

parseDeclarator :: TokStream.TokStream -> Either String (Tuple Declarator TokStream.TokStream)
parseDeclarator tokens =
  case peekOpt tokens of
    Just Star -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple inner tokens'' <- parseDeclarator tokens'
      Right (Tuple (PointerDeclarator inner) tokens'')
    _ -> parseDirectDeclarator tokens

parseDirectDeclarator :: TokStream.TokStream -> Either String (Tuple Declarator TokStream.TokStream)
parseDirectDeclarator tokens = do
  Tuple simpleDec tokens' <- parseSimpleDeclarator tokens
  case peekOpt tokens' of
    Just OpenParen -> do
      Tuple paramList tokens'' <- parseParamList tokens'
      Right (Tuple (FunDeclarator paramList simpleDec) tokens'')
    Just OpenBracket -> parseArrayDeclSuffix simpleDec tokens'
    _ -> Right (Tuple simpleDec tokens')

parseParamList :: TokStream.TokStream -> Either String (Tuple (List ParamInfo) TokStream.TokStream)
parseParamList tokens =
  if TokStream.npeek 2 tokens == (OpenParen : CloseParen : Nil) then do
    Tuple _ tokens' <- takeToken tokens
    Tuple _ tokens'' <- takeToken tokens'
    Right (Tuple Nil tokens'')
  else if TokStream.npeek 3 tokens == (OpenParen : KWVoid : CloseParen : Nil) then do
    Tuple _ tokens' <- takeToken tokens
    Tuple _ tokens'' <- takeToken tokens'
    Tuple _ tokens''' <- takeToken tokens''
    Right (Tuple Nil tokens''')
  else do
    tokens' <- expect OpenParen tokens
    Tuple paramList tokens'' <- paramLoop tokens'
    tokens''' <- expect CloseParen tokens''
    Right (Tuple paramList tokens''')
  where
  paramLoop :: TokStream.TokStream -> Either String (Tuple (List ParamInfo) TokStream.TokStream)
  paramLoop ts = do
    Tuple nextParam ts' <- parseParam ts
    if peekEq Comma ts' then do
      Tuple _ ts'' <- takeToken ts'
      Tuple rest ts''' <- paramLoop ts''
      Right (Tuple (nextParam : rest) ts''')
    else
      Right (Tuple (nextParam : Nil) ts')

parseParam :: TokStream.TokStream -> Either String (Tuple ParamInfo TokStream.TokStream)
parseParam tokens = do
  Tuple specs tokens' <- parseTypeSpecifierList tokens
  paramT <- parseType specs
  Tuple paramDecl tokens'' <- parseDeclarator tokens'
  Right (Tuple (Param paramT paramDecl) tokens'')

parseSimpleDeclarator :: TokStream.TokStream -> Either String (Tuple Declarator TokStream.TokStream)
parseSimpleDeclarator tokens = do
  Tuple nextTok tokens' <- takeToken tokens
  case nextTok of
    OpenParen -> do
      Tuple decl tokens'' <- parseDeclarator tokens'
      tokens''' <- expect CloseParen tokens''
      Right (Tuple decl tokens''')
    Identifier id -> Right (Tuple (Ident id) tokens')
    other -> Left (formatError (Name "a simple declarator") other)

processDeclarator :: Declarator -> Types.CType -> Either String { name :: String, typ :: Types.CType, params :: List String }
processDeclarator decl baseType =
  case decl of
    Ident s -> Right { name: s, typ: baseType, params: Nil }
    PointerDeclarator d ->
      let derivedType = Types.Pointer baseType
      in processDeclarator d derivedType
    ArrayDeclarator inner size ->
      let derivedType = Types.Array baseType size
      in processDeclarator inner derivedType
    FunDeclarator paramList (Ident s) -> do
      processedParams <- foldParams paramList
      let result = unzip processedParams
      let paramNames = fst result
      let paramTypes = snd result
      let funType = Types.FunType paramTypes baseType
      Right { name: s, typ: funType, params: paramNames }
    FunDeclarator _ _ ->
      Left "can't apply additional type derivations to a function declarator"
  where
  processParam :: ParamInfo -> Either String (Tuple String Types.CType)
  processParam (Param pBaseType pDecl) = do
    result <- processDeclarator pDecl pBaseType
    case result.typ of
      Types.FunType _ _ -> Left "Function pointers in parameters are not supported"
      _ -> Right (Tuple result.name result.typ)

  foldParams :: List ParamInfo -> Either String (List (Tuple String Types.CType))
  foldParams Nil = Right Nil
  foldParams (Cons p rest) = do
    processed <- processParam p
    restProcessed <- foldParams rest
    Right (processed : restProcessed)

parseInitializer :: TokStream.TokStream -> Either String (Tuple Ast.Initializer TokStream.TokStream)
parseInitializer tokens =
  if peekEq OpenBrace tokens then do
    Tuple _ tokens' <- takeToken tokens
    Tuple initList tokens'' <- parseInitLoop tokens'
    tokens''' <- expect CloseBrace tokens''
    Right (Tuple (Ast.CompoundInit initList) tokens''')
  else do
    Tuple e tokens' <- parseExp 0 tokens
    Right (Tuple (Ast.SingleInit e) tokens')
  where
  parseInitLoop :: TokStream.TokStream -> Either String (Tuple (List Ast.Initializer) TokStream.TokStream)
  parseInitLoop ts = do
    Tuple nextInit ts' <- parseInitializer ts
    case TokStream.npeek 2 ts' of
      Cons Comma (Cons CloseBrace Nil) -> do
        Tuple _ ts'' <- takeToken ts'
        Right (Tuple (nextInit : Nil) ts'')
      Cons Comma _ -> do
        Tuple _ ts'' <- takeToken ts'
        Tuple rest ts''' <- parseInitLoop ts''
        Right (Tuple (nextInit : rest) ts''')
      _ -> Right (Tuple (nextInit : Nil) ts')

parseMemberDeclaration :: TokStream.TokStream -> Either String (Tuple Ast.MemberDeclaration TokStream.TokStream)
parseMemberDeclaration tokens = do
  Tuple specifiers tokens' <- parseTypeSpecifierList tokens
  baseType <- parseType specifiers
  Tuple decl tokens'' <- parseDeclarator tokens'
  case decl of
    FunDeclarator _ _ -> Left "Found function declarator in struct member list"
    _ -> do
      tokens''' <- expect Semicolon tokens''
      result <- processDeclarator decl baseType
      Right (Tuple { memberName: result.name, memberType: result.typ } tokens''')

parseStructDeclaration :: TokStream.TokStream -> Either String (Tuple Ast.StructDeclaration TokStream.TokStream)
parseStructDeclaration tokens = do
  tokens' <- expect KWStruct tokens
  Tuple tag tokens'' <- parseId tokens'
  Tuple members tokens''' <- case peekOpt tokens'' of
    Just OpenBrace -> do
      Tuple _ ts <- takeToken tokens''
      Tuple ms ts' <- parseMemberLoop ts
      ts'' <- expect CloseBrace ts'
      Right (Tuple ms ts'')
    _ -> Right (Tuple Nil tokens'')
  tokens'''' <- expect Semicolon tokens'''
  Right (Tuple { tag: tag, members: members } tokens'''')
  where
  parseMemberLoop :: TokStream.TokStream -> Either String (Tuple (List Ast.MemberDeclaration) TokStream.TokStream)
  parseMemberLoop ts = do
    Tuple nextMember ts' <- parseMemberDeclaration ts
    if peekEq CloseBrace ts' then
      Right (Tuple (nextMember : Nil) ts')
    else do
      Tuple rest ts'' <- parseMemberLoop ts'
      Right (Tuple (nextMember : rest) ts'')

parseFunctionOrVariableDeclaration :: TokStream.TokStream -> Either String (Tuple Ast.Declaration TokStream.TokStream)
parseFunctionOrVariableDeclaration tokens = do
  Tuple specifiers tokens' <- parseSpecifierList tokens
  Tuple baseType storageClass <- parseTypeAndStorageClass specifiers
  Tuple decl tokens'' <- parseDeclarator tokens'
  result <- processDeclarator decl baseType
  case result.typ of
    Types.FunType _ _ -> do
      Tuple body tokens''' <- case peekOpt tokens'' of
        Just Semicolon -> do
          Tuple _ ts <- takeToken tokens''
          Right (Tuple Nothing ts)
        _ -> do
          Tuple block ts <- parseBlock tokens''
          Right (Tuple (Just block) ts)
      Right (Tuple (Ast.FunDecl
        { name: result.name
        , funType: result.typ
        , storageClass: storageClass
        , paramList: result.params
        , body: body
        }) tokens''')
    _ -> do
      Tuple init tokens''' <-
        if peekEq EqualSign tokens'' then do
          Tuple _ ts <- takeToken tokens''
          Tuple i ts' <- parseInitializer ts
          Right (Tuple (Just i) ts')
        else
          Right (Tuple Nothing tokens'')
      tokens'''' <- expect Semicolon tokens'''
      Right (Tuple (Ast.VarDecl
        { name: result.name
        , varType: result.typ
        , storageClass: storageClass
        , init: init
        }) tokens'''')

parseDeclaration :: TokStream.TokStream -> Either String (Tuple Ast.Declaration TokStream.TokStream)
parseDeclaration tokens =
  case TokStream.npeek 3 tokens of
    Cons KWStruct (Cons (Identifier _) (Cons OpenBrace Nil)) -> do
      Tuple sd tokens' <- parseStructDeclaration tokens
      Right (Tuple (Ast.StructDecl sd) tokens')
    Cons KWStruct (Cons (Identifier _) (Cons Semicolon Nil)) -> do
      Tuple sd tokens' <- parseStructDeclaration tokens
      Right (Tuple (Ast.StructDecl sd) tokens')
    _ -> parseFunctionOrVariableDeclaration tokens

parseForInit :: TokStream.TokStream -> Either String (Tuple Ast.ForInit TokStream.TokStream)
parseForInit tokens =
  if peekIs isSpecifier tokens then do
    Tuple decl tokens' <- parseDeclaration tokens
    case decl of
      Ast.VarDecl vd -> Right (Tuple (Ast.InitDecl vd) tokens')
      _ -> Left "Found a function declaration in a for loop header"
  else do
    Tuple optE tokens' <- parseOptionalExp Semicolon tokens
    Right (Tuple (Ast.InitExp optE) tokens')

parseStatement :: TokStream.TokStream -> Either String (Tuple Ast.Statement TokStream.TokStream)
parseStatement tokens =
  case peekOpt tokens of
    Just KWReturn -> do
      Tuple _ tokens' <- takeToken tokens
      Tuple optExp tokens'' <- parseOptionalExp Semicolon tokens'
      Right (Tuple (Ast.Return optExp) tokens'')
    Just KWIf -> do
      Tuple _ tokens' <- takeToken tokens
      tokens'' <- expect OpenParen tokens'
      Tuple condition tokens''' <- parseExp 0 tokens''
      tokens'''' <- expect CloseParen tokens'''
      Tuple thenClause tokens5 <- parseStatement tokens''''
      Tuple elseClause tokens6 <-
        if peekEq KWElse tokens5 then do
          Tuple _ ts <- takeToken tokens5
          Tuple e ts' <- parseStatement ts
          Right (Tuple (Just e) ts')
        else
          Right (Tuple Nothing tokens5)
      Right (Tuple (Ast.If condition thenClause elseClause) tokens6)
    Just OpenBrace -> do
      Tuple block tokens' <- parseBlock tokens
      Right (Tuple (Ast.Compound block) tokens')
    Just KWBreak -> do
      Tuple _ tokens' <- takeToken tokens
      tokens'' <- expect Semicolon tokens'
      Right (Tuple (Ast.Break "") tokens'')
    Just KWContinue -> do
      Tuple _ tokens' <- takeToken tokens
      tokens'' <- expect Semicolon tokens'
      Right (Tuple (Ast.Continue "") tokens'')
    Just KWWhile -> do
      Tuple _ tokens' <- takeToken tokens
      tokens'' <- expect OpenParen tokens'
      Tuple condition tokens''' <- parseExp 0 tokens''
      tokens'''' <- expect CloseParen tokens'''
      Tuple body tokens5 <- parseStatement tokens''''
      Right (Tuple (Ast.While condition body "") tokens5)
    Just KWDo -> do
      tokens' <- expect KWDo tokens
      Tuple body tokens'' <- parseStatement tokens'
      tokens''' <- expect KWWhile tokens''
      tokens'''' <- expect OpenParen tokens'''
      Tuple condition tokens5 <- parseExp 0 tokens''''
      tokens6 <- expect CloseParen tokens5
      tokens7 <- expect Semicolon tokens6
      Right (Tuple (Ast.DoWhile body condition "") tokens7)
    Just KWFor -> do
      tokens' <- expect KWFor tokens
      tokens'' <- expect OpenParen tokens'
      Tuple init tokens''' <- parseForInit tokens''
      Tuple condition tokens'''' <- parseOptionalExp Semicolon tokens'''
      Tuple post tokens5 <- parseOptionalExp CloseParen tokens''''
      Tuple body tokens6 <- parseStatement tokens5
      Right (Tuple (Ast.For init condition post body "") tokens6)
    _ -> do
      Tuple optExp tokens' <- parseOptionalExp Semicolon tokens
      case optExp of
        Just e -> Right (Tuple (Ast.Expression e) tokens')
        Nothing -> Right (Tuple Ast.Null tokens')

parseBlockItem :: TokStream.TokStream -> Either String (Tuple Ast.BlockItem TokStream.TokStream)
parseBlockItem tokens =
  if peekIs isSpecifier tokens then do
    Tuple decl tokens' <- parseDeclaration tokens
    Right (Tuple (Ast.Decl decl) tokens')
  else do
    Tuple stmt tokens' <- parseStatement tokens
    Right (Tuple (Ast.Stmt stmt) tokens')

parseBlock :: TokStream.TokStream -> Either String (Tuple Ast.Block TokStream.TokStream)
parseBlock tokens = do
  tokens' <- expect OpenBrace tokens
  Tuple block tokens'' <- parseBlockItemLoop tokens'
  tokens''' <- expect CloseBrace tokens''
  Right (Tuple (Ast.Block block) tokens''')
  where
  parseBlockItemLoop :: TokStream.TokStream -> Either String (Tuple (List Ast.BlockItem) TokStream.TokStream)
  parseBlockItemLoop ts =
    if peekEq CloseBrace ts then
      Right (Tuple Nil ts)
    else do
      Tuple nextBlockItem ts' <- parseBlockItem ts
      Tuple rest ts'' <- parseBlockItemLoop ts'
      Right (Tuple (nextBlockItem : rest) ts'')

parseProgram :: TokStream.TokStream -> Either String Ast.UntypedProgram
parseProgram tokens = do
  Tuple funDecls _ <- parseDeclLoop tokens
  Right (Ast.Program funDecls)
  where
  parseDeclLoop :: TokStream.TokStream -> Either String (Tuple (List Ast.Declaration) TokStream.TokStream)
  parseDeclLoop ts =
    if TokStream.isEmpty ts then
      Right (Tuple Nil ts)
    else do
      Tuple nextDecl ts' <- parseDeclaration ts
      Tuple rest ts'' <- parseDeclLoop ts'
      Right (Tuple (nextDecl : rest) ts'')

parse :: List Token -> Either String Ast.UntypedProgram
parse tokens =
  let tokenStream = TokStream.ofList tokens
  in parseProgram tokenStream
