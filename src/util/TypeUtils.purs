module TypeUtils where

import Prelude

import CompilerError (CompilerError(..))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import TypeTable (TypeTableMap)
import TypeTable as TypeTable
import TypedAst as TypedAst
import Types (CType(..))

getType :: TypedAst.Exp -> CType
getType e = e.t

setType :: TypedAst.InnerExp -> CType -> TypedAst.Exp
setType e newType = { e: e, t: newType }

getSize :: TypeTableMap -> CType -> Either CompilerError BigInt
getSize tt = case _ of
  Char -> Right (BigInt.fromInt 1)
  SChar -> Right (BigInt.fromInt 1)
  UChar -> Right (BigInt.fromInt 1)
  Int -> Right (BigInt.fromInt 4)
  UInt -> Right (BigInt.fromInt 4)
  Long -> Right (BigInt.fromInt 8)
  ULong -> Right (BigInt.fromInt 8)
  Double -> Right (BigInt.fromInt 8)
  Pointer _ -> Right (BigInt.fromInt 8)
  Array elemType size -> map (\s -> size * s) (getSize tt elemType)
  Structure tag -> map (\sd -> BigInt.fromInt sd.size) (TypeTable.find tag tt)
  t@(FunType _ _) -> Left (InternalError ("type doesn't have size: " <> show t))
  Void -> Left (InternalError ("type doesn't have size: " <> show Void))

getAlignment :: TypeTableMap -> CType -> Either CompilerError Int
getAlignment tt = case _ of
  Char -> Right 1
  SChar -> Right 1
  UChar -> Right 1
  Int -> Right 4
  UInt -> Right 4
  Long -> Right 8
  ULong -> Right 8
  Double -> Right 8
  Pointer _ -> Right 8
  Array elemType _ -> getAlignment tt elemType
  Structure tag -> map _.alignment (TypeTable.find tag tt)
  t@(FunType _ _) -> Left (InternalError ("type doesn't have alignment: " <> show t))
  Void -> Left (InternalError ("type doesn't have alignment: " <> show Void))

isSigned :: CType -> Either CompilerError Boolean
isSigned = case _ of
  Int -> Right true
  Long -> Right true
  Char -> Right true
  SChar -> Right true
  UInt -> Right false
  ULong -> Right false
  Pointer _ -> Right false
  UChar -> Right false
  t -> Left (InternalError ("signedness doesn't make sense for non-integral type " <> show t))

isPointer :: CType -> Boolean
isPointer (Pointer _) = true
isPointer _ = false

isInteger :: CType -> Boolean
isInteger = case _ of
  Char -> true
  UChar -> true
  SChar -> true
  Int -> true
  UInt -> true
  Long -> true
  ULong -> true
  _ -> false

isArray :: CType -> Boolean
isArray (Array _ _) = true
isArray _ = false

isCharacter :: CType -> Boolean
isCharacter = case _ of
  Char -> true
  SChar -> true
  UChar -> true
  _ -> false

isArithmetic :: CType -> Boolean
isArithmetic = case _ of
  Int -> true
  UInt -> true
  Long -> true
  ULong -> true
  Char -> true
  UChar -> true
  SChar -> true
  Double -> true
  _ -> false

isScalar :: CType -> Boolean
isScalar = case _ of
  Int -> true
  UInt -> true
  Long -> true
  ULong -> true
  Char -> true
  UChar -> true
  SChar -> true
  Double -> true
  Pointer _ -> true
  _ -> false

isComplete :: TypeTableMap -> CType -> Boolean
isComplete tt = case _ of
  Void -> false
  Structure tag -> TypeTable.mem tag tt
  _ -> true

isCompletePointer :: TypeTableMap -> CType -> Boolean
isCompletePointer tt (Pointer t) = isComplete tt t
isCompletePointer _ _ = false
