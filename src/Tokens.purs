module Tokens where

import Prelude

import Data.BigInt (BigInt, toString)

data Token
  -- tokens with contents
  = Identifier String
  | StringLiteral String
  | ConstChar String
  | ConstInt BigInt
  | ConstLong BigInt
  | ConstUInt BigInt
  | ConstULong BigInt
  | ConstDouble Number
  -- Keywords
  | KWInt
  | KWLong
  | KWChar
  | KWSigned
  | KWUnsigned
  | KWDouble
  | KWReturn
  | KWVoid
  | KWIf
  | KWElse
  | KWDo
  | KWWhile
  | KWFor
  | KWBreak
  | KWContinue
  | KWStatic
  | KWExtern
  | KWSizeOf
  | KWStruct
  -- punctuation
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | Semicolon
  | Hyphen
  | DoubleHyphen
  | Tilde
  | Plus
  | Star
  | Slash
  | Percent
  | Bang
  | LogicalAnd
  | LogicalOr
  | DoubleEqual
  | NotEqual
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual
  | EqualSign
  | QuestionMark
  | Colon
  | Comma
  | Ampersand
  | OpenBracket
  | CloseBracket
  | Dot
  | Arrow

derive instance eqToken :: Eq Token
derive instance ordToken :: Ord Token

instance showToken :: Show Token where
  show (Identifier s) = "(Identifier " <> s <> ")"
  show (StringLiteral s) = "(StringLiteral " <> s <> ")"
  show (ConstChar s) = "(ConstChar " <> s <> ")"
  show (ConstInt i) = "(ConstInt " <> toString i <> ")"
  show (ConstLong i) = "(ConstLong " <> toString i <> ")"
  show (ConstUInt i) = "(ConstUInt " <> toString i <> ")"
  show (ConstULong i) = "(ConstULong " <> toString i <> ")"
  show (ConstDouble d) = "(ConstDouble " <> show d <> ")"
  show KWInt = "KWInt"
  show KWLong = "KWLong"
  show KWChar = "KWChar"
  show KWSigned = "KWSigned"
  show KWUnsigned = "KWUnsigned"
  show KWDouble = "KWDouble"
  show KWReturn = "KWReturn"
  show KWVoid = "KWVoid"
  show KWIf = "KWIf"
  show KWElse = "KWElse"
  show KWDo = "KWDo"
  show KWWhile = "KWWhile"
  show KWFor = "KWFor"
  show KWBreak = "KWBreak"
  show KWContinue = "KWContinue"
  show KWStatic = "KWStatic"
  show KWExtern = "KWExtern"
  show KWSizeOf = "KWSizeOf"
  show KWStruct = "KWStruct"
  show OpenParen = "OpenParen"
  show CloseParen = "CloseParen"
  show OpenBrace = "OpenBrace"
  show CloseBrace = "CloseBrace"
  show Semicolon = "Semicolon"
  show Hyphen = "Hyphen"
  show DoubleHyphen = "DoubleHyphen"
  show Tilde = "Tilde"
  show Plus = "Plus"
  show Star = "Star"
  show Slash = "Slash"
  show Percent = "Percent"
  show Bang = "Bang"
  show LogicalAnd = "LogicalAnd"
  show LogicalOr = "LogicalOr"
  show DoubleEqual = "DoubleEqual"
  show NotEqual = "NotEqual"
  show LessThan = "LessThan"
  show GreaterThan = "GreaterThan"
  show LessOrEqual = "LessOrEqual"
  show GreaterOrEqual = "GreaterOrEqual"
  show EqualSign = "EqualSign"
  show QuestionMark = "QuestionMark"
  show Colon = "Colon"
  show Comma = "Comma"
  show Ampersand = "Ampersand"
  show OpenBracket = "OpenBracket"
  show CloseBracket = "CloseBracket"
  show Dot = "Dot"
  show Arrow = "Arrow"
