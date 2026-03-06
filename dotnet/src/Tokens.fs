module Tokens

open System.Numerics

type Token =
    (* tokens with contents *)
    | Identifier of string
    | StringLiteral of string
    | ConstChar of string
    | ConstInt of BigInteger
    | ConstLong of BigInteger
    | ConstUInt of BigInteger
    | ConstULong of BigInteger
    | ConstDouble of float
    (* Keywords *)
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
    (* punctuation *)
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
    | Bang (* ! *)
    | LogicalAnd (* && *)
    | LogicalOr (* || *)
    | DoubleEqual (* == *)
    | NotEqual (* != *)
    | LessThan
    | GreaterThan
    | LessOrEqual
    | GreaterOrEqual
    | EqualSign (* = *)
    | QuestionMark
    | Colon
    | Comma
    | Ampersand
    | OpenBracket
    | CloseBracket
    | Dot
    | Arrow

let show (tok: Token) : string =
    match tok with
    | Identifier s -> sprintf "(Identifier %s)" s
    | StringLiteral s -> sprintf "(StringLiteral %s)" s
    | ConstChar s -> sprintf "(ConstChar %s)" s
    | ConstInt i -> sprintf "(ConstInt %O)" i
    | ConstLong i -> sprintf "(ConstLong %O)" i
    | ConstUInt i -> sprintf "(ConstUInt %O)" i
    | ConstULong i -> sprintf "(ConstULong %O)" i
    | ConstDouble d -> sprintf "(ConstDouble %s)" (string d)
    | KWInt -> "KWInt"
    | KWLong -> "KWLong"
    | KWChar -> "KWChar"
    | KWSigned -> "KWSigned"
    | KWUnsigned -> "KWUnsigned"
    | KWDouble -> "KWDouble"
    | KWReturn -> "KWReturn"
    | KWVoid -> "KWVoid"
    | KWIf -> "KWIf"
    | KWElse -> "KWElse"
    | KWDo -> "KWDo"
    | KWWhile -> "KWWhile"
    | KWFor -> "KWFor"
    | KWBreak -> "KWBreak"
    | KWContinue -> "KWContinue"
    | KWStatic -> "KWStatic"
    | KWExtern -> "KWExtern"
    | KWSizeOf -> "KWSizeOf"
    | KWStruct -> "KWStruct"
    | OpenParen -> "OpenParen"
    | CloseParen -> "CloseParen"
    | OpenBrace -> "OpenBrace"
    | CloseBrace -> "CloseBrace"
    | Semicolon -> "Semicolon"
    | Hyphen -> "Hyphen"
    | DoubleHyphen -> "DoubleHyphen"
    | Tilde -> "Tilde"
    | Plus -> "Plus"
    | Star -> "Star"
    | Slash -> "Slash"
    | Percent -> "Percent"
    | Bang -> "Bang"
    | LogicalAnd -> "LogicalAnd"
    | LogicalOr -> "LogicalOr"
    | DoubleEqual -> "DoubleEqual"
    | NotEqual -> "NotEqual"
    | LessThan -> "LessThan"
    | GreaterThan -> "GreaterThan"
    | LessOrEqual -> "LessOrEqual"
    | GreaterOrEqual -> "GreaterOrEqual"
    | EqualSign -> "EqualSign"
    | QuestionMark -> "QuestionMark"
    | Colon -> "Colon"
    | Comma -> "Comma"
    | Ampersand -> "Ampersand"
    | OpenBracket -> "OpenBracket"
    | CloseBracket -> "CloseBracket"
    | Dot -> "Dot"
    | Arrow -> "Arrow"

let equal (a: Token) (b: Token) : bool = a = b

let compare (a: Token) (b: Token) : int =
    Operators.compare a b