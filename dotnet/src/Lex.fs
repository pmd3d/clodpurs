module Lexer

open System.Text.RegularExpressions

module T = Tokens

open ResultCE

type TokenDef =
    { re: Regex
      group: int
      converter: string -> Tokens.Token }

type MatchDef =
    { matchedSubstring: string
      matchingToken: TokenDef }

(* Functions to convert individual tokens from string to Tok.t *)

let literal tok _s = tok

let convertIdentifier =
    function
    | "int" -> T.KWInt
    | "return" -> T.KWReturn
    | "void" -> T.KWVoid
    | "if" -> T.KWIf
    | "else" -> T.KWElse
    | "do" -> T.KWDo
    | "while" -> T.KWWhile
    | "for" -> T.KWFor
    | "break" -> T.KWBreak
    | "continue" -> T.KWContinue
    | "static" -> T.KWStatic
    | "extern" -> T.KWExtern
    | "long" -> T.KWLong
    | "unsigned" -> T.KWUnsigned
    | "signed" -> T.KWSigned
    | "double" -> T.KWDouble
    | "char" -> T.KWChar
    | "sizeof" -> T.KWSizeOf
    | "struct" -> T.KWStruct
    | other -> T.Identifier other

let convertInt s =
    T.ConstInt(System.Numerics.BigInteger.Parse(s))

let convertLong s =
    let constStr = StringUtil.chopSuffix1 s
    T.ConstLong(System.Numerics.BigInteger.Parse(constStr))

let convertUInt s =
    let constStr = StringUtil.chopSuffix1 s
    T.ConstUInt(System.Numerics.BigInteger.Parse(constStr))

let convertULong s =
    let constStr = StringUtil.chopSuffix 2 s
    T.ConstULong(System.Numerics.BigInteger.Parse(constStr))

let convertDouble s =
    T.ConstDouble(float s)

let convertChar s =
    let ch = s |> StringUtil.chopSuffix1 |> StringUtil.drop 1
    T.ConstChar ch

let convertString s =
    let str = s |> StringUtil.chopSuffix1 |> StringUtil.drop 1
    T.StringLiteral str

let tokenDefs =
    let def (group: int) (reStr: string) (converter: string -> Tokens.Token) =
        { re = Regex(@"\A" + reStr, RegexOptions.None)
          group = group
          converter = converter }
    let def0 reStr converter = def 0 reStr converter
    [ (* all identifiers, including keywords *)
      def0 @"[A-Za-z_][A-Za-z0-9_]*\b" convertIdentifier
      (* constants *)
      def 1 @"([0-9]+)[^\w.]" convertInt
      def 1 @"([0-9]+[lL])[^\w.]" convertLong
      def 1 @"([0-9]+[uU])[^\w.]" convertUInt
      def 1 @"([0-9]+([lL][uU]|[uU][lL]))[^\w.]" convertULong
      def 1 @"(([0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)[^\w.]" convertDouble
      def0 @"'([^'\\\n]|\\['\""?\\abfnrtv])'" convertChar
      (* string literals *)
      def0 @"""([^""\\\n]|\\['\""\\?abfnrtv])*""" convertString
      (* punctuation *)
      def0 @"\(" (literal T.OpenParen)
      def0 @"\)" (literal T.CloseParen)
      def0 @"\{" (literal T.OpenBrace)
      def0 @"\}" (literal T.CloseBrace)
      def0 @";" (literal T.Semicolon)
      def0 @"-" (literal T.Hyphen)
      def0 @"--" (literal T.DoubleHyphen)
      def0 @"~" (literal T.Tilde)
      def0 @"\+" (literal T.Plus)
      def0 @"\*" (literal T.Star)
      def0 @"/" (literal T.Slash)
      def0 @"%" (literal T.Percent)
      def0 @"!" (literal T.Bang)
      def0 @"&&" (literal T.LogicalAnd)
      def0 @"\|\|" (literal T.LogicalOr)
      def0 @"==" (literal T.DoubleEqual)
      def0 @"!=" (literal T.NotEqual)
      def0 @"<" (literal T.LessThan)
      def0 @">" (literal T.GreaterThan)
      def0 @"<=" (literal T.LessOrEqual)
      def0 @">=" (literal T.GreaterOrEqual)
      def0 @"=" (literal T.EqualSign)
      def0 @"\?" (literal T.QuestionMark)
      def0 @":" (literal T.Colon)
      def0 @"," (literal T.Comma)
      def0 @"&" (literal T.Ampersand)
      def0 @"\[" (literal T.OpenBracket)
      def0 @"\]" (literal T.CloseBracket)
      def0 @"->" (literal T.Arrow)
      (* . operator must be followed by non-digit *)
      def 1 @"(\.)[^\d]" (literal T.Dot) ]

let findMatch s tokDef =
    let m = tokDef.re.Match(s)
    if m.Success then
        Some
            { matchedSubstring = m.Groups.[tokDef.group].Value
              matchingToken = tokDef }
    else
        None

let countLeadingWs s =
    let wsMatcher = Regex(@"\A\s+")
    let m = wsMatcher.Match(s)
    if m.Success then Some m.Length
    else None

let rec lex input : Result<Tokens.Token list, string> =
    if input = "" then
        Ok []
    else
        match countLeadingWs input with
        | Some wsCount -> lex (StringUtil.drop wsCount input)
        | None ->
            let matches: MatchDef List = List.choose (findMatch input) tokenDefs
            if matches.Length = 0 then
                Error input
            else
                let longestMatch =
                    matches
                    |> List.maxBy (fun m -> m.matchedSubstring.Length)
                let converter = longestMatch.matchingToken.converter
                let matchingSubstring = longestMatch.matchedSubstring
                let nextTok = converter matchingSubstring
                let remaining =
                    StringUtil.drop
                        longestMatch.matchedSubstring.Length
                        input
                result {
                    let! rest = lex remaining
                    return nextTok :: rest
                }