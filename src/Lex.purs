module Lex where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Foldable (maximumBy)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String.CodeUnits as SCU
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafeCrashWith)
import Tokens (Token(..))

foreign import parseDoubleImpl :: String -> Nullable Number

type TokenDef =
  { re :: Regex
  , group :: Int
  , converter :: String -> Token
  }

type MatchDef =
  { matchedSubstring :: String
  , matchingToken :: TokenDef
  }

-- Helpers

unsafeRegex :: String -> Regex
unsafeRegex s = case regex s noFlags of
  Right r -> r
  Left err -> unsafeCrashWith ("Bad regex: " <> err)

unsafeParseBigInt :: String -> BigInt
unsafeParseBigInt s = case BigInt.fromString s of
  Just n -> n
  Nothing -> unsafeCrashWith ("Failed to parse BigInt: " <> s)

-- Converter functions

literal :: Token -> String -> Token
literal tok _ = tok

convertIdentifier :: String -> Token
convertIdentifier = case _ of
  "int" -> KWInt
  "return" -> KWReturn
  "void" -> KWVoid
  "if" -> KWIf
  "else" -> KWElse
  "do" -> KWDo
  "while" -> KWWhile
  "for" -> KWFor
  "break" -> KWBreak
  "continue" -> KWContinue
  "static" -> KWStatic
  "extern" -> KWExtern
  "long" -> KWLong
  "unsigned" -> KWUnsigned
  "signed" -> KWSigned
  "double" -> KWDouble
  "char" -> KWChar
  "sizeof" -> KWSizeOf
  "struct" -> KWStruct
  other -> Identifier other

convertInt :: String -> Token
convertInt s = ConstInt (unsafeParseBigInt s)

convertLong :: String -> Token
convertLong s = ConstLong (unsafeParseBigInt (SCU.dropRight 1 s))

convertUInt :: String -> Token
convertUInt s = ConstUInt (unsafeParseBigInt (SCU.dropRight 1 s))

convertULong :: String -> Token
convertULong s = ConstULong (unsafeParseBigInt (SCU.dropRight 2 s))

convertDouble :: String -> Token
convertDouble s = case toMaybe (parseDoubleImpl s) of
  Just n -> ConstDouble n
  Nothing -> unsafeCrashWith ("Failed to parse double: " <> s)

convertChar :: String -> Token
convertChar s = ConstChar (SCU.drop 1 (SCU.dropRight 1 s))

convertString :: String -> Token
convertString s = StringLiteral (SCU.drop 1 (SCU.dropRight 1 s))

-- Token definitions

tokenDefs :: Array TokenDef
tokenDefs =
  let
    def :: Int -> String -> (String -> Token) -> TokenDef
    def group reStr converter =
      { re: unsafeRegex ("^" <> reStr)
      , group
      , converter
      }
    def0 :: String -> (String -> Token) -> TokenDef
    def0 = def 0
  in
    [ -- identifiers and keywords
      def0 "[A-Za-z_][A-Za-z0-9_]*\\b" convertIdentifier
      -- constants
    , def 1 "([0-9]+)[^\\w.]" convertInt
    , def 1 "([0-9]+[lL])[^\\w.]" convertLong
    , def 1 "([0-9]+[uU])[^\\w.]" convertUInt
    , def 1 "([0-9]+([lL][uU]|[uU][lL]))[^\\w.]" convertULong
    , def 1 "(([0-9]*\\.[0-9]+|[0-9]+\\.?)[Ee][+-]?[0-9]+|[0-9]*\\.[0-9]+|[0-9]+\\.)[^\\w.]" convertDouble
    , def0 "'([^'\\\\\\n]|\\\\['\"?\\\\abfnrtv])'" convertChar
      -- string literals
    , def0 "\"([^\"\\\\\\n]|\\\\['\"\\\\?abfnrtv])*\"" convertString
      -- punctuation
    , def0 "\\(" (literal OpenParen)
    , def0 "\\)" (literal CloseParen)
    , def0 "\\{" (literal OpenBrace)
    , def0 "\\}" (literal CloseBrace)
    , def0 ";" (literal Semicolon)
    , def0 "-" (literal Hyphen)
    , def0 "--" (literal DoubleHyphen)
    , def0 "~" (literal Tilde)
    , def0 "\\+" (literal Plus)
    , def0 "\\*" (literal Star)
    , def0 "/" (literal Slash)
    , def0 "%" (literal Percent)
    , def0 "!" (literal Bang)
    , def0 "&&" (literal LogicalAnd)
    , def0 "\\|\\|" (literal LogicalOr)
    , def0 "==" (literal DoubleEqual)
    , def0 "!=" (literal NotEqual)
    , def0 "<" (literal LessThan)
    , def0 ">" (literal GreaterThan)
    , def0 "<=" (literal LessOrEqual)
    , def0 ">=" (literal GreaterOrEqual)
    , def0 "=" (literal EqualSign)
    , def0 "\\?" (literal QuestionMark)
    , def0 ":" (literal Colon)
    , def0 "," (literal Comma)
    , def0 "&" (literal Ampersand)
    , def0 "\\[" (literal OpenBracket)
    , def0 "\\]" (literal CloseBracket)
    , def0 "->" (literal Arrow)
      -- dot operator must be followed by non-digit
    , def 1 "(\\.)([^\\d])" (literal Dot)
    ]

-- Core lexer functions

wsRegex :: Regex
wsRegex = unsafeRegex "^\\s+"

countLeadingWs :: String -> Maybe Int
countLeadingWs s = case match wsRegex s of
  Just groups -> case NEA.head groups of
    Just m -> Just (SCU.length m)
    Nothing -> Nothing
  Nothing -> Nothing

findMatch :: String -> TokenDef -> Maybe MatchDef
findMatch s tokDef = case match tokDef.re s of
  Just groups ->
    let
      extracted = join (NEA.index groups tokDef.group)
    in
      case extracted of
        Just matchStr ->
          Just { matchedSubstring: matchStr, matchingToken: tokDef }
        Nothing -> Nothing
  Nothing -> Nothing

lex :: String -> Either String (List Token)
lex input =
  if input == "" then Right Nil
  else case countLeadingWs input of
    Just wsCount -> lex (SCU.drop wsCount input)
    Nothing ->
      let
        matches = Array.mapMaybe (findMatch input) tokenDefs
      in
        case maximumBy (comparing (\m -> SCU.length m.matchedSubstring)) matches of
          Nothing -> Left input
          Just longestMatch ->
            let
              nextTok = longestMatch.matchingToken.converter longestMatch.matchedSubstring
              remaining = SCU.drop (SCU.length longestMatch.matchedSubstring) input
            in
              case lex remaining of
                Left err -> Left err
                Right rest -> Right (Cons nextTok rest)
