module Test.Main where

import Prelude

import Assembly (AsmInstruction(..), AsmOperand(..), AsmReg(..), AsmType(..))
import Ast as Ast
import Const as Const
import Data.BigInt (BigInt, fromInt, fromString)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (launchAff_)
import InstructionFixup (fixupInstruction)
import Lex as Lex
import Parse as Parse
import Partial.Unsafe (unsafePartial)
import Test.TestInt8 as TestInt8
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TestUtil as TestUtil
import ConstConvert as ConstConvert
import TokStream as TokStream
import Tokens (Token(..))
import Types as Types

unsafeBigInt :: String -> BigInt
unsafeBigInt s = unsafePartial (fromJust (fromString s))

unwrap :: forall a. Either String a -> a
unwrap (Right v) = v
unwrap (Left _) = unsafePartial (fromJust Nothing)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Lexer" do
    it "leading whitespace" do
      Lex.lex "   return" `shouldEqual` Right (KWReturn : Nil)

    it "trailing whitespace" do
      Lex.lex "0;\t\n" `shouldEqual` Right (ConstInt (fromInt 0) : Semicolon : Nil)

    it "a full program" do
      Lex.lex "int main(void){return 0;}" `shouldEqual` Right
        ( KWInt
        : Identifier "main"
        : OpenParen
        : KWVoid
        : CloseParen
        : OpenBrace
        : KWReturn
        : ConstInt (fromInt 0)
        : Semicolon
        : CloseBrace
        : Nil
        )

    it "two hyphens" do
      Lex.lex "- -" `shouldEqual` Right (Hyphen : Hyphen : Nil)

    it "double hyphen" do
      Lex.lex "a--" `shouldEqual` Right (Identifier "a" : DoubleHyphen : Nil)

    it "two tildes" do
      Lex.lex "~~" `shouldEqual` Right (Tilde : Tilde : Nil)

  describe "Parser" do
    it "signed long constant" do
      let resultString =
            (ConstLong (unsafeBigInt "4611686018427387904") : Nil)
            # TokStream.ofList
            # Parse.parseConst
            # unwrap
            # fst
            # show
      resultString `shouldEqual` "ConstLong 4611686018427387904L"

    it "unsigned int constant" do
      let resultString =
            (ConstUInt (unsafeBigInt "4294967291") : Nil)
            # TokStream.ofList
            # Parse.parseConst
            # unwrap
            # fst
            # show
      resultString `shouldEqual` "ConstUInt 4294967291U"

    it "unsigned long constant" do
      let resultString =
            (ConstULong (unsafeBigInt "18446744073709551611") : Nil)
            # TokStream.ofList
            # Parse.parseConst
            # unwrap
            # fst
            # show
      resultString `shouldEqual` "ConstULong 18446744073709551611UL"

    it "expression" do
      let result =
            (ConstInt (fromInt 100) : Semicolon : Nil)
            # TokStream.ofList
            # Parse.parseExp 40
            # unwrap
            # fst
      result `shouldEqual` Ast.Constant (Const.ConstInt 100)

    it "statement" do
      let result =
            (KWReturn : ConstInt (fromInt 4) : Semicolon : Nil)
            # TokStream.ofList
            # Parse.parseStatement
            # unwrap
            # fst
      result `shouldEqual` Ast.Return (Just (Ast.Constant (Const.ConstInt 4)))

    it "error" do
      let result = Parse.parse (KWInt : Nil)
      result `shouldSatisfy` case _ of
        Left _ -> true
        Right _ -> false

  describe "ConstConvert" do
    it "preserve int value" do
      let result = unwrap (ConstConvert.constConvert Types.Long (Const.ConstInt 1000))
      result `shouldEqual` Const.ConstLong (fromInt 1000)

    it "preserve negative int value" do
      let result = unwrap (ConstConvert.constConvert Types.Long (Const.ConstInt (-1000)))
      result `shouldEqual` Const.ConstLong (fromInt (-1000))

    it "preserve long value" do
      let result = unwrap (ConstConvert.constConvert Types.Int (Const.ConstLong (fromInt 200000)))
      result `shouldEqual` Const.ConstInt 200000

    it "truncate positive long" do
      let result = unwrap (ConstConvert.constConvert Types.Int (Const.ConstLong (unsafeBigInt "4503599627370501")))
      result `shouldEqual` Const.ConstInt 5

    it "truncate positive long to negative" do
      let result = unwrap (ConstConvert.constConvert Types.Int (Const.ConstLong (unsafeBigInt "4503599627370491")))
      result `shouldEqual` Const.ConstInt (-5)

    it "truncate negative long to zero" do
      let result = unwrap (ConstConvert.constConvert Types.Int (Const.ConstLong (unsafeBigInt "-8589934592")))
      result `shouldEqual` Const.ConstInt 0

    it "truncate negative long to negative" do
      let result = unwrap (ConstConvert.constConvert Types.Int (Const.ConstLong (unsafeBigInt "-8589934692")))
      result `shouldEqual` Const.ConstInt (-100)

    it "trivial uint to int" do
      let result = unwrap (ConstConvert.constConvert Types.Int (Const.ConstUInt (fromInt 100)))
      result `shouldEqual` Const.ConstInt 100

    it "wrapping uint to int" do
      let result = unwrap (ConstConvert.constConvert Types.Int (Const.ConstUInt (unsafeBigInt "4294967200")))
      result `shouldEqual` Const.ConstInt (-96)

    it "trivial int to uint" do
      let result = unwrap (ConstConvert.constConvert Types.UInt (Const.ConstInt 1000))
      result `shouldEqual` Const.ConstUInt (fromInt 1000)

    it "wrapping int to uint" do
      let result = unwrap (ConstConvert.constConvert Types.UInt (Const.ConstInt (-1000)))
      result `shouldEqual` Const.ConstUInt (unsafeBigInt "4294966296")

    it "int to ulong" do
      let result = unwrap (ConstConvert.constConvert Types.ULong (Const.ConstInt (-10)))
      result `shouldEqual` Const.ConstULong (unsafeBigInt "18446744073709551606")

    it "uint to long" do
      let result = unwrap (ConstConvert.constConvert Types.Long (Const.ConstUInt (unsafeBigInt "4294967200")))
      result `shouldEqual` Const.ConstLong (unsafeBigInt "4294967200")

    it "long to uint" do
      let result = unwrap (ConstConvert.constConvert Types.UInt (Const.ConstLong (unsafeBigInt "-9223372036854774574")))
      result `shouldEqual` Const.ConstUInt (fromInt 1234)

    it "ulong to int" do
      let result = unwrap (ConstConvert.constConvert Types.Int (Const.ConstULong (unsafeBigInt "4294967200")))
      result `shouldEqual` Const.ConstInt (-96)

    it "ulong to uint" do
      let result = unwrap (ConstConvert.constConvert Types.UInt (Const.ConstULong (unsafeBigInt "1152921506754330624")))
      result `shouldEqual` Const.ConstUInt (unsafeBigInt "2147483648")

    it "double to long" do
      let result = unwrap (ConstConvert.constConvert Types.Long (Const.ConstDouble 2148429099.3))
      result `shouldEqual` Const.ConstLong (unsafeBigInt "2148429099")

    it "double to int" do
      let result = unwrap (ConstConvert.constConvert Types.Long (Const.ConstDouble (-200000.9999)))
      result `shouldEqual` Const.ConstLong (fromInt (-200000))

    it "double to uint" do
      let result = unwrap (ConstConvert.constConvert Types.UInt (Const.ConstDouble 2147483750.5))
      result `shouldEqual` Const.ConstUInt (unsafeBigInt "2147483750")

    it "double to ulong" do
      let result = unwrap (ConstConvert.constConvert Types.ULong (Const.ConstDouble 3458764513821589504.0))
      result `shouldEqual` Const.ConstULong (unsafeBigInt "3458764513821589504")

    it "int to double" do
      let result = unwrap (ConstConvert.constConvert Types.Double (Const.ConstInt (-1000)))
      result `shouldEqual` Const.ConstDouble (-1000.0)

    it "long to double" do
      let result = unwrap (ConstConvert.constConvert Types.Double (Const.ConstLong (unsafeBigInt "-9007199254751227")))
      result `shouldEqual` Const.ConstDouble (-9007199254751228.0)

    it "uint to double" do
      let result = unwrap (ConstConvert.constConvert Types.Double (Const.ConstUInt (unsafeBigInt "4294967200")))
      result `shouldEqual` Const.ConstDouble 4294967200.0

    it "ulong to double" do
      let result = unwrap (ConstConvert.constConvert Types.Double (Const.ConstULong (unsafeBigInt "138512825844")))
      result `shouldEqual` Const.ConstDouble 138512825844.0

    it "ulong to double inexact" do
      let result = unwrap (ConstConvert.constConvert Types.Double (Const.ConstULong (unsafeBigInt "10223372036854775816")))
      result `shouldEqual` Const.ConstDouble 10223372036854775808.0

    it "ulong to double round to odd" do
      let result = unwrap (ConstConvert.constConvert Types.Double (Const.ConstULong (unsafeBigInt "9223372036854776832")))
      result `shouldEqual` Const.ConstDouble 9223372036854775808.0

    it "ulong to double above halfway" do
      let result = unwrap (ConstConvert.constConvert Types.Double (Const.ConstULong (unsafeBigInt "9223372036854776833")))
      result `shouldEqual` Const.ConstDouble 9223372036854777856.0

    it "signed char to long" do
      let result = unwrap (ConstConvert.constConvert Types.Long (Const.ConstChar (-10)))
      result `shouldEqual` Const.ConstLong (fromInt (-10))

    it "signed char to ulong" do
      let result = unwrap (ConstConvert.constConvert Types.ULong (Const.ConstChar (-10)))
      result `shouldEqual` Const.ConstULong (unsafeBigInt "18446744073709551606")

    it "unsigned char to int" do
      let result = unwrap (ConstConvert.constConvert Types.Int (Const.ConstUChar 255))
      result `shouldEqual` Const.ConstInt 255

    it "signed char to double" do
      let result = unwrap (ConstConvert.constConvert Types.Double (Const.ConstChar (-70)))
      result `shouldEqual` Const.ConstDouble (-70.0)

    it "unsigned char to double" do
      let result = unwrap (ConstConvert.constConvert Types.Double (Const.ConstUChar 200))
      result `shouldEqual` Const.ConstDouble 200.0

    it "long to char" do
      let result = unwrap (ConstConvert.constConvert Types.SChar (Const.ConstLong (fromInt (-1000))))
      result `shouldEqual` Const.ConstChar 24

    it "uint to char" do
      let result = unwrap (ConstConvert.constConvert Types.Char (Const.ConstUInt (unsafeBigInt "2147483858")))
      result `shouldEqual` Const.ConstChar (-46)

    it "ulong to uchar" do
      let result = unwrap (ConstConvert.constConvert Types.UChar (Const.ConstULong (unsafeBigInt "18446744073709551606")))
      result `shouldEqual` Const.ConstUChar 246

    it "int to uchar" do
      let result = unwrap (ConstConvert.constConvert Types.UChar (Const.ConstInt 356))
      result `shouldEqual` Const.ConstUChar 100

    it "double to char" do
      let result = unwrap (ConstConvert.constConvert Types.Char (Const.ConstDouble (-100.8)))
      result `shouldEqual` Const.ConstChar (-100)

    it "double to uchar" do
      let result = unwrap (ConstConvert.constConvert Types.UChar (Const.ConstDouble 250.1234))
      result `shouldEqual` Const.ConstUChar 250

  describe "InstructionFixup" do
    it "truncates large byte immediate to signed byte" do
      let result = fixupInstruction Nil (Mov Byte (Imm (unsafeBigInt "5369233654")) (Reg AX))
          expected = Mov Byte (Imm (fromInt (-10))) (Reg AX) : Nil
      (result == expected) `shouldEqual` true

  TestUtil.spec
  TestInt8.spec
