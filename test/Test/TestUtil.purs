module Test.TestUtil where

import Prelude

import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import ListUtil (take, takeDrop)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "ListUtil" do
  it "takeDrop empty_0" do
    takeDrop 0 (Nil :: List Char) `shouldEqual` Tuple Nil Nil

  it "takeDrop empty_n" do
    takeDrop 10 (Nil :: List Char) `shouldEqual` Tuple Nil Nil

  it "takeDrop one" do
    takeDrop 1 ('a' : 'b' : 'c' : Nil) `shouldEqual` Tuple ('a' : Nil) ('b' : 'c' : Nil)

  it "takeDrop three" do
    takeDrop 3 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil) `shouldEqual` Tuple (1 : 2 : 3 : Nil) (4 : 5 : 6 : 7 : Nil)

  it "takeDrop n_gt_len" do
    takeDrop 10 ('a' : 'b' : 'c' : Nil) `shouldEqual` Tuple ('a' : 'b' : 'c' : Nil) Nil

  it "takeDrop all" do
    takeDrop 3 ('a' : 'b' : 'c' : Nil) `shouldEqual` Tuple ('a' : 'b' : 'c' : Nil) Nil

  it "take zero" do
    take 0 (1 : 2 : 3 : 4 : 5 : Nil) `shouldEqual` (Nil :: List Int)

  it "take all" do
    take 10 (1 : 2 : 3 : 4 : 5 : Nil) `shouldEqual` (1 : 2 : 3 : 4 : 5 : Nil)

  it "take some" do
    take 3 (1 : 2 : 3 : 4 : 5 : Nil) `shouldEqual` (1 : 2 : 3 : Nil)
