module Data.Algorithm.PPattern.APermSpec where

import SpecHelper

import qualified Data.List as List

import qualified Data.Algorithm.PPattern.APerm as APerm

spec :: Spec
spec = do
  describe "APerm" $ do
    describe "mk" $ do
      it "correctly constructs the empty APermutation" $ do
        APerm.mk ([] :: [Int]) `shouldBe` APerm.empty

      it "correctly reduces its input" $ do
        APerm.mk [1,2,3] `shouldBe` APerm.mk [10,20,30]
        APerm.mk [1,3,2] `shouldBe` APerm.mk [10,30,20]
        APerm.mk [2,1,3] `shouldBe` APerm.mk [20,10,30]
        APerm.mk [2,3,1] `shouldBe` APerm.mk [20,30,10]
        APerm.mk [3,1,2] `shouldBe` APerm.mk [30,10,20]
        APerm.mk [3,2,1] `shouldBe` APerm.mk [30,20,10]

    describe "reversal" $ do
      it "correctly reverses" $ do
        let p = APerm.mk [3,5,1,4,2]
        let q = APerm.mk (List.reverse [3,5,1,4,2])
        APerm.reversal p `shouldBe` q

      it "reversal of reversal should produce the initial APermutation" $ do
        let p = APerm.mk [3,5,1,4,2]
        APerm.reversal (APerm.reversal p) `shouldBe` p

      it "correctly reverses the null APermutation" $ do
        let p = APerm.mk ([] :: [Int])
        APerm.reversal p `shouldBe` APerm.empty

    describe "skewSum" $ do
      it "correctly computes skew sum" $ do
        let p = APerm.mk [2,4,1,3]
        let q = APerm.mk [3,5,1,4,2]
        let r = APerm.mk [7,9,6,8,3,5,1,4,2]
        APerm.skewSum p q `shouldBe` r

      it "correctly computes skew sum with null first APermutation" $ do
        let p = APerm.mk ([] :: [Int])
        let q = APerm.mk [3,5,1,4,2]
        APerm.skewSum p q `shouldBe` q

      it "correctly computes skew sum with null second APermutation" $ do
        let p = APerm.mk [3,5,1,4,2]
        let q = APerm.mk ([] :: [Int])
        APerm.skewSum p q `shouldBe` p

    context "directSum" $ do
      it "correctly computes direct sum" $ do
        let p = APerm.mk [2,4,1,3]
        let q = APerm.mk [3,5,1,4,2]
        let r = APerm.mk [2,4,1,3,7,9,5,8,6]
        APerm.directSum p q `shouldBe` r

      it "correctly computes direct sum with null first APermutation" $ do
        let p = APerm.mk ([] :: [Int])
        let q = APerm.mk [3,5,1,4,2]
        APerm.directSum p q `shouldBe` q

      it "correctly computes direct sum with null second APermutation" $ do
        let p = APerm.mk [3,5,1,4,2]
        let q = APerm.mk ([] :: [Int])
        APerm.directSum p q `shouldBe` p

main :: IO ()
main = hspec spec
