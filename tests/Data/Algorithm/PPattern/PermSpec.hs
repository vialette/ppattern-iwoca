module Data.Algorithm.PPattern.PermSpec where

import SpecHelper

import qualified Data.List as List

import qualified Data.Algorithm.PPattern.Perm as Perm

spec :: Spec
spec = do
  describe "Perm" $ do
    describe "mk" $ do
      it "correctly constructs the empty permutation" $ do
        Perm.mk ([] :: [Int]) `shouldBe` Perm.empty

      it "correctly reduces its input" $ do
        Perm.mk [1,2,3] `shouldBe` Perm.mk [10,20,30]
        Perm.mk [1,3,2] `shouldBe` Perm.mk [10,30,20]
        Perm.mk [2,1,3] `shouldBe` Perm.mk [20,10,30]
        Perm.mk [2,3,1] `shouldBe` Perm.mk [20,30,10]
        Perm.mk [3,1,2] `shouldBe` Perm.mk [30,10,20]
        Perm.mk [3,2,1] `shouldBe` Perm.mk [30,20,10]

    describe "reversal" $ do
      it "correctly reverses" $ do
        let p = Perm.mk [3,5,1,4,2]
        let q = Perm.mk (List.reverse [3,5,1,4,2])
        Perm.reversal p `shouldBe` q

      it "reversal of reversal should produce the initial permutation" $ do
        let p = Perm.mk [3,5,1,4,2]
        Perm.reversal (Perm.reversal p) `shouldBe` p

      it "correctly reverses the null permutation" $ do
        let p = Perm.mk ([] :: [Int])
        Perm.reversal p `shouldBe` Perm.empty

    describe "skewSum" $ do
      it "correctly computes skew sum" $ do
        let p = Perm.mk [2,4,1,3]
        let q = Perm.mk [3,5,1,4,2]
        let r = Perm.mk [7,9,6,8,3,5,1,4,2]
        Perm.skewSum p q `shouldBe` r

      it "correctly computes skew sum with null first permutation" $ do
        let p = Perm.mk ([] :: [Int])
        let q = Perm.mk [3,5,1,4,2]
        Perm.skewSum p q `shouldBe` q

      it "correctly computes skew sum with null second permutation" $ do
        let p = Perm.mk [3,5,1,4,2]
        let q = Perm.mk ([] :: [Int])
        Perm.skewSum p q `shouldBe` p

    context "directSum" $ do
      it "correctly computes direct sum" $ do
        let p = Perm.mk [2,4,1,3]
        let q = Perm.mk [3,5,1,4,2]
        let r = Perm.mk [2,4,1,3,7,9,5,8,6]
        Perm.directSum p q `shouldBe` r

      it "correctly computes direct sum with null first permutation" $ do
        let p = Perm.mk ([] :: [Int])
        let q = Perm.mk [3,5,1,4,2]
        Perm.directSum p q `shouldBe` q

      it "correctly computes direct sum with null second permutation" $ do
        let p = Perm.mk [3,5,1,4,2]
        let q = Perm.mk ([] :: [Int])
        Perm.directSum p q `shouldBe` p

main :: IO ()
main = hspec spec
