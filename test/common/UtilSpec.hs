module Common.UtilSpec where

import Common.Util
import SpecHelper

spec :: Spec
spec = do
  describe "unique" $ do
    it "works on empty list" $ do
      unique ([] :: [Int]) `shouldBe` True
    it "works on single element list" $ do
      unique [1 :: Int] `shouldBe` True
    it "can detect duplicates" $ do
      unique [1 :: Int, 1] `shouldBe` False
    it "works on multiple element list" $ do
      unique [1 :: Int, 2, 3] `shouldBe` True
    it "can detect duplicates in multiple element" $ do
      unique [1 :: Int, 2, 1] `shouldBe` False

  describe "maybeToEither" $ do
    it "Just value is mapped to Right" $ do
      maybeToEither "error" (Just 5 :: Maybe Int) `shouldBe` Right 5
    it "Nothing is mapped to Left" $ do
      maybeToEither "error" (Nothing :: Maybe Int) `shouldBe` Left "error"

  describe "guardE" $ do
    it "When predicate is True, value is mapped to Right" $ do
      guardE True "error" (5 :: Int) `shouldBe` Right 5
    it "When predicate is False, error is mapped to Left" $ do
      guardE False "error" (5 :: Int) `shouldBe` Left "error"

  describe "(|>)" $ do
    it "Can be used to apply an Int to a function" $ do
      (5 |> (+ 3)) `shouldBe` (8 :: Int)
    it "Can be used to apply a String to a function" $ do
      ("hello" |> length) `shouldBe` (5 :: Int)
    it "Can be chained from left to right" $ do
      ("Hello" |> length |> odd) `shouldBe` True

  describe "(|>>)" $ do
    it "Can be used to apply a function to a Maybe value" $ do
      (Just 5 |>> (+ 3)) `shouldBe` Just (8 :: Int)
    it "Can be used to apply a function to a List" $ do
      ([1, 2, 3] |>> (+ 3)) `shouldBe` [4, 5, 6 :: Int]
    it "Can be chained from left to right" $ do
      (Just 5 |>> (+ 3) |>> (* 2)) `shouldBe` Just (16 :: Int)
    it "Can be mixed with (|>)" $ do
      ([1 .. 10] |>> (* 10) |> sum) `shouldBe` (550 :: Int)
