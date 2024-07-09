module Main where

import Test.Hspec

spec :: Spec
spec = describe "Your module" $ do
  it "should work" $ do
    (1 + 1) `shouldBe` (2 :: Int)
  it "should fail" $ do
    (1 + 1) `shouldBe` (3 :: Int)

main :: IO ()
main = hspec spec
