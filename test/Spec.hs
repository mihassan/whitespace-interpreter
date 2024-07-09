module Main where

import Common.UtilSpec qualified
import Test.Hspec

spec :: Spec
spec = do
  describe "Common.Util" Common.UtilSpec.spec

main :: IO ()
main = hspec spec
