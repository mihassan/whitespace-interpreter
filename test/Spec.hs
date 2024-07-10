module Main where

import Whitespace.DebugSpec qualified
import Common.UtilSpec qualified
import Test.Hspec

spec :: Spec
spec = do
  describe "Common.Util" Common.UtilSpec.spec
  describe "Whitespace.Debug" Whitespace.DebugSpec.spec

main :: IO ()
main = hspec spec
