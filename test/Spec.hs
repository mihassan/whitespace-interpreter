module Main where

import Common.UtilSpec qualified
import Test.Hspec
import Whitespace.DebugSpec qualified
import Whitespace.ProgramSpec qualified

spec :: Spec
spec = do
  describe "Common.Util" Common.UtilSpec.spec
  describe "Whitespace.Debug" Whitespace.DebugSpec.spec
  describe "Whitespace.Program" Whitespace.ProgramSpec.spec

main :: IO ()
main = hspec spec
