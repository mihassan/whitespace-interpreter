module Main where

import Common.UtilSpec qualified
import Test.Hspec
import Whitespace.DebugSpec qualified
import Whitespace.InterpreterSpec qualified
import Whitespace.ProcessSpec qualified
import Whitespace.ProgramSpec qualified

spec :: Spec
spec = do
  describe "Common.Util" Common.UtilSpec.spec
  describe "Whitespace.Debug" Whitespace.DebugSpec.spec
  describe "Whitespace.Interpreter" Whitespace.InterpreterSpec.spec
  describe "Whitespace.Program" Whitespace.ProgramSpec.spec
  describe "Whitespace.Process" Whitespace.ProcessSpec.spec

main :: IO ()
main = hspec spec
