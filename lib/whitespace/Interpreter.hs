module Whitespace.Interpreter where

import Common.Util
import Data.Char
import Whitespace.Process
import Whitespace.Program

type ErrorMsg = String

type Result = Either ErrorMsg String

runProcess :: Process -> Maybe Process
runProcess p | running p = do
  c <- command p
  p' <- runCommand c p
  runProcess p'
runProcess p = Just p

runCommand :: Command -> Process -> Maybe Process
runCommand (CmdStack c) p = runCmdStack c p >>= incIp
runCommand (CmdArith c) p = runCmdArith c p >>= incIp
runCommand (CmdHeap c) p = runCmdHeap c p >>= incIp
runCommand (CmdIO c) p = runCmdIO c p >>= incIp
runCommand (CmdFlow c) p = runCmdFlow c p

runCmdStack :: CmdStack -> Process -> Maybe Process
runCmdStack (CmdStackPush n) p = Just $ push p n
runCmdStack (CmdStackDup n) p = get p n >>= (Just . push p)
runCmdStack (CmdStackDiscard n) p = discard p n
runCmdStack CmdStackDupTop p = get p 0 >>= (Just . push p)
runCmdStack CmdStackSwap p = swap p
runCmdStack CmdStackDiscardTop p = pop p |>> snd

runCmdArith :: CmdArith -> Process -> Maybe Process
runCmdArith c p = binOp p (arithOp c)

arithOp :: CmdArith -> Number -> Number -> Maybe Number
arithOp CmdArithAdd a b = Just $ b + a
arithOp CmdArithSub a b = Just $ b - a
arithOp CmdArithMul a b = Just $ b * a
arithOp CmdArithDiv 0 _ = Nothing
arithOp CmdArithDiv a b = Just $ b `div` a
arithOp CmdArithMod 0 _ = Nothing
arithOp CmdArithMod a b = Just $ b `mod` a

runCmdHeap :: CmdHeap -> Process -> Maybe Process
runCmdHeap CmdHeapStore p = do
  (a, p') <- pop p
  (b, p'') <- pop p'
  pure $ store p'' b a
runCmdHeap CmdHeapLoad p = do
  (a, p') <- pop p
  load p' a

runCmdIO :: CmdIO -> Process -> Maybe Process
runCmdIO CmdIOPrintChar p = do
  (x, p') <- pop p
  pure $ write p' (chr x : "")
runCmdIO CmdIOPrintNum p = do
  (x, p') <- pop p
  pure $ write p' (show x)
runCmdIO CmdIOReadChar p = do
  (a, p') <- readChar p
  (b, p'') <- pop p'
  pure $ store p'' b (ord a)
runCmdIO CmdIOReadNum p = do
  (a, p') <- readInt p
  (b, p'') <- pop p'
  pure $ store p'' b a

runCmdFlow :: CmdFlow -> Process -> Maybe Process
runCmdFlow (CmdFlowMark _) p = incIp p -- All labels are processed at the beginning. So, just move to the next instruction.
runCmdFlow (CmdFlowSub l) p = sub p l
runCmdFlow (CmdFlowJump l) p = jump p l
runCmdFlow (CmdFlowJumpIfZero l) p = do
  (x, p') <- pop p
  if x == 0 then jump p' l else incIp p'
runCmdFlow (CmdFlowJumpIfNeg l) p = do
  (x, p') <- pop p
  if x < 0 then jump p' l else incIp p'
runCmdFlow CmdFlowRet p = ret p
runCmdFlow CmdFlowExit p = Just $ exit p

getResult :: Maybe Process -> Result
getResult Nothing = Left ""
getResult (Just p) = Right $ output p
