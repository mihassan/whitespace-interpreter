{-# LANGUAGE RecordWildCards #-}

module Whitespace.Interpreter where

import Common.Util
import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Text.Read
import Whitespace.Program
import Whitespace.Tokenizer (Token (..))

data ProcessStatus = Running | Exit deriving (Eq, Show)

type ErrorMsg = String

type Result = Either ErrorMsg String

data Process = Process
  { program :: Program,
    input :: String,
    output :: String,
    stack :: [Number],
    heap :: Map Number Number,
    labels :: Map Label Int,
    callStack :: [Int],
    ip :: Int,
    status :: ProcessStatus
  }
  deriving (Eq, Show)

mkProcess :: String -> Program -> Process
mkProcess input program = Process {output = "", stack = [], heap = Map.empty, labels = Map.empty, callStack = [], ip = 0, status = Running, ..}

markLabels :: Process -> Maybe Process
markLabels Process {..} = do
  let ls = [(l, i) | (CmdFlow (CmdFlowMark l), i) <- zip program [0 ..]]
  guard . not . hasDuplicates $ fst <$> ls
  pure $ Process {labels = Map.fromList ls, ..}

runProcess :: Process -> Maybe Process
runProcess p@(Process {status = Exit, ..}) = Just p
runProcess p@(Process {..}) = do
  c <- nextCommand program ip
  p' <- runCommand c p
  runProcess p'

nextCommand :: Program -> Int -> Maybe Command
nextCommand p i = p !? i

push :: Process -> Number -> Process
push Process {..} n = Process {stack = n : stack, ..}

discard :: Process -> Number -> Process
discard Process {..} n = case stack of
  (x : xs) -> Process {stack = x : drop n' xs, ..}
  otehrwise -> Process {..}
  where
    n' = if n >= 0 then n else length stack

readInt :: Process -> Maybe (Int, Process)
readInt Process {..} = do
  guard . not . null $ input
  let (x : xs) = lines input
  i <- readMaybe x
  pure (i, Process {input = unlines xs, ..})

readChar :: Process -> Maybe (Char, Process)
readChar Process {..} = do
  guard . not . null $ input
  let (x : xs) = input
  pure (x, Process {input = xs, ..})

incIp :: Process -> Process
incIp p = p {ip = ip p + 1}

runCommand :: Command -> Process -> Maybe Process
runCommand (CmdStack c) = fmap incIp <$> runCmdStack c
runCommand (CmdArith c) = fmap incIp <$> runCmdArith c
runCommand (CmdHeap c) = fmap incIp <$> runCmdHeap c
runCommand (CmdIO c) = fmap incIp <$> runCmdIO c
runCommand (CmdFlow c) = runCmdFlow c

runCmdStack :: CmdStack -> Process -> Maybe Process
runCmdStack (CmdStackPush n) p = Just $ push p n
runCmdStack (CmdStackDup n) p@(Process {..}) = stack !? n |>> push p
runCmdStack (CmdStackDiscard n) p@(Process {..})
  | null stack = Nothing
  | otherwise = Just $ discard p n
runCmdStack CmdStackDupTop Process {..} = case stack of
  (x : xs) -> Just Process {stack = x : x : xs, ..}
  otherwise -> Nothing
runCmdStack CmdStackSwap Process {..} = case stack of
  (x : y : ys) -> Just Process {stack = y : x : ys, ..}
  otherwise -> Nothing
runCmdStack CmdStackDiscardTop Process {..}
  | null stack = Nothing
  | otherwise = Just Process {stack = tail stack, ..}

arithOp :: CmdArith -> Number -> Number -> Maybe Number
arithOp CmdArithAdd a b = Just $ b + a
arithOp CmdArithSub a b = Just $ b - a
arithOp CmdArithMul a b = Just $ b * a
arithOp CmdArithDiv 0 b = Nothing
arithOp CmdArithDiv a b = Just $ b `div` a
arithOp CmdArithMod 0 b = Nothing
arithOp CmdArithMod a b = Just $ b `mod` a

runCmdArith :: CmdArith -> Process -> Maybe Process
runCmdArith c Process {..} = case stack of
  (a : b : rs) -> arithOp c a b |>> \x -> Process {stack = x : rs, ..}
  otherwise -> Nothing

runCmdHeap :: CmdHeap -> Process -> Maybe Process
runCmdHeap CmdHeapStore Process {..} = case stack of
  (a : b : rs) -> Just $ Process {stack = rs, heap = Map.insert b a heap, ..}
  otherwise -> Nothing
runCmdHeap CmdHeapLoad Process {..} = case stack of
  (a : rs) -> Map.lookup a heap |>> \x -> Process {stack = x : rs, ..}
  otherwise -> Nothing

runCmdIO :: CmdIO -> Process -> Maybe Process
runCmdIO CmdIOPrintChar p@(Process {..}) = case stack of
  (x : xs) -> Just Process {output = output ++ [chr x], stack = xs, ..}
  otherwise -> Nothing
runCmdIO CmdIOPrintNum p@(Process {..}) = case stack of
  (x : xs) -> Just Process {output = output ++ show x, stack = xs, ..}
  otherwise -> Nothing
runCmdIO CmdIOReadChar p@(Process {..}) = case stack of
  (b : rs) -> do
    (c, p') <- readChar p
    let a = ord c
    pure p' {heap = Map.insert b a heap, stack = rs}
  otherwise -> Nothing
runCmdIO CmdIOReadNum p@(Process {..}) = case stack of
  (b : rs) -> do
    (a, p') <- readInt p
    pure p' {heap = Map.insert b a heap, stack = rs}
  otherwise -> Nothing

runCmdFlow :: CmdFlow -> Process -> Maybe Process
runCmdFlow (CmdFlowMark l) p@(Process {..}) = incIp <$> Just Process {labels = Map.insert l ip labels, ..}
runCmdFlow (CmdFlowSub l) p@(Process {..}) = Map.lookup l labels >>= \x -> Just Process {callStack = (ip + 1) : callStack, ip = x, ..}
runCmdFlow (CmdFlowJump l) p@(Process {..}) = Map.lookup l labels >>= \x -> Just Process {ip = x, ..}
runCmdFlow (CmdFlowJumpIfZero l) p@(Process {..}) = case stack of
  (x : xs) | x == 0 -> Map.lookup l labels >>= \ip' -> Just Process {ip = ip', stack = xs, ..}
  (x : xs) | otherwise -> incIp <$> Just Process {stack = xs, ..}
  otherwise -> Nothing
runCmdFlow (CmdFlowJumpIfNeg l) p@(Process {..}) = case stack of
  (x : xs) | x < 0 -> Map.lookup l labels >>= \ip' -> Just Process {ip = ip', stack = xs, ..}
  (x : xs) | otherwise -> incIp <$> Just Process {stack = xs, ..}
  otherwise -> Nothing
runCmdFlow CmdFlowRet p@(Process {..}) = case callStack of
  (x : xs) -> Just Process {ip = x, callStack = xs, ..}
  otherwise -> Nothing
runCmdFlow CmdFlowExit p@(Process {..}) = Just Process {status = Exit, ..}

getResult :: Maybe Process -> Result
getResult Nothing = Left ""
getResult (Just p) = Right $ output p
