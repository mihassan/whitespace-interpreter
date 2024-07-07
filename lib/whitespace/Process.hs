{-# LANGUAGE ParallelListComp #-}

module Whitespace.Process
  ( Process (output),
    binOp,
    command,
    discard,
    exit,
    get,
    incIp,
    jump,
    load,
    process,
    pop,
    push,
    ret,
    store,
    sub,
    swap,
    readInt,
    readChar,
    running,
    write,
  )
where

import Common.Util
import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Read (readMaybe)
import Whitespace.Program

-- | Data Types representing the process of executing a Whitespace program.
data Process = Process
  { program :: Program,
    input :: String,
    output :: String,
    stack :: [Number],
    heap :: Map Number Number,
    labels :: Map Label Int,
    callStack :: [Int],
    ip :: Int,
    status :: Status
  }
  deriving (Eq, Show)

data Status = Running | Finished deriving (Eq, Show)

-- | A smart constructor for Process which initializes the process with the given input and program.
-- | It also populates the labels map with the labels in the program.
process :: String -> Program -> Maybe Process
process input program = do
  let ls = findLabels program
  guard $ uniqeLabels ls
  pure $
    Process
      { program = program,
        input = input,
        output = "",
        stack = [],
        heap = Map.empty,
        labels = Map.fromList ls,
        callStack = [],
        ip = 0,
        status = Running
      }
  where
    findLabels :: Program -> [(Label, Int)]
    findLabels p = [(l, i) | CmdFlow (CmdFlowMark l) <- p | i <- [0 ..]]
    uniqeLabels :: [(Label, Int)] -> Bool
    uniqeLabels ls = not . hasDuplicates $ fst <$> ls

-- | Helper functions for manipulating the process.

-- | Helper functions related to the process status.
running :: Process -> Bool
running = (== Running) . status

exit :: Process -> Process
exit p = p {status = Finished}

-- | Helper functions for manipulating the instruction pointer and the command it points to.
incIp :: Process -> Maybe Process
incIp p
  | ip p + 1 >= length (program p) = Nothing
  | otherwise = Just p {ip = ip p + 1}

command :: Process -> Maybe Command
command p = (program p) !? (ip p)

-- | Helper functions for manipulating the process stack.
push :: Process -> Number -> Process
push p n = p {stack = n : stack p}

pop :: Process -> Maybe (Number, Process)
pop p = case stack p of
  (x : xs) -> Just (x, p {stack = xs})
  _ -> Nothing

get :: Process -> Int -> Maybe Number
get p i = stack p !? i

swap :: Process -> Maybe Process
swap p = do
  (a, p') <- pop p
  (b, p'') <- pop p'
  pure $ push (push p'' a) b

binOp :: Process -> (Number -> Number -> Maybe Number) -> Maybe Process
binOp p f = do
  (a, p') <- pop p
  (b, p'') <- pop p'
  c <- f a b
  pure $ push p'' c

-- | Discard n elements below the top of the stack.
-- | If n is negative, discard all elements except the top.
discard :: Process -> Number -> Maybe Process
discard p n = case stack p of
  (x : xs) -> pure $ p {stack = x : drop n' xs}
  _ -> Nothing
  where
    n' = if n >= 0 then n else length (stack p)

-- | Helper functions for manipulating the heap.
store :: Process -> Number -> Number -> Process
store p k v = p {heap = Map.insert k v (heap p)}

load :: Process -> Number -> Maybe Process
load p k = do
  v <- Map.lookup k (heap p)
  pure $ push p v

-- | Helper functions for reading input and writing output.
readInt :: Process -> Maybe (Int, Process)
readInt p = case lines (input p) of
  [] -> Nothing
  (x : xs) -> do
    i <- readMaybe x
    pure (i, p {input = unlines xs})

readChar :: Process -> Maybe (Char, Process)
readChar p = case input p of
  [] -> Nothing
  (x : xs) -> pure (x, p {input = xs})

write :: Process -> String -> Process
write p s = p {output = output p ++ s}

-- | Helper functions for flow control.
jump :: Process -> Label -> Maybe Process
jump p l = do
  x <- Map.lookup l (labels p)
  Just p {ip = x}

sub :: Process -> Label -> Maybe Process
sub p l = do
  x <- Map.lookup l (labels p)
  Just p {callStack = ip p + 1 : callStack p, ip = x}

ret :: Process -> Maybe Process
ret p = case callStack p of
  (x : xs) -> Just p {ip = x, callStack = xs}
  _ -> Nothing
