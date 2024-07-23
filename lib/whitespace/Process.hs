{-# LANGUAGE ParallelListComp #-}

module Whitespace.Process
  ( Process (..),
    Status (..),
    binOp,
    binOpE,
    call,
    discard,
    exit,
    get,
    incIp,
    instruction,
    jump,
    load,
    output,
    process,
    pop,
    push,
    ret,
    store,
    swap,
    readInt,
    readChar,
    running,
    write,
  )
where

import Common.Util
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Read (readMaybe)
import Whitespace.Program

-- | Data Types representing the process of executing a Whitespace program.
data Process = Process
  { program :: Program,
    input :: String,
    outputAcc :: [String],
    stack :: [Int],
    heap :: Map Int Int,
    labels :: Map Label Int,
    callStack :: [Int],
    ip :: Int,
    status :: Status
  }
  deriving (Eq, Show)

data Status = Running | Finished deriving (Eq, Show)

-- | A smart constructor for Process which initializes the process with the given input and program.
-- | It also populates the labels map with the labels in the program.
process :: String -> Program -> Either String Process
process input program =
  findLabels program >>= \labels ->
    pure $
      Process
        { program = program,
          input = input,
          outputAcc = [],
          stack = [],
          heap = Map.empty,
          labels = labels,
          callStack = [],
          ip = 0,
          status = Running
        }

-- | Helper functions for manipulating the process.

-- | Helper functions related to the process status.
running :: Process -> Bool
running = (== Running) . status

exit :: Process -> Process
exit p = p {status = Finished}

-- | Helper functions for manipulating the instruction pointer and the instruction it points to.
incIp :: Process -> Either String Process
incIp p
  | validIndex (program p) (ip p + 1) = pure $ p {ip = ip p + 1}
  | otherwise = Left "Instruction pointer out of bounds"

instruction :: Process -> Either String Instruction
instruction p = instructionAt (program p) (ip p)

-- | Helper functions for manipulating the process stack.
push :: Process -> Int -> Process
push p n = p {stack = n : stack p}

pop :: Process -> Either String (Int, Process)
pop p = case stack p of
  (x : xs) -> pure (x, p {stack = xs})
  _ -> Left "Stack underflow"

get :: Process -> Int -> Either String Int
get p i = stack p !? i |> maybeToEither ("Stack out of bounds: " <> show i)

swap :: Process -> Either String Process
swap p = do
  (a, p') <- pop p
  (b, p'') <- pop p'
  pure $ push (push p'' a) b

binOp :: Process -> (Int -> Int -> Int) -> Either String Process
binOp p f = binOpE p (pure ... f)

binOpE :: Process -> (Int -> Int -> Either String Int) -> Either String Process
binOpE p f = do
  (b, p') <- pop p
  (a, p'') <- pop p'
  c <- f a b
  pure $ push p'' c

-- | Discard n elements below the top of the stack.
-- | If n is negative, discard all elements except the top.
discard :: Process -> Int -> Either String Process
discard p 0 = pure p
discard p n = case stack p of
  (x : xs) -> pure $ p {stack = x : drop n' xs}
  _ -> Left "Stack underflow"
  where
    n' = if n >= 0 then n else length (stack p)

-- | Helper functions for manipulating the heap.
store :: Process -> Int -> Int -> Process
store p k v = p {heap = Map.insert k v (heap p)}

load :: Process -> Int -> Either String Process
load p k = do
  v <- Map.lookup k (heap p) |> maybeToEither ("Heap address not found: " <> show k)
  pure $ push p v

-- | Helper functions for reading input and writing output.
readInt :: Process -> Either String (Int, Process)
readInt p = case lines (input p) of
  [] -> Left "Empty input"
  (x : xs) -> do
    i <- readMaybe x |> maybeToEither ("Not a number: " <> x)
    pure (i, p {input = unlines xs})

readChar :: Process -> Either String (Char, Process)
readChar p = case input p of
  [] -> Left "Empty input"
  (x : xs) -> pure (x, p {input = xs})

write :: Process -> String -> Process
write p s = p {outputAcc = s : outputAcc p}

output :: Process -> String
output = concat . reverse . outputAcc

-- | Helper functions for flow control.
jump :: Process -> Label -> Either String Process
jump p l = do
  x <- Map.lookup l (labels p) |> maybeToEither ("Label not found: " <> show l)
  if validIndex (program p) x
    then Right p {ip = x}
    else Left "Jump out of bounds"

call :: Process -> Label -> Either String Process
call p l = do
  x <- Map.lookup l (labels p) |> maybeToEither ("Label not found: " <> show l)
  if validIndex (program p) x
    then
      if validIndex (program p) (ip p + 1)
        then Right p {callStack = ip p + 1 : callStack p, ip = x}
        else Left "Subroutine out of bounds when returning"
    else Left "Subroutine out of bounds"

ret :: Process -> Either String Process
ret p = case callStack p of
  (x : xs) ->
    if validIndex (program p) x
      then Right p {ip = x, callStack = xs}
      else Left "Return address out of bounds"
  _ -> Left "Call stack empty"
