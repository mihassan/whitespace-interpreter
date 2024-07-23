module Whitespace.Interpreter where

import Common.Util
import Data.Char
import Whitespace.Process
import Whitespace.Program

runProcess :: Process -> Either String Process
runProcess p | running p = do
  c <- instruction p
  p' <- runInsruction c p
  runProcess p'
runProcess p = Right p

runInsruction :: Instruction -> Process -> Either String Process
runInsruction i p = case i of
  (PUSH n) -> push p n |> incIp
  (DUP n) -> get p n |>> push p >>= incIp
  (DISCARD n) -> discard p n >>= incIp
  DUP_TOP -> get p 0 |>> push p >>= incIp
  SWAP -> swap p >>= incIp
  DISCARD_TOP -> pop p |>> snd >>= incIp
  ADD -> binOp p (+) >>= incIp
  SUB -> binOp p (-) >>= incIp
  MUL -> binOp p (*) >>= incIp
  DIV -> binOpE p f >>= incIp
    where
      f _ 0 = Left "Division by zero"
      f a b = pure $ a `div` b
  MOD -> binOpE p f >>= incIp
    where
      f _ 0 = Left "Modulo by zero"
      f a b = pure $ a `mod` b
  STORE -> do
    (a, p') <- pop p
    (b, p'') <- pop p'
    store p'' b a |> incIp
  LOAD -> do
    (a, p') <- pop p
    load p' a >>= incIp
  WRITE_CHAR -> do
    (x, p') <- pop p
    write p' (chr x : "") |> incIp
  WRITE_NUM -> do
    (x, p') <- pop p
    write p' (show x) |> incIp
  READ_CHAR -> do
    (a, p') <- readChar p
    (b, p'') <- pop p'
    store p'' b (ord a) |> incIp
  READ_NUM -> do
    (a, p') <- readInt p
    (b, p'') <- pop p'
    store p'' b a |> incIp
  (MARK _) -> incIp p
  (CALL l) -> call p l
  (JUMP l) -> jump p l
  (JZ l) -> do
    (x, p') <- pop p
    if x == 0 then jump p' l else incIp p'
  (JNG l) -> do
    (x, p') <- pop p
    if x < 0 then jump p' l else incIp p'
  RET -> ret p
  EXIT -> pure $ exit p
