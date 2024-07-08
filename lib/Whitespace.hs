{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Whitespace (whitespace) where

import Common.Util
import Whitespace.Interpreter
import Whitespace.Parser
import Whitespace.Process
import Whitespace.Program
import Whitespace.Tokenizer

whitespace :: String -> String -> Either String String
whitespace code input = tokenize code |> parse programP >>= process input >>= runProcess |>> output
