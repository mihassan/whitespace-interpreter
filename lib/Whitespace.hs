{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Whitespace (whitespace) where

import Whitespace.Interpreter
import Whitespace.Parser
import Whitespace.Program
import Whitespace.Tokenizer
import Common.Util

whitespace :: String -> String -> Result
whitespace code input = tokenize code |> parse programP |>> mkProcess input >>= markLabels >>= runProcess |> getResult
