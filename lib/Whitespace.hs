{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Whitespace (whitespace) where

import Common.Util
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 qualified as B
import Whitespace.Interpreter
import Whitespace.Process
import Whitespace.Program
import Whitespace.Tokenizer

whitespace :: String -> String -> Either String String
whitespace code input = tokenize code |> B.pack |> parseOnly programP >>= process input >>= runProcess |>> output
