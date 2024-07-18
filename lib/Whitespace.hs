{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Whitespace (whitespace, convertFromReadable, convertToReadable) where

import Common.Util
import Whitespace.Converter
import Whitespace.Interpreter
import Whitespace.Process
import Whitespace.Program

whitespace :: String -> String -> Either String String
whitespace code input = parseProgram code >>= process input >>= runProcess |>> output
