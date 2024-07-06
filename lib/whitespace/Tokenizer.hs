module Whitespace.Tokenizer (Token (..), tokenize) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

-- | A token in the Whitespace language. The Whitespace language consists of three tokens: Space, Tab, and LF.
-- | Any other character is ignored by the interpreter like a comment.
data Token = Space | Tab | LF deriving (Eq, Ord, Show)

-- | A map from characters to tokens.
tokenMap :: Map Char Token
tokenMap = Map.fromList [(' ', Space), ('\t', Tab), ('\n', LF)]

-- | Tokenize a string into a list of tokens.
-- | Any character that is not a space, tab, or LF is ignored.
--
-- >>> tokenize " \t\n"
-- [Space,Tab,LF]
--
-- >>> tokenize "abc"
-- []
--
-- >>> tokenize " \t\nabc"
-- [Space,Tab,LF]
tokenize :: String -> [Token]
tokenize = mapMaybe (`Map.lookup` tokenMap)
