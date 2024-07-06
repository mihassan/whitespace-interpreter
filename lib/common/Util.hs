module Common.Util ((|>), (|>>), hasDuplicates) where

import Data.Set qualified as Set -- From the 'containers' library

infixl 1 |>, |>>

-- >>> 5 |> (+ 3)
-- 8
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- >>> Just 5 |>> (+ 3)
-- Just 8
(|>>) :: (Functor f) => f a -> (a -> b) -> f b
x |>> f = f <$> x

-- >>> hasDuplicates [1, 2, 3]
-- False
-- >>> hasDuplicates [1, 2, 3, 1]
-- True
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length xs /= length (Set.fromList xs)
