module Common.Util ((|>), (|>>), guardE, hasDuplicates, maybeToEither) where

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

-- >>> maybeToEither "error" (Just 5)
-- Right 5
-- >>> maybeToEither "error" Nothing
-- Left "error"
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

-- >>> guardE True "error" 5
-- Right 5
-- >>> guardE False "error" 5
-- Left "error"
guardE :: Bool -> a -> b -> Either a b
guardE True _ b = Right b
guardE False a _ = Left a
