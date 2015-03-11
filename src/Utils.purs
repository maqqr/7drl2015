module Utils where

import Data.Char
import Data.Tuple
import Data.Monoid
import Data.Foldable
import Data.Traversable
import qualified Data.String.Unsafe as SU


(>>) :: forall a b m. (Monad m) => m a -> m b -> m b
(>>) f g = f >>= \_ -> g

(//) = Tuple

monoidSum :: forall a. (Monoid a) => [a] -> a
monoidSum = foldr (<>) mempty

clamp :: forall a. (Ord a) => a -> a -> a -> a
clamp x min max | x < min = min
clamp x min max | x > max = max
clamp x min max = x

sign :: Number -> Number
sign x | x < 0     = -1
sign x | x > 0     =  1
sign x | otherwise = 0

makeChar :: String -> Char
makeChar = SU.charAt 0

numpad :: Number -> Number
numpad key = 96 + key

replicate :: forall a. Number -> a -> [a]
replicate i v | i > 0 = v : replicate (i - 1) v
replicate _ v         = []

mapM_ :: forall a b f m. (Foldable f, Applicative m) => (a -> m b) -> f a -> m Unit
mapM_ = flip for_

enumerate :: forall a. [a] -> [Tuple Number a]
enumerate xs = enum' xs 0
    where
        enum' (x:xs) i = Tuple i x : enum' xs (i + 1)
        enum' [] _ = []
