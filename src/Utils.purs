module Utils where

import Data.Char
import Data.Tuple
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Traversable
import qualified Data.String.Unsafe as SU
import qualified Data.String as S


(>>) :: forall a b m. (Monad m) => m a -> m b -> m b
(>>) f g = f >>= \_ -> g

(//) = Tuple

monoidSum :: forall a. (Monoid a) => [a] -> a
monoidSum = foldr (<>) mempty

capitalize :: String -> String
capitalize s = case S.uncons s of
    Just s' -> S.toUpper (S.singleton s'.head) ++ s'.tail
    Nothing -> ""

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

makeCharCode :: String -> Number
makeCharCode = toCharCode <<< makeChar

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

rgb :: Number -> Number -> Number -> String
rgb r g b = toHex r ++ toHex g ++ toHex b

foreign import toHex
    """
    function toHex (n) {
        return n.toString(16);
    }
    """ :: Number -> String

fill :: String -> Number -> String
fill xs number | (S.length xs) == number = xs
fill xs number | (S.length xs) < number  = fill (xs ++ " ") number
fill xs _ = xs
