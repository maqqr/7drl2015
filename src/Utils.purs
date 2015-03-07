module Utils where

import Data.Tuple


(>>) :: forall a b m. (Monad m) => m a -> m b -> m b
(>>) f g = f >>= \_ -> g

(//) = Tuple
