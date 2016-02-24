module Data.Array.Repa.Operators.Filtering where

import Data.Array.Repa as Repa

filterP :: (Source r a, Shape sh, Monad m) => (a -> b) -> Array r sh a -> m (Array D sh b)
-- todo: Add an appropriate INLINE pragma.
filterP = undefined
