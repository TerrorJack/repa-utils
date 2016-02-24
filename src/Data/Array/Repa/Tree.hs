{-|
    A multi-way tree (rose tree) as a nested 'Array'.
-}

{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Data.Array.Repa.Tree where

import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa

-- | A non-empty tree, with a label and an 'Array' of children.
data Tree r sh a = Node {
    label :: a,
    children :: Forest r sh a
}

-- | A possibly-empty 'Array' of trees.
type Forest r sh a = Repa.Array r sh (Tree r sh a)

-- | Sequentially compute the tree.
computeS :: (Repa.Shape sh, Repa.Source r1 (Tree r1 sh a), Repa.Target r2 (Tree r2 sh a)) => Tree r1 sh a -> Tree r2 sh a
{-# INLINE computeS #-}
computeS Node {..} = Node label $ Repa.computeS $ Repa.map computeS children
