{-|
    A multi-way tree (rose tree) as a nested 'Repa.Array'.
-}

{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Data.Array.Repa.Tree where

import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa

-- | A non-empty tree, with a label and an 'Repa.Array' of children.
data Tree r sh a = Node {
    label :: a,
    children :: Forest r sh a
}

-- | A possibly-empty 'Repa.Array' of trees.
type Forest r sh a = Repa.Array r sh (Tree r sh a)

-- | Sequentially compute the tree.
computeS :: (Repa.Load r1 sh (Tree r1 sh a), Repa.Target r2 (Tree r2 sh a)) => Tree r1 sh a -> Tree r2 sh a
{-# INLINE computeS #-}
computeS Node {..} = Node label $ Repa.computeS $ Repa.map computeS children

-- | Compute a tree in parallel. Only applies one layer of parallelism when evaluating 'children' of the root node, so depending on the shape of the tree, this function may or may not improve performance.
computeP :: (Repa.Load r1 sh (Tree r1 sh a), Repa.Target r2 (Tree r2 sh a), Repa.Source r2 (Tree r2 sh a), Monad m) => Tree r1 sh a -> m (Tree r2 sh a)
{-# INLINE computeP #-}
computeP Node {..} = Node label <$> Repa.computeP (Repa.map computeS children)

-- | Map a function to all elements in a tree, yielding a delayed tree.
map :: (Repa.Shape sh, Repa.Source r (Tree r sh a)) => (a -> b) -> Tree r sh a -> Tree Repa.D sh b
{-# INLINE map #-}
map f Node {..} = Node (f label) $ Repa.map (Data.Array.Repa.Tree.map f) children
