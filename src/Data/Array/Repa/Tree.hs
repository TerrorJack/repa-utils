{-|
    A multi-way tree (rose tree) as a nested 'Array'.
-}

module Data.Array.Repa.Tree where

import Data.Array.Repa as Repa

-- | A non-empty tree, with a label and an 'Array' of children.
data Tree r sh a = Node {
    label :: a,
    children :: Forest r sh a
}

-- | A possibly-empty 'Array' of trees.
type Forest r sh a = Array r sh (Tree r sh a)
