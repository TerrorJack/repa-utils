{-# LANGUAGE FlexibleContexts, GADTs #-}

module Data.Array.Repa.Tree where

import Data.Array.Repa

data Tree r sh a where
    Node :: (Source r (Tree r sh a), Shape sh) => a -> Array r sh (Tree r sh a) -> Tree r sh a
