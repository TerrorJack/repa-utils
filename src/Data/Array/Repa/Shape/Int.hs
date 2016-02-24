module Data.Array.Repa.Shape.Int where

import Data.Array.Repa.Shape

instance Shape Int where
    {-# INLINE [1] rank #-}
    rank _ = 1

    {-# INLINE [1] zeroDim #-}
    zeroDim = 0

    {-# INLINE [1] unitDim #-}
    unitDim = 1

    {-# INLINE [1] intersectDim #-}
    intersectDim = min

    {-# INLINE [1] addDim #-}
    addDim = (+)

    {-# INLINE [1] size #-}
    size = id

    {-# INLINE [1] sizeIsValid #-}
    sizeIsValid _ = True

    {-# INLINE [1] toIndex #-}
    toIndex _ idx = idx

    {-# INLINE [1] fromIndex #-}
    fromIndex _ idx = idx

    {-# INLINE [1] inShapeRange #-}
    inShapeRange l r idx = l <= idx && idx < r

    {-# NOINLINE listOfShape #-}
    listOfShape idx = [idx]

    {-# NOINLINE shapeOfList #-}
    shapeOfList [idx] = idx
    shapeOfList _ = error "Data.Array.Repa.Shape.Int.shapeOfList: Not a singleton list."

    {-# INLINE deepSeq #-}
    deepSeq = seq
