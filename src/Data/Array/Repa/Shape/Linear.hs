{-|
    Orphan instance of 'V0' 'Int', 'V1' 'Int', 'V2' 'Int', 'V3' 'Int', 'V4' 'Int' for 'Shape' class. Use 'Shape's which support much richer features.
-}

{-# LANGUAGE FlexibleInstances #-}

module Data.Array.Repa.Shape.Linear () where

import Control.Applicative
import Data.Array.Repa.Shape
import Data.Foldable
import Linear

int2integer :: Int -> Integer
{-# INLINE int2integer #-}
int2integer = fromIntegral

instance Shape (V0 Int) where
    {-# INLINE [1] rank #-}
    rank _ = 0

    {-# INLINE [1] zeroDim #-}
    zeroDim = pure 0

    {-# INLINE [1] unitDim #-}
    unitDim = pure 1

    {-# INLINE [1] intersectDim #-}
    intersectDim _ _ = V0

    {-# INLINE [1] addDim #-}
    addDim _ _ = V0

    {-# INLINE [1] size #-}
    size _ = 1

    {-# INLINE [1] sizeIsValid #-}
    sizeIsValid _ = True

    {-# INLINE [1] toIndex #-}
    toIndex _ _ = 0

    {-# INLINE [1] fromIndex #-}
    fromIndex _ _ = V0

    {-# INLINE [1] inShapeRange #-}
    inShapeRange _ _ _ = True

    {-# NOINLINE listOfShape #-}
    listOfShape _ = []

    {-# NOINLINE shapeOfList #-}
    shapeOfList [] = V0
    shapeOfList _ = error "Data.Array.Repa.Shape.Linear.shapeOfList: Not an empty list."

    {-# INLINE deepSeq #-}
    deepSeq V0 x = x

instance Shape (V1 Int) where
    {-# INLINE [1] rank #-}
    rank _ = 1

    {-# INLINE [1] zeroDim #-}
    zeroDim = pure 0

    {-# INLINE [1] unitDim #-}
    unitDim = pure 1

    {-# INLINE [1] intersectDim #-}
    intersectDim = liftA2 min

    {-# INLINE [1] addDim #-}
    addDim = (+)

    {-# INLINE [1] size #-}
    size = foldl' (*) 1

    {-# INLINE [1] sizeIsValid #-}
    sizeIsValid v = foldl' (*) 1 (fmap int2integer v) < int2integer maxBound


instance Shape (V2 Int) where
    {-# INLINE [1] rank #-}
    rank _ = 2

    {-# INLINE [1] zeroDim #-}
    zeroDim = pure 0

    {-# INLINE [1] unitDim #-}
    unitDim = pure 1

    {-# INLINE [1] intersectDim #-}
    intersectDim = liftA2 min

    {-# INLINE [1] addDim #-}
    addDim = (+)

    {-# INLINE [1] size #-}
    size = foldl' (*) 1

    {-# INLINE [1] sizeIsValid #-}
    sizeIsValid v = foldl' (*) 1 (fmap int2integer v) < int2integer maxBound


instance Shape (V3 Int) where
    {-# INLINE [1] rank #-}
    rank _ = 3

    {-# INLINE [1] zeroDim #-}
    zeroDim = pure 0

    {-# INLINE [1] unitDim #-}
    unitDim = pure 1

    {-# INLINE [1] intersectDim #-}
    intersectDim = liftA2 min

    {-# INLINE [1] addDim #-}
    addDim = (+)

    {-# INLINE [1] size #-}
    size = foldl' (*) 1

    {-# INLINE [1] sizeIsValid #-}
    sizeIsValid v = foldl' (*) 1 (fmap int2integer v) < int2integer maxBound


instance Shape (V4 Int) where
    {-# INLINE [1] rank #-}
    rank _ = 4

    {-# INLINE [1] zeroDim #-}
    zeroDim = pure 0

    {-# INLINE [1] unitDim #-}
    unitDim = pure 1

    {-# INLINE [1] intersectDim #-}
    intersectDim = liftA2 min

    {-# INLINE [1] addDim #-}
    addDim = (+)

    {-# INLINE [1] size #-}
    size = foldl' (*) 1

    {-# INLINE [1] sizeIsValid #-}
    sizeIsValid v = foldl' (*) 1 (fmap int2integer v) < int2integer maxBound
