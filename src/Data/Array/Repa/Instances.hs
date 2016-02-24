{-|
    Orphan instances of 'Repa.Array's for some "Prelude" classes. Currently provide 'Functor', 'Foldable', 'Traversable' instances of 'Repa.Array's with 'Repa.V', 'Repa.D' representations.
-}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.Array.Repa.Instances () where

import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa
import qualified Data.Array.Repa.Repr.Delayed as Repa
import qualified Data.Array.Repa.Repr.Vector as Repa
import qualified Data.Vector as V

class IsVector r sh where
    toVector :: Repa.Array r sh e -> V.Vector e
    fromVector :: sh -> V.Vector e -> Repa.Array r sh e

instance Repa.Shape sh => IsVector Repa.V sh where
    toVector = Repa.toVector
    fromVector = Repa.fromVector

instance Repa.Shape sh => IsVector Repa.D sh where
    toVector = Repa.toVector . Repa.computeS
    fromVector sh v = Repa.delay $ Repa.fromVector sh v

instance Repa.Shape sh => Functor (Repa.Array Repa.V sh) where
    fmap f arr = fromVector (Repa.extent arr) $ V.map f $ toVector arr

instance Repa.Shape sh => Functor (Repa.Array Repa.D sh) where
    fmap f arr = fromVector (Repa.extent arr) $ V.map f $ toVector arr

instance Repa.Shape sh => Foldable (Repa.Array Repa.V sh) where
    foldr f z arr = foldr f z $ toVector arr

instance Repa.Shape sh => Foldable (Repa.Array Repa.D sh) where
    foldr f z arr = foldr f z $ toVector arr

instance Repa.Shape sh => Traversable (Repa.Array Repa.V sh) where
    traverse f arr = fromVector (Repa.extent arr) <$> traverse f (toVector arr)

instance Repa.Shape sh => Traversable (Repa.Array Repa.D sh) where
    traverse f arr = fromVector (Repa.extent arr) <$> traverse f (toVector arr)
