{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Data.DiscreteSpaceMap (Pos(..), Map(..), modify) where

import Control.Comonad
import Control.Comonad.Store.Class
import Data.Key
import Data.Functor.Bind
import Data.Functor.Rep
import Data.Foldable
import Data.Traversable
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable

import Data.DiscreteSpaceMap.Internal

-- | `Map` is a zipper on an infinite perfect binary tree.
--   It contains the position and value of the focus.
--   The other values are stored in the derivative of a perfect binary tree.
--
--   Functions that combine 2 maps like `zipWith`, `zipWithKey` and `<@>` preserve the focus position of the second argument.
data Map p a = Map !p !a (MapD a) deriving Show

-- | Modify the value of the focus.
modify :: (a -> a) -> Map p a -> Map p a
modify m (Map p a f) = Map p (m a) f

type instance Key (Map p) = p

-- | @ `zipWith` :: Pos p => (a -> b -> c) -> Map p a -> Map p b -> Map p c@
instance Pos p => Zip (Map p) where
  zipWith f = zipWithKey (const f)
-- | @ `zipWithKey` :: Pos p => (p -> a -> b -> c) -> Map p a -> Map p b -> Map p c@
instance Pos p => ZipWithKey (Map p) where
  zipWithKey f as bs = Map p (f p a b) $ zipWithKeyD f p ca cb
    where
      Map p b cb = bs
      Map _ a ca = seek p as

-- | @ (`<@>`) :: Pos p => Map p (a -> b) -> Map p a -> Map p b@
instance Pos p => ComonadApply (Map p) where
  (<@>) = zap

-- | @
-- `extract` :: Map p a -> a
-- `extend` :: (Map p a -> b) -> Map p a -> Map p b@
instance Comonad (Map p) where
  extract (Map _ a _) = a
  extend f z@(Map p _ c) = Map p (f z) $ fmap (\a -> f (Map p a c)) c

-- | @
-- `pos` :: Pos p => Map p a -> p
-- `peek` :: Pos p => p -> Map p a -> a
-- `seek` :: Pos p => p -> Map p a -> Map p a
-- `seeks` :: Pos p => (p -> p) -> Map p a -> Map p a@
instance Pos p => ComonadStore p (Map p) where
  pos (Map p _ _) = p
  peek tp (Map sp a c) = fst $ gotoD sp tp (a, c)
  seek tp (Map sp a c) = let (a', c') = gotoD sp tp (a, c) in Map tp a' c'
  seeks f w = seek (f (pos w)) w

-- | @ `index` :: Pos p => Map p a -> p -> a@
instance Pos p => Indexable (Map p) where
  index = flip peek
instance Pos p => Lookup (Map p) where 
  lookup = lookupDefault
instance Pos p => Adjustable (Map p) where
  adjust f p z = seek (pos z) . modify f . seek p $ z
-- | @ `tabulate` :: Pos p => (p -> a) -> Map p a@
instance Pos p => Representable (Map p) where
  tabulate f = Map zero (f zero) (tabulateD f)

-- | @ `fmap` :: (a -> b) -> Map p a -> Map p b@
instance Functor (Map p) where
  fmap = fmapDefault  
-- | @ `mapWithKey` :: Pos p => (p -> a -> b) -> Map p a -> Map p b@
instance Pos p => Keyed (Map p) where
  mapWithKey = mapWithKeyDefault
-- | @ `foldMap` :: Monoid m => (a -> m) -> Map p a -> m@
instance Foldable (Map p) where 
  foldMap = foldMapDefault
-- | @ `foldMapWithKey` :: (Pos p, Monoid m) => (p -> a -> m) -> Map p a -> m@
instance Pos p => FoldableWithKey (Map p) where
  foldMapWithKey = foldMapWithKeyDefault
instance Foldable1 (Map p) where 
  foldMap1 = foldMap1Default
instance Pos p => FoldableWithKey1 (Map p) where
  foldMapWithKey1 = foldMapWithKey1Default
-- | @ `traverse` :: Applicative f => (a -> f b) -> Map p a -> f (Map p b)@
instance Traversable (Map p) where
  traverse f = unwrapApplicative . traverse1 (WrapApplicative . f)
-- | @ `traverseWithKey` :: (Pos p, Applicative f) => (p -> a -> f b) -> Map p a -> f (Map p b)@
instance Pos p => TraversableWithKey (Map p) where
  traverseWithKey f = unwrapApplicative . traverseWithKey1 (\k a -> WrapApplicative (f k a))
instance Traversable1 (Map p) where
  traverse1 f (Map p a c) = Map p <$> f a <.> traverse1 f c
instance Pos p => TraversableWithKey1 (Map p) where
  traverseWithKey1 f (Map p a c) = Map p <$> f p a <.> traverseWithKey1D f p c