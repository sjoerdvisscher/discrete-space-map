{-# LANGUAGE FlexibleInstances #-}
module Data.DiscreteSpaceMap.Internal where

import Data.Functor.Bind
import Data.Foldable
import Data.Traversable
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable

-- | To be a key in the map, a position needs to be convertible to and from a stream of bits.
class Eq p => Pos p where
  uncons :: p -> (p, Bool)
  cons   :: (p, Bool) -> p
  -- ^ > cons . uncons == id 
  zero :: p
  -- ^ > uncons zero == (zero, False)


-- | 1D discrete space
instance Pos Integer where
  zero = 0
  uncons i = let (d, m) = i `divMod` 2 in (negate d, m == 1)
  cons (i, b) = (if b then 1 else 0) - 2 * i

-- | 2D discrete space
instance Pos p => Pos (p, p) where
  zero = (zero, zero)
  uncons (x, y) = let (x', b) = uncons x in ((y, x'), b)
  cons ((x, y), b) = (cons (y, b), x)

-- | 3D discrete space
instance Pos p => Pos (p, p, p) where
  zero = (zero, zero, zero)
  uncons (x, y, z) = let (x', b) = uncons x in ((y, z, x'), b)
  cons ((x, y, z), b) = (cons (z, b), x, y)

-- | @[Bool]@ is the free instance of `Pos`.
instance Pos [Bool] where
  zero = []
  uncons [] = ([], False)
  uncons (b:bs) = (bs, b)
  cons (bs, b) = b:bs


data MapD a = MapD a (MapD (a, a)) deriving Show

instance Functor MapD where
  fmap = fmapDefault
instance Foldable MapD where
  foldMap = foldMapDefault
instance Foldable1 MapD where
  foldMap1 = foldMap1Default
instance Traversable MapD where
  traverse f = unwrapApplicative . traverse1 (WrapApplicative . f)
instance Traversable1 MapD where
  traverse1 f (MapD a ca) = MapD <$> f a <.> traverse1 (\(a1, a2) -> (,) <$> f a1 <.> f a2) ca

gotoD :: Pos p => p -> p -> (a, MapD a) -> (a, MapD a)
gotoD sp tp | sp == tp = id
            | otherwise = down tb . gotoD sp' tp' . up sb
  where
    (tp', tb) = uncons tp
    (sp', sb) = uncons sp
    up   False (a, MapD b c) = ((a, b), c)
    up   True  (b, MapD a c) = ((a, b), c)
    down False ((a, b), c) = (a, MapD b c)
    down True  ((a, b), c) = (b, MapD a c)

tabulateD :: Pos p => (p -> a) -> MapD a
tabulateD f = MapD (f (cons (zero, True))) $ tabulateD (\p -> (f (cons (p, False)), f (cons (p, True)))) 

zipWithKeyD :: Pos p => (p -> a -> b -> c) -> p -> MapD a -> MapD b -> MapD c
zipWithKeyD f p (MapD a ca) (MapD b cb) = 
  MapD (f pOther a b) (zipWithKeyD f' pUp ca cb)
  where 
    (pUp, s) = uncons p
    pOther = cons (pUp, not s)
    f' p' (a1, a2) (b1, b2) = (f (cons (p', False)) a1 b1, f (cons (p', True)) a2 b2)

traverseWithKey1D :: (Pos p, Apply f) => (p -> a -> f b) -> p -> MapD a -> f (MapD b)
traverseWithKey1D f p (MapD a ca) = 
  MapD <$> f pOther a <.> traverseWithKey1D f' pUp ca
  where 
    (pUp, s) = uncons p
    pOther = cons (pUp, not s)
    f' p' (a1, a2) = (,) <$> f (cons (p', False)) a1 <.> f (cons (p', True)) a2