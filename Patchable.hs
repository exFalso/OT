{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Patchable where

import Test.QuickCheck (Gen)

-- [a, b, c] means first a, then b and c
type P d = [Mod d]

class (GenPatch d) => Patchable d where
    data Mod d
    act :: Mod d -> d -> d
    -- first arg has precedence
    -- let (a_xb,bx_a) = merge a b   in   ... 
    merge :: Mod d -> Mod d -> (P d, P d)
    -- may turn out that the mod doesn't do anything
    inverse :: Mod d -> P d
    -- optimise patches
    optimise :: P d -> P d

class GenPatch d where
    genPatch :: d -> Gen (P d)

-- groupoid composition
(.*) :: P d -> P d -> P d
(.*) = (++)

inv :: Patchable d => P d -> P d
inv p = concatMap inverse $ reverse p

action :: Patchable d => P d -> d -> d
action = foldr (flip (.)) id . map act

-- a >< b = (a x b, b x a) with 'a' having precedence
-- sry couldnt come up with "more descriptive" names
(><) :: Patchable d => P d -> P d -> (P d, P d)
[] >< b = ([], b)
b >< [] = (b, [])
(a0 : a1) >< (b0 : b1) = (a0a1b0b1, b0b1a0a1)
  where
    (a0b0, b0a0) = a0 `merge` b0
    (a1b0a0, b0a0a1) = a1 >< b0a0
    (a0b0b1, b1a0b0) = a0b0 >< b1
    (a1b0a0b1a0b0, b1a0b0a1b0a0) = a1b0a0 >< b1a0b0
    (a0a1b0b1, b0b1a0a1) = (a0b0b1 .* a1b0a0b1a0b0, b0a0a1 .* b1a0b0a1b0a0)

_x :: Patchable d => P d -> P d -> P d
_x a b = fst $ a >< b

x_ :: Patchable d => P d -> P d -> P d
x_ a b = snd $ b >< a

-- comm a b = (b', a') -> a .* b = b' .* a'
-- there are two definitions, both are commutations
comm0 :: Patchable d => P d -> P d -> (P d, P d)
comm0 a b = (b', a')
  where
    (_, b') = inv a >< b
    (a', _) = a >< b'

comm1 :: Patchable d => P d -> P d -> (P d, P d)
comm1 a b = (b', a')
  where
    (b', _) = b >< inv a
    (_, a') = b' >< a

-- helper for writing optimisers
optimiseWith :: (Mod d -> Mod d -> P d) -> (Mod d -> P d) -> P d -> P d
optimiseWith f g p = optimiseWith' (p >>= g)
  where
    optimiseWith' [] = []
    optimiseWith' (a : as) = case optimiseWith' as of
      [] -> [a]
      (b : bs) -> f a b ++ bs
