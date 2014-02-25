{-# LANGUAGE TypeFamilies, FlexibleContexts, RankNTypes, TypeOperators, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Patchable where

import Data.Constraint
import Test.QuickCheck (Gen)

-- [a, b, c] means first a, then b and c
type P d = [Mod d]

class Patchable d where
    data Mod d
    act :: Mod d -> d -> d
    -- first arg has precedence
    -- let (a_xb,bx_a) = merge a b   in   ... 
    merge :: Mod d -> Mod d -> (P d, P d)
    -- may turn out that the mod doesn't do anything
    inverse :: Mod d -> P d

data Mu f = Roll (f (Mu f))

class PatchableF f where
  entailsP :: forall a. Patchable a :- Patchable (f a)

instance PatchableF f => Patchable (Mu f) where
  data Mod (Mu f) = ModMu (Mod (f (Mu f)))

  merge (ModMu m0) (ModMu m1) = case entailsP :: Patchable (Mu f) :- Patchable (f (Mu f)) of
    Sub Dict -> (\(a, b) -> (map ModMu a, map ModMu b)) (merge m0 m1)

  inverse (ModMu f) = case entailsP :: Patchable (Mu f) :- Patchable (f (Mu f)) of
    Sub Dict -> map ModMu $ inverse f

  act (ModMu m) (Roll f) = case entailsP :: Patchable (Mu f) :- Patchable (f (Mu f)) of
    Sub Dict -> Roll $ act m f

class GenPatch d where
    genPatch :: d -> Gen (P d)

instance (Patchable a, Patchable b) => Patchable (Either a b) where
instance (Patchable a, Patchable b) => Patchable (a, b) where
instance Patchable () where

newtype ListF a b = ListF (Either () (a, b))

instance (Patchable b) => Patchable (ListF a b) where

instance PatchableF (ListF a) where
  entailsP = Sub Dict

type List a = Mu (ListF a)

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
