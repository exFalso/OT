{-# LANGUAGE TypeFamilies, GADTs, PolyKinds, DataKinds, TypeOperators, RankNTypes, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, InstanceSigs #-}
module Patchable where

import Test.QuickCheck (Gen)
import Data.Constraint

data EitherS a b s where
  LeftS :: a s -> EitherS a b (Left s)
  RightS :: b s -> EitherS a b (Right s)

data MuS f s where
  RollS :: f (MuS f) s -> MuS f s

data P doc a b where
  PNil :: P doc a a
  PCons :: Mod doc a b -> P doc b c -> P doc a c

data Some f where
  Some :: f a -> Some f

data MergeRes doc b c where
  MergeRes :: P doc c d -> P doc b d -> MergeRes doc b c

class Patchable doc where
    data Mod doc a b
    act :: Mod doc a b -> doc a -> doc b
    -- first arg has precedence
    merge :: Mod doc a b -> Mod doc a c -> MergeRes doc b c
    -- may turn out that the mod doesn't do anything
    inverse :: Mod doc a b -> P doc b a

instance (Patchable x, Patchable y) => Patchable (EitherS x y) where
  data Mod (EitherS x y) a b where
    LeftMod :: Mod x a b -> Mod (EitherS x y) (Left a) (Left b)
    RightMod :: Mod y a b -> Mod (EitherS x y) (Right a) (Right b)

  act (LeftMod m) (LeftS d) = LeftS (act m d)
  act (RightMod m) (RightS d) = RightS (act m d)

  merge (LeftMod m0) (LeftMod m1) = case merge m0 m1 of
    MergeRes p0 p1 -> MergeRes (lefty p0) (lefty p1)
  merge (RightMod m0) (RightMod m1) = case merge m0 m1 of
    MergeRes p0 p1 -> MergeRes (righty p0) (righty p1)

  inverse (LeftMod m) = lefty (inverse m)
  inverse (RightMod m) = righty (inverse m)

class PatchableF f where
  entailsP :: forall doc. Dict (Patchable doc) -> Dict (Patchable (f doc))

instance (PatchableF f) => Patchable (MuS f) where
  data Mod (MuS f) a b = MuMod (Mod (f (MuS f)) a b)

  act (MuMod m) (RollS f) = case entailsP Dict :: Dict (Patchable (f (MuS f))) of
    Dict -> RollS (act m f)

  merge (MuMod m0) (MuMod m1) = case entailsP Dict :: Dict (Patchable (f (MuS f))) of
    Dict -> case merge m0 m1 of
      MergeRes p0 p1 -> MergeRes (muy p0) (muy p1)

  inverse (MuMod m) = case entailsP Dict :: Dict (Patchable (f (MuS f))) of
    Dict -> muy (inverse m)

-- these are just simple maps but the types cannot be inferred
lefty :: P x a b -> P (EitherS x y) (Left a) (Left b)
lefty PNil = PNil
lefty (PCons m p) = PCons (LeftMod m) (lefty p)

righty :: P y a b -> P (EitherS x y) (Right a) (Right b)
righty PNil = PNil
righty (PCons m p) = PCons (RightMod m) (righty p)

muy :: P (f (MuS f)) a b -> P (MuS f) a b
muy PNil = PNil
muy (PCons m p) = PCons (MuMod m) (muy p)

(.*) :: P doc a b -> P doc b c -> P doc a c
PNil .* b = b
(PCons m a) .* b = PCons m (a .* b)

inv :: Patchable doc => P doc a b -> P doc b a
inv PNil = PNil
inv (PCons m a) = inv a .* inverse m

action :: Patchable doc => P doc a b -> doc a -> doc b
action PNil = id
action (PCons m a) = action a . act m

-- a >< b = (a x b, b x a) with 'a' having precedence
-- sry couldnt come up with "more descriptive" names
(><) :: Patchable doc => P doc a b -> P doc a c -> MergeRes doc b c
PNil >< b = MergeRes PNil b
b >< PNil = MergeRes b PNil
PCons a0 a1 >< PCons b0 b1 =
  case a0 `merge` b0 of
    MergeRes a0b0 b0a0 -> case a1 >< b0a0 of
      MergeRes a1b0a0 b0a0a1 -> case a0b0 >< b1 of
        MergeRes a0b0b1 b1a0b0 -> case a1b0a0 >< b1a0b0 of
          MergeRes a1b0a0b1a0b0 b1a0b0a1b0a0 -> MergeRes (a0b0b1 .* a1b0a0b1a0b0) (b0a0a1 .* b1a0b0a1b0a0)

-- _x :: Patchable doc => P doc -> P doc -> P d
-- _x a b = fst $ a >< b

-- x_ :: Patchable doc => P doc -> P doc -> P d
-- x_ a b = snd $ b >< a

