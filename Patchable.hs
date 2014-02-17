{-# LANGUAGE TypeFamilies, GADTs, PolyKinds, DataKinds, TypeOperators #-}
module Patchable where

import Test.QuickCheck (Gen)

data EitherS (a :: k0 -> *) (b :: k1 -> *) (s :: Either k0 k1) where
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

-- class GenPatch doc where
--     genPatch :: d -> Gen (P d)

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

