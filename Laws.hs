{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Laws where

import Patchable
import Doc

import Control.Monad
import Test.QuickCheck hiding ((><))
import Debug.Trace

data Proxy k = Proxy

-- Patch equality
eq :: (Doc d) => P d -> P d -> d -> Bool
eq p0 p1 d = action p0 d == action p1 d

checkLaws :: forall d. Doc d => Proxy d -> IO ()
checkLaws _ = do
  let args = stdArgs { maxSuccess = 5000 }
      tests = [invInv, invLeft, invRight, merge1, merge2a, merge2b]
  forM_ tests $ \f -> do
    quickCheckWith args (f :: d -> Gen Prop)

failPrint :: (Show a, Testable prop) => a -> prop -> Property
failPrint = whenFail . print

-- INVERSE
-- (a^-1)^-1 = a
invInv d = do
  p <- genPatch d
  failPrint p $ eq p (inv (inv p)) d

-- (a^-1) * a = id
invLeft d = do
  p <- genPatch d
  failPrint p $ eq [] (inv p .* p) (action p d)

-- a * (a^-1) = id
invRight d = do
  p <- genPatch d
  failPrint p $ eq [] (p .* inv p) d

debug a = trace (show a) a

-- MERGE
-- a * (b x_ a) = b * (a _x b)
merge1 d = do
  a <- genPatch d
  b <- genPatch d
  failPrint (a, b) $ eq (a .* (b `x_` a)) (b .* (a `_x` b)) d

-- (a _x b) _x c = a _x (b * c)
merge2a d = do
  a <- genPatch d
  b <- genPatch d
  let d' = action b d
  c <- genPatch d'
  failPrint (a, b, c) $ eq ((a `_x` b) `_x` c) (a `_x` (b .* c)) (action c d')

-- (a x_ b) x_ c = a x_ (b * c)
merge2b d = do
  a <- genPatch d
  b <- genPatch d
  let d' = action b d
  c <- genPatch d'
  failPrint (a, b, c) $ eq ((a `x_` b) `x_` c) (a `x_` (b .* c)) (action c d')

