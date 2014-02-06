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

checkLaws :: forall d. Doc d => Proxy d -> IO [Result]
checkLaws _ = do
  let args = stdArgs { maxSuccess = 500 }
      tests = [merge2a]
      -- tests = [invInv, invLeft, invRight, merge1, merge2a, merge2b]
  forM tests $ \f -> do
    quickCheckWithResult args $ do
      doc <- arbitrary
      f (doc :: d)

-- INVERSE
-- (a^-1)^-1 = a
-- invInv :: Doc d => d -> Gen Bool
invInv :: Doc d => d -> Gen Bool
invInv d = do
  p <- genPatch d
  return $ eq p (inv (inv p)) d

-- (a^-1) * a = id
invLeft :: Doc d => d -> Gen Bool
invLeft d = do
  p <- genPatch d
  return $ eq [] (inv p .* p) (action p d)

invRight :: Doc d => d -> Gen Bool
-- a * (a^-1) = id
invRight d = do
  p <- genPatch d
  return $ eq [] (p .* inv p) d

debug :: Show a => a -> a
debug a = trace (show a) a

-- MERGE
-- a * (b x_ a) = b * (a _x b)
merge1 :: Doc d => d -> Gen Bool
merge1 d = do
  a <- genPatch d
  b <- genPatch d
  return $ eq (a .* (b `x_` a)) (b .* (a `_x` b)) d

-- (a _x b) _x c = a _x (b * c)
merge2a :: Doc d => d -> Gen Bool
merge2a d = do
  a <- genPatch d
  b <- genPatch d
  let d' = action b d
  c <- genPatch d'
  trace (show (d, a, b, c)) $ return $ eq ((a `_x` b) `_x` c) (a `_x` (b .* c)) d

-- (a x_ b) x_ c = a x_ (b * c)
merge2b :: Doc d => d -> Gen Bool
merge2b d = do
  a <- genPatch d
  b <- genPatch d
  let d' = action b d
  c <- genPatch d'
  return $ eq ((a `x_` b) `x_` c) (a `x_` (b .* c)) d

