{-# LANGUAGE FlexibleContexts, PolyKinds, DataKinds, TypeFamilies #-}
module Doc where

import Patchable
import Tagable

import Test.QuickCheck

data Proxy (k :: *) = Proxy

class Shape (a :: Proxy k) where
  type InitShape a :: k

class (Tagable d, Show (Tag d)) => Doc (d :: k -> *) where
    initDoc :: d (InitShape ('Proxy :: Proxy k))
