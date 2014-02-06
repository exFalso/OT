{-# LANGUAGE FlexibleContexts #-}
module Doc where

import Patchable
import Tagable

import Test.QuickCheck

class (Arbitrary d, Tagable d, Eq d, Show d,
       Show (Mod d), Show (Tag d)) => Doc d where
    initDoc :: d
