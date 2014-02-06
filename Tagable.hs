{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Tagable where

import Patchable

class (Patchable d, Ord (Tag d)) => Tagable d where
    data Tag d
    -- tag of empty document
    initTag :: Tag d
    -- alpha id t = t
    alpha :: P d -> Tag d -> Tag d
    -- first has precedence
    beta :: Tag d -> Tag d -> Tag d

