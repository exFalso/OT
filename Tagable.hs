{-# LANGUAGE TypeFamilies, FlexibleContexts, PolyKinds, DataKinds #-}
module Tagable where

import Patchable

class (Patchable doc, Ord (Tag doc)) => Tagable doc where
    data Tag doc
    -- tag of empty document
    initTag :: Tag doc
    -- -- alpha id t = t
    alpha :: P doc a b -> Tag doc -> Tag doc
    -- first has precedence
    beta :: Tag doc -> Tag doc -> Tag doc

