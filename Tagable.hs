{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- data structure to identify documents
module Tagable where

import Patchable

class (Patchable d, Ord (Tag d)) => Tagable d where
    data Tag d
    -- tag of empty document
    initTag :: Tag d
    -- tag of patched document (could be named as patchTag)
    -- alpha id t = t
    alpha :: {- client_id? -> -} P d -> Tag d -> Tag d
    -- tag of merged document (could be named as mergeTag)
    -- first has precedence
    beta :: Tag d -> Tag d -> Tag d

