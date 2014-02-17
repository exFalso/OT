{-# LANGUAGE TypeFamilies, GADTs, PolyKinds, DataKinds, TypeOperators, FlexibleContexts, ScopedTypeVariables #-}
module History where

import Patchable
import Tagable
import Doc
import qualified Data.Map as M
import Control.Monad
import Data.Maybe
import Unsafe.Coerce

import Prelude hiding (lookup)

-- Checked Tag for maps of shape 't', pointing to a state of shape 's'
-- Can be safely used to lookup a Node with 'lookup'
newtype CTag doc t s = CTag (Tag doc)

data Arc doc t s where
  Arc :: P doc a b -> CTag doc t b -> Arc doc t a

data Node doc t s = Node (CTag doc t s) (Arc doc t s)

newtype History doc t = History (M.Map (Tag doc) (Maybe (Some (Node doc t))))

data Path doc t s where
  PathNil :: CTag doc t s -> Path doc t s
  PathCons :: CTag doc t a -> P doc a b -> Path doc t b -> Path doc t a

initHistory :: (InitShape 'Proxy ~ t) => Tag doc -> (History doc t, Some (CTag doc t))
initHistory t = (History (M.singleton t Nothing), Some (CTag t))

-- returns a Checked Tag given a raw one. The shape it points to is hidden.
checkTag :: (Ord (Tag doc)) => Tag doc -> History doc t -> Maybe (Some (CTag doc t))
checkTag t (History m) = M.lookup t m >> Just (Some (CTag t))

-- The CTag guarantees that lookup will return an element of the map (which is itself a Maybe) and that it is well typed
lookup :: (Ord (Tag doc)) => CTag doc t s -> History doc t -> Maybe (Node doc t s)
lookup (CTag t) (History m) = case fromJust $ M.lookup t m of
  Nothing -> Nothing
  Just (Some n) -> Just $ unsafeCoerce n      -- ez

path :: (Ord (Tag doc)) => CTag doc t s -> History doc t -> Path doc t s
path a h = case lookup a h of
  Nothing -> PathNil a
  Just (Node a' (Arc p b)) -> PathCons a' p $ path b h

-- hides resulting shape so that when unpacked it wont be equal to the original shape 't'. Type checker needs a bit of help
insert :: (Ord (Tag doc)) => Some (Path doc t) -> [Some (Node doc t)] ->
          History doc (t :: k) -> Some (History doc :: k -> *)
insert (Some p) ns h = Some $ foldr rawInsert h (fst (pathToNodes p) ++ ns)
  where
    pathToNodes :: Path doc t s -> ([Some (Node doc t)], CTag doc t s)
    pathToNodes (PathNil t) = ([], t)
    pathToNodes (PathCons t ptch rest) =
      let (res, t') = pathToNodes rest in (Some (Node t (Arc ptch t')) : res, t)
    -- not a safe function by itself as the shape 't' is the same, dont expose
    -- rawInsert :: Some (Node doc t) -> History doc t -> History doc t
    rawInsert (Some n@(Node (CTag t) _)) (History m) =
      History $ M.insert t (Just (Some n)) m

-- helpers
insertNodes :: (Tagable doc, Ord (Tag doc)) => [Some (Node doc t)] ->
               History doc (t :: k) -> Some (History doc :: k -> *)
insertNodes ns = insert (Some (PathNil (CTag initTag))) ns

insertPath :: (Tagable doc, Ord (Tag doc)) => Some (Path doc t) ->
               History doc (t :: k) -> Some (History doc :: k -> *)
insertPath p = insert p []
