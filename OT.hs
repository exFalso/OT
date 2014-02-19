{-# LANGUAGE TypeFamilies, GADTs, PolyKinds, DataKinds, TypeOperators, FlexibleContexts, ScopedTypeVariables #-}
module OT where

-- OT. Without rebase for now

import Tagable
import Patchable
import Doc
import History
import Control.Monad
import Control.Applicative
import qualified Data.Map as M

import Prelude hiding (head)

data ClientError = ConsistencyError

data ClientState doc t head = ClientState
    { history :: History doc t
    , branch :: Path doc t head
    , headTag :: CTag doc t head
    }

data SomeClientState doc where
  SomeClientState :: ClientState doc t head -> SomeClientState doc

clientState :: ClientState doc t head -> SomeClientState doc
clientState = SomeClientState

asd :: History doc t -> CTag doc t head -> ClientState doc t head
asd i h = ClientState i (PathNil h) h

initClientState :: forall doc. (Doc doc) => SomeClientState doc
initClientState = case (initHistory (initTag :: Tag doc) :: InitHistory doc) of
  InitHistory initHist head -> clientState $ asd initHist head

type Client a = Either ClientError a

inconsistent :: Client a
inconsistent = Left ConsistencyError

mClient :: Maybe a -> Client a
mClient = maybe inconsistent Right

-- -- returns the result of the merge (can be applied to head) and intermediate arcs
-- patch :: Doc doc => Arc doc -> History doc -> Client (Arc doc, [Arc doc])
-- patch arc@(Arc x b y) h = do
--   node <- mClient $ M.lookup x h
--   case node of
--     Nothing -> return (arc, [])
--     Just (Arc x' a z) -> do
--       when (x' /= x) inconsistent
--       let (ab, ba) = a >< b
--           new = beta z y
--           (arcab, arcba) = (Arc y ab new, Arc z ba new)
--       (result, rest) <- patch arcba h
--       return (result, arc : arcab : rest)

-- arcAction :: Doc doc => [Arc doc] -> doc -> doc
-- arcAction = foldr (flip (.)) id . map (\(Arc _ p _) -> action p)

-- insertArc :: Doc doc => Arc doc -> History doc -> Client (History doc)
-- insertArc a@(Arc x _ _) h = do
--   _ <- mClient $ M.lookup x h   -- just for consistency
--   return $ M.insert x (Just a) h

-- -- applies a patch received from server
-- applyPatch :: Doc doc => Arc doc -> ClientState doc -> Client (ClientState doc)
-- applyPatch arc@(Arc _ _ y) state@ClientState{..} = do
--   (result@(Arc _ p resTag), inter) <- patch arc history
--   newHistory <- insertArcs (inter ++ [result]) history

--   case branch of
--     -- we don't have a branch, we progress with the others
--     [] -> let newHead = action p head in
--           return state { history = newHistory
--                        , headTag = resTag
--                        , head = newHead
--                        , branchPoint = resTag }
--      -- we have a branch
--     (Arc _ _ z : as) ->
--         -- we got an ACK
--         if y == z
--              -- our whole branch has been broadcast
--         then if null as
--              then do
--                arcs <- path branchPoint newHistory
--                let newHead = arcAction arcs head
--                return state { history = newHistory
--                             , headTag = resTag
--                             , head = newHead
--                             , branchPoint = resTag
--                             , branch = [] }
--              else return state { history = newHistory
--                                , branch = as }
--         else return state { history = newHistory }

--   where
--     insertArcs = foldr (>=>) return . map insertArc

-- -- user creates a patch
-- createPatch :: Doc doc => P doc -> ClientState doc -> ClientState doc
-- createPatch p state@ClientState{..} = do
--   let newHead = action p head
--       newHeadTag = alpha p headTag
--   state { headTag = newHeadTag
--         , head = newHead
--         , branch = branch ++ [Arc headTag p newHeadTag] }

-- currentHead :: ClientState doc -> doc
-- currentHead ClientState{..} = head
