{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module OT where

-- OT. Without rebase for now

import Tagable
import Patchable
import Doc
import Control.Monad
import Control.Applicative
import qualified Data.Map as M

import Prelude hiding (head)

-- Arc x a y = x --a-> y
data Arc d = Arc (Tag d) (P d) (Tag d)

data ClientError = ConsistencyError

type History d = M.Map (Tag d) (Maybe (Arc d))

{-
invariant properties:
- branchPoint + branch  -->  head
- headTag is tag of head
-}
data ClientState d = ClientState
    { branch :: [Arc d]      -- client's modifications, not acknowledged yet
    , history :: History d   -- the client built this from server messages
    , branchPoint :: Tag d   -- this is the point where local modifications started happening
    , headTag :: Tag d       -- tag of head
    , head :: d              -- this is the current state of the document with local modifications (end of branch)
    }

initClientState :: (Doc d) => ClientState d
initClientState = ClientState [] (M.singleton initTag Nothing) initTag initTag initDoc

type Client a = Either ClientError a

inconsistent :: Client a
inconsistent = Left ConsistencyError

mClient :: Maybe a -> Client a
mClient = maybe inconsistent Right

-- given a tag and a history, path gives an arc-sequence until the end of history
path :: Doc d => Tag d -> History d -> Client [Arc d]
path x h = do
  node <- mClient $ M.lookup x h
  case node of
    Nothing -> return []
    Just a@(Arc _ _ next) -> (a :) <$> path next h

-- returns the result of the merge (can be applied to head) and intermediate arcs
patch :: Doc d => Arc d -> History d -> Client (Arc d, [Arc d])
patch arc@(Arc x b y) h = do
  node <- mClient $ M.lookup x h
  case node of
    Nothing -> return (arc, [])
    Just (Arc x' a z) -> do
      when (x' /= x) inconsistent
      let (ab, ba) = a >< b
          new = beta z y
          (arcab, arcba) = (Arc y ab new, Arc z ba new)
      (result, rest) <- patch arcba h
      return (result, arc : arcab : rest)

arcAction :: Doc d => [Arc d] -> d -> d
arcAction = foldr (flip (.)) id . map (\(Arc _ p _) -> action p)

insertArc :: Doc d => Arc d -> History d -> Client (History d)
insertArc a@(Arc x _ _) h = do
  _ <- mClient $ M.lookup x h   -- just for consistency
  return $ M.insert x (Just a) h

-- applies a patch received from server
applyPatch :: Doc d => Arc d -> ClientState d -> Client (ClientState d)
applyPatch arc@(Arc _ _ y) state@ClientState{..} = do
  (result@(Arc _ p resTag), inter) <- patch arc history
  newHistory <- insertArcs (inter ++ [result]) history

  case branch of
    -- we don't have a branch, we progress with the others
    [] -> let newHead = action p head in
          return state { history = newHistory
                       , headTag = resTag
                       , head = newHead
                       , branchPoint = resTag }
     -- we have a branch
    (Arc _ _ z : as) ->
        -- we got an ACK
        if y == z
             -- our whole branch has been broadcast
        then if null as
             then do
               arcs <- path branchPoint newHistory
               let newHead = arcAction arcs head
               return state { history = newHistory
                            , headTag = resTag
                            , head = newHead
                            , branchPoint = resTag
                            , branch = [] }
             else return state { history = newHistory
                               , branch = as }
        else return state { history = newHistory }

  where
    insertArcs = foldr (>=>) return . map insertArc

-- user creates a patch
createPatch :: Doc d => P d -> ClientState d -> ClientState d
createPatch p state@ClientState{..} = do
  let newHead = action p head
      newHeadTag = alpha p headTag
  state { headTag = newHeadTag
        , head = newHead
        , branch = branch ++ [Arc headTag p newHeadTag] }

currentHead :: ClientState d -> d
currentHead ClientState{..} = head
