module TigerGraph where

import qualified Data.Map                   as M
import qualified Data.Set                   as Set
import qualified Control.Monad.State.Strict as ST

data Node a = Node Int a 
        deriving Show
content (Node _ x) = x

instance Eq (Node a) where
  (Node id1 _) == (Node id2 _) = id1 == id2

instance Ord (Node a) where
  (Node id1 _) <= (Node id2 _) = id1 <= id2

data DGraph a = DGraph { succs :: M.Map a [a]
                       , preds :: M.Map a [a]
                       , edges :: Set.Set (a, a)
                       , gnodes :: [a]
                       , nextId :: Int
                       } deriving (Show)

empty = DGraph M.empty M.empty Set.empty [] 0

newNode :: a -> ST.State (DGraph (Node a)) (Node a)
newNode content = do
  nId <- ST.gets nextId
  let node = Node nId content
  ST.modify $ (\g -> g {nextId = nId + 1})
  addNode node
  return node

addNode :: Ord a => a -> ST.State (DGraph a) ()
addNode node = do
  exists <- ST.gets $ maybe False (const True) . M.lookup node . succs
  ST.when (not exists) (ST.modify $ (\g -> g { succs  = M.insert node [] (succs g)
                                             , preds  = M.insert node [] (preds g)
                                             , gnodes = node:(gnodes g)}))

newEdge :: Ord a => a -> a -> ST.State (DGraph a) ()
newEdge from to = do
  exists <- ST.gets $ isEdge from to
  ST.when (not exists) (ST.modify $ (\g -> g { succs = M.adjust (to:) from (succs g)
                                             , preds = M.adjust (from:) to (preds g)
                                             , edges = Set.insert (from, to) (edges g)}))

successors :: Ord a => a -> DGraph a -> [a]
successors node dgraph =
  maybe [] id (M.lookup node $ succs dgraph)

predecessors :: Ord a => a -> DGraph a -> [a]
predecessors node dgraph =
  maybe [] id (M.lookup node $ preds dgraph)

nodes :: DGraph a -> [a]
nodes = gnodes

isEdge :: Ord a => a -> a -> DGraph a -> Bool
isEdge from to = Set.member (from, to) . edges

--TODO: printGraph :: (Show a) => DGraph a -> ()
