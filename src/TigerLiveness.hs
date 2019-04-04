module Liveness where

import Distribution.Compat.Graph as G
import Data.List                    (nub)
import Data.Map.Strict           as M

import TigerTemp                 as Temp
import TigerMunch                as Mn


-- | Construido con (N Instr Int [Int]) lleva internamente
--   un mapping entre ints unicos e instrucciones
typedef Node = G.Node Int Mn.Instr
typedef Graph = G.Graph Node

{-
 -  GENERIC GRAPH  -
 -}

class tigGraph g where
    nodes :: g -> [Node]
    succ  :: Node -> g -> [Node]
    pred  :: Node -> g -> [Node]
    adj   :: Node -> g -> [Node]
    eq    :: Node -> Node -> Bool
    
    newGraph :: g
    newNode  :: Node -> g -> g
    mkEdge   :: (Int, Int) -> g -> Maybe g
    rmEdge   :: (Int, Int) -> g -> g

instance tigGraph Graph where
    nodes          = G.toList
    succ (N _ k _) = (flip G.neighbors) k
    pred (N _ k _) = (flip G.revNeighbors) k
    adj  n         = nub . (\g -> succ n g ++ pred n g)
    eq   n1 n2     = n1 == n2
    
    newGraph          = G.empty
    newNode           = G.insert -- #leponemoacaquetegenereliunik
    mkEdge (k1, k2) g = do n1 <- G.lookup k1 g
                           n2 <- G.lookup k2 g
                           let (N s k1 ks) = n1
                           return $ newNode (N s k1 (nub (k2 : ks))) g
    rmEdge (k1, k2) g = do n1 <- G.lookup k1 g
                           n2 <- G.lookup k2 g
                           let (N s k1 ks) = n1
                               ks'         = delete k2 ks
                           return $ newNode (N s k1 ks') g

{-
 -  FLOW GRAPH  -
 -}

-- | Esto deja todo igual al libro (hay que cambiar todo)
-- typedef Table a = M.Map Node a

data FlowGraph = FGRAPH {
                        -- Grafo dirigido donde c/nodo representa una instruccion
                        -- o bloque basico
                          control :: Graph
                        -- Tabla de temporarios definidos en cada nodo
                        , def     :: M.Map Node [Temp.Temp]
                        -- Tabla de temporarios usados en cada nodo
                        , use     :: M.Map Node [Temp.Temp]
                        -- Corrobora si la instruccion que se le pasa es un move
                        , ismove  :: M.Map Node Bool
                        } deriving (Show, Eq)

emptyFG :: FlowGraph
emptyFG = FGRAPH{ control = newGraph
                , def = M.empty
                , use = M.empty
                , ismove = M.empty }


-- | Toma una lista de instrucciones y devuelve un grafo de flujo
--   junto a una lista de nodos que corresponde exactamente a las instrucciones
instrs2graph :: [Mn.Instr] -> (FlowGraph, [Node])
instrs2graph xs = instr2graph' 1 (zip [1..] xs)

instrs2graph' :: Int -> [(Int,Mn.Instr)] -> (FlowGraph, [Node])
instrs2graph' _ []   = (emptyFG, [])
instrs2graph' n x:xs = let n = ()
                           (fg, ns) = instrs2graph' (n+1) xs
                       in (fg, ns)

getInfo :: Mn.Instr -> FlowGraph -> FlowGraph
getInfo instr fg = case instr of
                      OPER{}  -> let def' = odst instr 
                                     use' = osrc instr 
                                     jmp' = maybe [] id (ojmp instr)
                                     graph = (control fg)
                                 in fg{def = def', use = use'}
                      MOVE{}  -> 
                      LABEL{} ->

{-
 -  LIVENESS  -
 -}

class Liveness where
    data IGraph = IGRAPH {
                           graph :: ()
                         , tnode :: ()
                         , gtemp :: ()
                         , moves :: ()
                         } deriving (Show, Eq)

-- | Toma un grafo de flujo y devuelve: un grafo de interferencia y una tabla
--   que mapea cada nodo del grafo de flujo al conjunto de temporarios que estan
--   vivos-en-salida
interferenceGraph :: FlowGraph -> (IGraph, M.Map Node [Temp.Temp])
interferenceGraph = undefined

