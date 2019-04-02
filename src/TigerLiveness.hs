module Liveness where

import Distribution.Compat.Graph as G

import TigerTemp                 as Temp
import TigerMunch                as Mn


{-
 -  FLOW GRAPH  -
 -}


-- | Construido con (N Instr Int [Int]) lleva internamente
--   un mapping entre ints unicos e instrucciones
typedef Node = G.Node Int Mn.Instr
typedef Graph = G.Graph Node

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


-- | Toma una lista de instrucciones y devuelve un grafo de flujo
--   junto a una lista de nodos que corresponde exactamente a las instrucciones
instrs2graph :: [Mn.Instr] -> (FlowGraph, [Node])
instrs2graph = undefined


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

