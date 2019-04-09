module TigerLiveness where

import Data.List                               (nub)
import Data.Map.Strict                      as M
import Data.Maybe
import qualified Control.Monad.State.Strict as ST
import Control.Monad.Extra

import TigerTemp                            as Temp
import TigerMunch                           as Mn
import TigerGraph                           as G

{-
 -  FLOW GRAPH  -
 -}

type NodeFG = G.Node Mn.Instr
type GraphFG = G.DGraph NodeFG

data FlowGraph = FGRAPH {
                        -- Grafo dirigido donde c/nodo representa una instruccion
                        -- o bloque basico
                          control :: GraphFG
                        -- Tabla de temporarios definidos en cada nodo
                        , def     :: M.Map NodeFG [Temp.Temp]
                        -- Tabla de temporarios usados en cada nodo
                        , use     :: M.Map NodeFG [Temp.Temp]
                        -- Corrobora si la instruccion que se le pasa es un move
                        , ismove  :: M.Map NodeFG Bool
                        } deriving (Show)

emptyFG :: FlowGraph
emptyFG = FGRAPH{ control = G.empty
                , def     = M.empty
                , use     = M.empty
                , ismove  = M.empty }


-- | Toma una lista de instrucciones y devuelve un grafo de flujo
--   junto a una lista de nodos que corresponde exactamente a las instrucciones
instrs2graph :: [Mn.Instr] -> FlowGraph
instrs2graph inss = let ((def, use, isMove), graph) = ST.runState (instrs2graph' inss) G.empty
                    in  FGRAPH graph def use isMove

instrs2graph' :: [Mn.Instr] -> ST.State GraphFG (M.Map NodeFG [Temp.Temp], M.Map NodeFG [Temp.Temp], M.Map NodeFG Bool)
instrs2graph' inss = do mlab <- insertLabels inss
                        insertInstrs inss mlab

-- | Primera pasada, inserto solo los labels para poder referenciarlos
--   mas adelante (en los Jump)
insertLabels :: [Mn.Instr] -> ST.State GraphFG (M.Map Temp.Label (Node Mn.Instr))
insertLabels inss = insertLabels' inss M.empty 
                    where insertLabels' []         labelMap = return labelMap
                          insertLabels' (ins:inss) labelMap = case ins of
                                                                LABEL{} -> do node <- G.newNode ins
                                                                              insertLabels' inss (M.insert (llab ins) (node) labelMap)
                                                                _       -> insertLabels' inss labelMap
                            

insertInstrs :: [Mn.Instr] -> M.Map Temp.Label (Node Mn.Instr) -> ST.State GraphFG (M.Map NodeFG [Temp.Temp], M.Map NodeFG [Temp.Temp], M.Map NodeFG Bool)
insertInstrs inss labmap = let getNode ins = case ins of
                                                LABEL{} -> return $ fromJust $ M.lookup (llab ins) labmap -- si no encuentra, rompe
                                                _       -> G.newNode ins
                               insertInstrs' []         (def, use, isMove) lastNode = return (def, use, isMove)
                               insertInstrs' (ins:inss) (def, use, isMove) lastNode = do
                                    node <- getNode ins
                                    let def' = M.insert node (case ins of 
                                                                  OPER{} -> odst ins
                                                                  MOVE{} -> [mdst ins]
                                                                  _      -> []         ) def
                                        use' = M.insert node (case ins of 
                                                                  OPER{} -> osrc ins
                                                                  MOVE{} -> [msrc ins]
                                                                  _      -> []         ) use
                                        isMove' = M.insert node (case ins of 
                                                                     MOVE{} -> True
                                                                     _      -> False ) isMove
                                    -- Agregamos la arista unicamente si el ultimo nodo no era un jump
                                    whenJust lastNode (\ln -> case G.content ln of
                                                                op@OPER{} -> maybe (return ()) (const (G.newEdge ln node)) (ojmp op)
                                                                _         -> G.newEdge ln node)
                                    case ins of
                                        OPER{} -> whenJust (ojmp ins) (mapM_ (\jmp -> whenJust (M.lookup jmp labmap) (G.newEdge node)))
                                        _      -> return () 
                                    insertInstrs' inss (def', use', isMove') (Just node)
                           in insertInstrs' inss (M.empty, M.empty, M.empty) Nothing
                                     
                                    
{-
 -  LIVENESS  -
 -}

--class Liveness where
--    data IGraph = IGRAPH {
--                           graph :: ()
--                         , tnode :: ()
--                         , gtemp :: ()
--                         , moves :: ()
--                         } deriving (Show, Eq)
--
---- | Toma un grafo de flujo y devuelve: un grafo de interferencia y una tabla
----   que mapea cada nodo del grafo de flujo al conjunto de temporarios que estan
----   vivos-en-salida
--interferenceGraph :: FlowGraph -> (IGraph, M.Map Node [Temp.Temp])
--interferenceGraph = undefined

