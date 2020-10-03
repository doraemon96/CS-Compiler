module TigerLiveness where

import Data.List                               (nub)
import Data.Map.Strict                      as M
import Data.Maybe
import Data.Set                             as Set
import qualified Control.Monad.State.Strict as ST
import Control.Monad.Extra

import TigerTemp                            as Temp
import TigerAsm                             as As
import TigerGraph                           as G

{-
 -  FLOW GRAPH  -
 -}

type NodeFG = G.Node As.Instr
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
instrs2graph :: [As.Instr] -> FlowGraph
instrs2graph inss = let ((def, use, isMove), graph) = ST.runState (instrs2graph' inss) G.empty
                    in  FGRAPH graph def use isMove

instrs2graph' :: [As.Instr] -> ST.State GraphFG (M.Map NodeFG [Temp.Temp], M.Map NodeFG [Temp.Temp], M.Map NodeFG Bool)
instrs2graph' inss = do mlab <- insertLabels inss
                        insertInstrs inss mlab

-- | Primera pasada, inserto solo los labels para poder referenciarlos
--   mas adelante (en los Jump)
insertLabels :: [As.Instr] -> ST.State GraphFG (M.Map Temp.Label (Node As.Instr))
insertLabels inss = insertLabels' inss M.empty 
                    where insertLabels' []         labelMap = return labelMap
                          insertLabels' (ins:inss) labelMap = case ins of
                                                                ILABEL{} -> do node <- G.newNode ins
                                                                               insertLabels' inss (M.insert (llab ins) (node) labelMap)
                                                                _       -> insertLabels' inss labelMap
                            

insertInstrs :: [As.Instr] -> M.Map Temp.Label (Node As.Instr) -> ST.State GraphFG (M.Map NodeFG [Temp.Temp], M.Map NodeFG [Temp.Temp], M.Map NodeFG Bool)
insertInstrs inss labmap = let getNode ins = case ins of
                                                ILABEL{} -> return $ fromJust $ M.lookup (llab ins) labmap -- si no encuentra, rompe
                                                _        -> G.newNode ins
                               insertInstrs' []         (def, use, isMove) lastNode = return (def, use, isMove)
                               insertInstrs' (ins:inss) (def, use, isMove) lastNode = do
                                    node <- getNode ins
                                    let def' = M.insert node (case ins of 
                                                                  IOPER{} -> odst ins
                                                                  IMOVE{} -> [mdst ins]
                                                                  _       -> []         ) def
                                        use' = M.insert node (case ins of 
                                                                  IOPER{} -> osrc ins
                                                                  IMOVE{} -> [msrc ins]
                                                                  _       -> []         ) use
                                        isMove' = M.insert node (case ins of 
                                                                     IMOVE{} -> True
                                                                     _       -> False ) isMove
                                    -- Agregamos la arista unicamente si el ultimo nodo no era un jump
                                    whenJust lastNode (\ln -> case G.content ln of
                                                                op@IOPER{} -> maybe (return ()) (const (G.newEdge ln node)) (ojmp op)
                                                                _          -> G.newEdge ln node)
                                    case ins of
                                        IOPER{} -> whenJust (ojmp ins) (mapM_ (\jmp -> whenJust (M.lookup jmp labmap) (G.newEdge node)))
                                        _       -> return () 
                                    insertInstrs' inss (def', use', isMove') (Just node)
                           in insertInstrs' inss (M.empty, M.empty, M.empty) Nothing
                                     
                                    
{-
 -  LIVENESS  -
 -}

-- LivenessMap lleva un par (liveIn, liveOut)
type LivenessMap =  M.Map NodeFG (Set.Set Temp.Temp, Set.Set Temp.Temp)

-- | Toma un grafo de flujo y devuelve: un grafo de interferencia y una tabla
--   que mapea cada nodo del grafo de flujo al conjunto de temporarios que estan
--   vivos-en-salida
interferenceGraph :: FlowGraph -> LivenessMap
interferenceGraph flow = 
    let fcontrol = control flow
        fdef     = def flow
        fuse     = use flow
--      _        = ismove flow

        computeLiveness :: NodeFG -> ST.State LivenessMap ()
        computeLiveness node =
            let ndef   = Set.fromList (fdef M.! node)
                nuse   = Set.fromList (fuse M.! node)
                nsuccs = (succs fcontrol) M.! node -- PRECAUCION (puede tirar error) 
                getLiveIn lout = Set.union nuse (Set.difference lout ndef)
                getLiveOut st  = Set.unions $ Prelude.map (\succ -> fst (fromMaybe (Set.empty, Set.empty) (M.lookup succ st))) nsuccs
                
                loop (liveIn, liveOut) = do 
                    st <- ST.get
                    let liveOut' = getLiveOut st
                        liveIn'  = getLiveIn liveOut'
                    ST.modify (M.insert node (liveIn', liveOut'))
                    if liveIn == liveIn' && liveOut == liveOut'
                    then return ()
                    else loop (liveIn', liveOut')
                
            in loop (Set.empty, Set.empty)

        -- mapM concatena los efectos? TODO
        buildInterference = mapM_ computeLiveness

    in ST.execState (buildInterference (gnodes fcontrol)) M.empty
