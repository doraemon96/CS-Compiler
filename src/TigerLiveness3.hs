module TigerLiveness3 where

--graph deps
import Data.Graph
import Data.List  as L
import Data.Map   as M
import Data.Set   as Set
import Prelude    as P

import Data.Maybe
import Control.Monad.Extra
import qualified Control.Monad.State.Strict as ST

--graph debug (thx denu :D)
import Data.GraphViz.Attributes.Complete as Att
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical
import qualified Data.Text.Lazy          as Lazy

--tiger deps
import TigerAsm  as As
import TigerTemp as Tm

import Debug.Trace


-- Graph utils
gsucc :: Graph -> Vertex -> [Vertex]
gsucc g v = P.map snd $ P.filter (\(p, s) -> p == v) e
  where e = edges g

gpred :: Graph -> Vertex -> [Vertex]
gpred g v = P.map fst $ P.filter (\(p, s) -> s == v) e
  where e = edges g

adj :: Graph -> Vertex -> [Vertex]
adj g v = L.union (gpred g v) (gsucc g v)

newGraph :: Graph
newGraph = buildG (0, -1) []

newNode :: Graph -> Graph
newNode g = buildG (0, length (vertices g)) (edges g) 

mkEdge :: Graph -> Edge -> Graph
mkEdge g e = buildG (0, length (vertices g) - 1) (e : edges g)

rmEdge :: Graph -> Edge -> Graph
rmEdge g e = buildG (0, length (vertices g) - 1) (edges g L.\\ [e]) 



-- FlowGraph
type TableInstr = Map Vertex Instr
type TableTemp = Map Vertex [Temp]
type TableBool = Map Vertex Bool
type TableLabel = Map Tm.Label Vertex

data FlowGraph = FGraph {control :: Graph,
                         info :: TableInstr,
                         def :: TableTemp,
                         use :: TableTemp,
                         ismove :: TableBool,
                         labmap :: TableLabel}
  deriving Show

emptyFG :: FlowGraph
emptyFG = FGraph {
    control = newGraph
    , info = M.empty
    , def = M.empty
    , use = M.empty
    , ismove = M.empty
    , labmap = M.empty
}

instrs2graph :: [Instr] -> FlowGraph
instrs2graph inss = let
    (f,v) = instrs2graph' inss
    in fst $ i2gWithJumps inss (f,v) v 

instrs2graph' :: [Instr] -> (FlowGraph, [Vertex])
instrs2graph' [] = (emptyFG, [])
instrs2graph' (o@(IOPER a dst src j) : instrs) =
  (fres{control = if L.null vres then newNode $ control fres
                    else mkEdge (newNode $ control fres) (node, vres !! 0), 
        info = M.insert node o $ info fres, 
        def = M.insert node dst $ def fres, 
        use = M.insert node src $ use fres, 
        ismove = M.insert node False $ ismove fres}, node : vres)
  where (fres, vres) = instrs2graph' instrs
        node = L.length vres
instrs2graph' (m@(IMOVE a dst src) : instrs) = 
  (fres{control = if L.null vres then newNode $ control fres
                    else mkEdge (newNode $ control fres) (node, vres !! 0), 
        info = M.insert node m $ info fres,
        def = M.insert node [dst] $ def fres, 
        use = M.insert node [src] $ use fres, 
        ismove = M.insert node True $ ismove fres}, node : vres)
  where (fres, vres) = instrs2graph' instrs
        node = L.length vres
instrs2graph' (lb@(ILABEL a l) : instrs) =
  (fres{control = if L.null vres then newNode $ control fres
                    else mkEdge (newNode $ control fres) (node, vres !! 0),
        info = M.insert node lb $ info fres, 
        def = M.insert node [] $ def fres,
        labmap = if L.null vres then labmap fres
                   else M.insert l node $ labmap fres,
        use = M.insert node [] $ use fres,
        ismove = M.insert node False $ ismove fres}, node : vres)
  where (fres, vres) = instrs2graph' instrs
        node = L.length vres

i2gWithJumps :: [Instr] -> (FlowGraph, [Vertex]) -> [Vertex] -> (FlowGraph, [Vertex])
i2gWithJumps [] (f, vs) vlist = (f, vlist)
i2gWithJumps ((IOPER a dst src j) : instrs) (f, vs) vlist =
  i2gWithJumps instrs (f{control = addJEdges j (head vs) (labmap f) $ control f}, tail vs) vlist
i2gWithJumps (i : instrs) (f, vs) vlist = i2gWithJumps instrs (f, tail vs) vlist

addJEdges :: Maybe [Tm.Label] -> Vertex -> TableLabel -> Graph -> Graph
addJEdges Nothing _ _ g = g
addJEdges (Just []) _ _ g = g --error "Revisar hasta liveness 1 -- TigerMakeGraph" 
addJEdges (Just j) v tl g = 
  L.foldl (\g' j' -> case M.lookup j' tl of
                       Just jj -> let eds = edges g'
                                  in case elem (v, jj) eds of
                                       True  -> g' 
                                       False -> mkEdge g' (v, jj)
                       Nothing -> g') g j   

-- Print graph in .dot format. For debugging purposes
defaultVis :: FlowGraph -> Lazy.Text
defaultVis fg =
  let (vs, es) = (vertices $ control fg, edges $ control fg)  
      (vcount, dotvs) = 
        P.foldl (\(i, vlist) v -> (i + 1, 
                                   DotNode i [Att.Label $ 
                                              StrLabel $ 
                                              Lazy.pack $ maybe (error "Liveness.hs")
                                                                show
                                                                (M.lookup v $ info fg)] : vlist)) 
                              (0, []) vs
      (ecount, dotes) = P.foldl (\(i, elist) e -> (i + 1, DotEdge (fst e) (snd e) 
                                                          [Att.Label $ StrLabel $ Lazy.pack $ show e] : elist)) 
                              (0, []) es
  in printDotGraph $
       DotGraph True True (Just $ Str (Lazy.pack "FlowGraph")) (DotStmts [] [] dotvs dotes)



-- LivenessMap
--  lleva un par (liveIn, liveOut)
type LivenessMap =  M.Map Vertex (Set.Set Tm.Temp, Set.Set Tm.Temp)

-- | Toma un grafo de flujo y devuelve: un grafo de interferencia y una tabla
--   que mapea cada nodo del grafo de flujo al conjunto de temporarios que estan
--   vivos-en-salida
interferenceGraph :: FlowGraph -> LivenessMap
interferenceGraph flow = 
    let fcontrol = control flow
        fdef     = def flow
        fuse     = use flow
--      _        = ismove flow

        computeLiveness :: Vertex -> ST.State LivenessMap ()
        computeLiveness node =
            let ndef   = Set.fromList (fdef M.! node)
                nuse   = Set.fromList (fuse M.! node)
                nsuccs = gsucc fcontrol node
                getLiveIn lout = Set.union nuse (Set.difference lout ndef)
                getLiveOut st  = Set.unions $ P.map (\succ -> fst (fromMaybe (Set.empty, Set.empty) (M.lookup succ st))) nsuccs
                
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

    in ST.execState (buildInterference (vertices fcontrol)) M.empty


data Liveness = LIVE {
    lin :: M.Map Vertex (Set.Set Tm.Temp)
    , lout :: M.Map Vertex (Set.Set Tm.Temp)
    , lin' :: M.Map Vertex (Set.Set Tm.Temp)
    , lout' :: M.Map Vertex (Set.Set Tm.Temp)
}

interferenceGraph2 :: FlowGraph -> LivenessMap
interferenceGraph2 flowgraph =
    let fgr = control flowgraph
        fdef = def flowgraph
        fuse = use flowgraph
        initial :: [Vertex] -> Liveness
        initial [] = LIVE {lin = M.empty, lout = M.empty, lin' = M.empty, lout' = M.empty}
        initial (v:vs) = let
            live' = initial vs
            in live'{lin = M.insert v Set.empty (lin live')
                , lout = M.insert v Set.empty (lout live')
                , lin' = M.insert v Set.empty (lin' live')
                , lout' = M.insert v Set.empty (lout' live')
            }
        outloop :: [Vertex] -> ST.State Liveness ()
        outloop vs = do
            equals <- mapM inloop vs
            if and equals
            then return ()
            else outloop vs
        inloop :: Vertex -> ST.State Liveness Bool
        inloop v = do
            ST.modify (\st -> st{lin' = M.insert v ((lin st) M.! v) (lin' st), lout' = M.insert v ((lout st) M.! v) (lout' st)})
            st <- ST.get
            let thisthing = (((lout st) M.! v) Set.\\ (Set.fromList $ fdef M.! v))
            let innew = (Set.fromList $ fuse M.! v) `Set.union` thisthing
            ST.modify (\st -> st{lin = M.insert v innew (lin st)})
            st' <- ST.get
            let outnew = Set.unions $ P.map (\s -> (lin st') M.! s) (gsucc fgr v)
            ST.modify (\st -> st{lout = M.insert v outnew (lout st)})
            st'' <- ST.get
            return $ (((lin' st'') M.! v) == innew) && (((lout' st'') M.! v) == outnew)
        joinMaps :: [Vertex] -> M.Map Vertex a -> M.Map Vertex b -> M.Map Vertex (a,b)
        joinMaps [] mapa mapb = M.empty
        joinMaps (v:vs) mapa mapb = let
            elema = mapa M.! v
            elemb = mapb M.! v
            in M.insert v (elema, elemb) $ joinMaps vs mapa mapb
            
        interLiveness = ST.execState (outloop (vertices fgr)) (initial (vertices fgr))
    in traceShow (fdef,fuse) $ joinMaps (vertices fgr) (lin interLiveness) (lout interLiveness)