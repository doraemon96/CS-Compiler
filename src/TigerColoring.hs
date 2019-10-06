{- 
 - Estructuras de datos y algoritmos de Coloreo
 -
 -}
module TigerColoring where

--Tiger Imports
import TigerMunch                 as Mn
import TigerLiveness              as Lv

import qualified Data.Set         as Set
import qualified Data.Stack       as Stack
import Control.Monad.State.Strict as ST

import Data.Map.Strict            as M
import TigerGraph                 as G

--data WorkSets = { precolored :: ()
--                , initial :: ()
--                , simplifyWorklist:: ()
--                , freezeWorklist :: ()
--                , spillWorklist :: ()
--                , spilledNodes :: ()
--                , coalescedNodes :: ()
--                , coloredNodes :: ()
--                , selectStack :: ()
--                }
--
--data MoveSets = { coalescedMoves :: ()
--                , constrainedMoves :: ()
--                , frozenMoves :: ()
--                , worklistMoves :: ()
--                , activeMoves :: ()
--                }

data ColorSets = {
                 -- WorkSets
                   precolored :: Set.Set Lv.NodeFG  --nodos que ya poseen un color
                 , initial :: Set.Set Lv.NodeFG  --nodos no procesados
                 , simplifyWorklist:: Set.Set Lv.NodeFG  --nodos low-degree non-moves
                 , freezeWorklist :: Set.Set Lv.NodeFG  --nodos low-degree moves
                 , spillWorklist :: Set.Set Lv.NodeFG  --nodos high-degree
                 , spilledNodes :: Set.Set Lv.NodeFG  --nodos potential spill
                 , coalescedNodes :: Set.Set Lv.NodeFG  --nodos coalescidos
                 , coloredNodes :: Set.Set Lv.NodeFG  --nodos coloreados con exito
                 , selectStack :: Stack.Stack Lv.NodeFG --stack de temporarios removidos del grafo
                 -- MoveSets
                 , coalescedMoves :: () --moves coalescidos
                 , constrainedMoves :: ()  --moves cuyo source y target interfieren
                 , frozenMoves :: ()  --moves no considerados para coalescing
                 , worklistMoves :: ()  --moves listos para coalescing
                 , activeMoves :: ()  --moves no listos para coalescing
                 -- Aditional functions
                 , degree :: M.Map Lv.NodeFG Int
                 }

-- WorkSets y MoveSets estan todos dentro de algo llamado ColorSets
-- Luego nuestra ColorMonad debe llevar estos sets junto al grafo original,
-- y también generar colores únicos para coloreo.
type ColorMonad = ST.State ColorSets ()

-- coloring es la función general, que toma el codigo assembler y
-- busca un coloreo factible para los nodos de dicho código
coloring :: [Mn.Instr] -> ColorMonad
coloring inss = do let flowgraph = instrs2graph inss
                       livemap   = interferenceGraph flowgraph
                   -- build construye el grafo de interferencia y llena el
                   -- conjunto worklistMoves
                   build livegraph
                   -- makeWorkList llena todas los conjuntos worklist
                   -- (pasamos el flowgraph para que pueda ver los degree)
                   makeWorkList flowgraph
                   -- intentamos simplify, coalesce, freeze y selectspill
                   -- hasta que no tengamos mas nodos para trabajar
                   until (coloreoCondition ({-ColorSets-})) coloreoLoop
                   -- asignamos colores
                   -- (pasamos el flowgraph para que pueda ver los degree)
                   assignColors flowgraph
                   -- si la asignación hace spilll, alocamos memoria para los
                   -- temporarios spilled y reintentamos
                   if MONADA.spilledNodes != empty 
                   then do rewriteProgram ({-?-})
                           coloring inss
                   -- sino, hemos finalizado
                   return ({-?-})

-- coloreoCondition nos informa si ya no tenemos nada que hacer en el loop
-- principal de coloring
coloreoCondition :: ColorSets -> Bool
coloreoCondition = undefined

-- coloreoLoop hace una pasada de simplify, coalesce, freeze o selectspill
coloreoLoop :: ColorMonad
coloreoLoop = undefined

decrementDegree :: Lv.NodeFG -> ColorMonad ()
decrementDegree n = do cmonad <- get
                       let d = maybe (error "nodo sin grado. Te re cabio") id $ M.lookup n (degree cmonad)
                       put $ cmonad {degree = M.update (\i -> Just (d-1)) n (degree cmonad)}
                       when (d == k) $ do 
                           adjn <- adjacent n
                           enableMoves $ Set.union (Set.singleton n) $ adjn
                           put $ cmonad {spillWorklist = Set.delete n $ (spillWorklist cmonad)}
                           if moveRelated n
                           then put $ cmonad {freezeWorklist = Set.union (Set.singleton n) $ (freezeWorklist cmonad)}
                           else put $ cmonad {simplifyWorklist = Set.union (Set.singleton n) $ (simplifyWorklist cmonad)}

simplify :: ColorMonad ()
simplify = do cmonad <- get
              let (n, simplifyWorklist') = Set.deleteFindMin $ simplifyWorklist cmonad
              put $ cmonad {simplifyWorklist = simplifyWorklist'}
              Stack.stackPush selectStack n
              adjn <- adjacent n
              mapM_ decrementDegree adjn

enableMoves :: Set.Set Lv.NodeFG -> ColorMonad ()
enableMoves ns = mapM_ (\n -> do nmoves <- nodeMoves n
                                 mapM_ (\m -> do cmonad <- get
                                                 when (Set.member m (activeMoves cmonad)) $ do
                                                     put $ cmonad {activeMoves = Set.delete m (activeMoves cmonad)}
                                                     put $ cmonad {worklist = Set.union (Set.singleton m) (worklist cmonad)}) nmoves) ns

conservative :: Set.Set Lv.NodeFG -> ColorMonad Bool 
conservative ns = do cmonad <- get
                     let kf = Set.foldr (\n ki -> do let d = maybe (error "nodo sin grado. Te re cabio (BIS)") id $ M.lookup n (degree cmonad)
                                                     when (d >= k) (ki = ki + 1)) 0 ns
                     return $ kf < k

addEdge :: (Lv.NodeFG, Lv.NodeFG) -> ColorMonad ()
addEdge (u, v) = do cmonad <- get
                    when ((Set.notMember (u, v) adjSet) && (u /= v)) $ do
                        put $ cmonad {adjSet = Set.union (adjSet cmonad) (Set.union (Set.singleton (u, v)) (Set.singleton (v, u)))}
                        when (Set.notMember u precolored) $ do
                            put $ cmonad {adjList = M.update (\s -> Set.union s (singleton v)) u (adjList cmonad)}
                            put $ cmonad {degree = M.update (\d -> d + 1) u (degree cmonad)}
                        when (Set.notMember v precolored) $ do
                            put $ cmonad {adjList = M.update (\s -> Set.union s (singleton u)) v (adjList cmonad)}
                            put $ cmonad {degree = M.update (\d -> d + 1) v (degree cmonad)}
