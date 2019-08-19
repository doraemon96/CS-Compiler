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


type ColorGen = ST.State Int

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
                 }

-- WorkSets y MoveSets estan todos dentro de algo llamado ColorSets
-- Luego nuestra ColorMonad debe llevar estos sets junto al grafo original,
-- y también generar colores únicos para coloreo.
type ColorMonad = ST.StateT ColorSets ColorGen

-- coloring es la función general, que toma el codigo assembler y
-- busca un coloreo factible para los nodos de dicho código
coloring :: [Mn.Instr] -> ColorMonad
coloring inss = do let flowgraph = instrs2graph inss
                       livegraph   = interferenceGraph flowgraph
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


--Julio
nodeMoves :: NodeFG -> ColorMonad -> ColorMonad
nodeMoves n m = do color_set <- get m
                   return (Set.intersection (moveList n) Set.union (activeMoves color_set) (worklistMoves color_set))

simpleAlloc :: a

addWorkList :: NodeFG -> ColorMonad -> ColorMonad
addWorkList n m = do color_set <- get m
                     when (Set.member n (precolored color_set) && (not (moveRelated n)) && (degree n < K))
                     put (color_set { freezeWorklist = freezeWorklist',
                                      simplifyWorklist = simplifyWorklist' })
                     where freezeWorklist' = Set.difference (freezeWorklist color_set) (Set.singleton n)
                           simplifyWorklist' =  Set.union (simplifyWorklist color_set) (Set.singleton n)

ok :: NodeFG -> NodeFG -> Bool --ColorMonad Bool ???
ok t r = (degree t < K) || (Set.member t precolored) || (Set.member (t,r) adjSet) 



makeWorklist :: [NodeFG] -> ColorMonad -> ColorMonad
makeWorklist x:xs m = do color_set <- get m
                         let spillWorklist' = Set.union (spillWorklist m) x
                             freezeWorklist' = Set.union (freezeWorklist m) x
                             simplifyWorklist' = Set.union (simplifyWorklist m) x
                         if degree x >= K
                            then makeWorklist xs (put ( color_set { spillWorklist = spillWorklist' }))
                         else if moveRelated x
                            then makeWorklist xs (put (color_set { freezeWorklist = freezeWorklist' }))
                         else 
                            makeWorklist xs (put (color_set { simplifyWorklist = simplifyWorklist' }))


moveRelated :: NodeFG -> Bool
moveRelated n = Set.null (nodeMoves n)

rewriteProgram

sethiUllman
