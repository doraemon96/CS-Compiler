module TigerColoring where

import TigerMunch                 as Mn
import TigerLiveness              as Lv

import qualified Data.Set         as Set
import qualified Data.Stack       as Stack
import Control.Monad.State.Strict as ST

type ColorGen = ST.State Int

data ColorSets = { -- WorkSets
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

type ColorMonad a = ST.StateT ColorSets a

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



addWorkList :: NodeFG -> ColorMonad () --TO DO: change  K
addWorkList n = do color_set <- get
                   move_related <- moveRelated n
                   let n_degree <- maybe (error "dalequenosovo9") id (M.lookup (degree color_set) n)
                   when (Set.member n (precolored color_set) && (not move_related) && (n_degree < K)) (put (color_set { freezeWorklist = freezeWorklist',
                                                                                                                        simplifyWorklist = simplifyWorklist' }))
                   where freezeWorklist' = Set.difference (freezeWorklist color_set) (Set.singleton n)
                         simplifyWorklist' =  Set.union (simplifyWorklist color_set) (Set.singleton n)

ok :: NodeFG -> NodeFG -> ColorMonad Bool --TO DO: change "degree", K
ok t r = do color_set <- get
            let n_degree <- maybe (error "dalequenosovo7") id (M.lookup (degree color_set) n)
            return (n_degree t < K) || (Set.member t (precolored color_set)) || (Set.member (t,r) (color_set adjSet)) 



makeWorklist :: [NodeFG] -> ColorMonad ()--TO DO: change  K
makeWorklist x:xs = do color_set <- get
                       let spillWorklist' = Set.union (spillWorklist color_set) x
                           freezeWorklist' = Set.union (freezeWorklist color_set) x
                           simplifyWorklist' = Set.union (simplifyWorklist color_set) x
                           let n_degree <- maybe (error "dalequenosovo154") id (M.lookup (degree color_set) n)
                    if n_degree x >= K
                    then do put (color_set { spillWorklist = spillWorklist' })
                            makeWorklist xs
                    else if moveRelated x
                    then do put (color_set { freezeWorklist = freezeWorklist' })
                            makeWorklist xs
                    else 
                            do put (color_set { simplifyWorklist = simplifyWorklist' })
                               makeWorklist xs              
                                    
nodeMoves :: NodeFG -> ColorMonad (Set.Set Lv.NodeFG)
nodeMoves n = do color_set <- get
                 return (Set.intersection (moveList n) Set.union (activeMoves color_set) (worklistMoves color_set))

moveRelated :: NodeFG -> ColorMonad Bool
moveRelated n = do node_moves <- nodeMoves n
                   return Set.null node_moves
                                    
simpleAlloc :: a

rewriteProgram :: a

sethiUllman :: a
