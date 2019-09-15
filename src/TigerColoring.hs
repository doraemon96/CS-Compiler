module TigerColoring where

import TigerMunch                 as Mn
import TigerLiveness              as Lv

import qualified Data.Map         as M
import qualified Data.Set         as Set
import qualified Data.Stack       as Stack
import Control.Monad.State


data ColorSets = ColorSetConstructor { precolored :: Set.Set Lv.NodeFG  --nodos que ya poseen un color
                 , initial :: Set.Set Lv.NodeFG  --nodos no procesados
                 , simplifyWorklist:: Set.Set Lv.NodeFG  --nodos low-degree non-moves
                 , freezeWorklist :: Set.Set Lv.NodeFG  --nodos low-degree moves
                 , spillWorklist :: Set.Set Lv.NodeFG  --nodos high-degree
                 , spilledNodes :: Set.Set Lv.NodeFG  --nodos potential spill
                 , coalescedNodes :: Set.Set Lv.NodeFG  --nodos coalescidos
                 , coloredNodes :: Set.Set Lv.NodeFG  --nodos coloreados con exito
                 , selectStack :: Stack.Stack Lv.NodeFG --stack de temporarios removidos del grafo
                 -- MoveSets
                 , coalescedMoves :: Set.Set (Lv.NodeFG, Lv.NodeFG) --moves coalescidos
                 , constrainedMoves :: Set.Set (Lv.NodeFG, Lv.NodeFG)  --moves cuyo source y target interfieren
                 , frozenMoves :: Set.Set (Lv.NodeFG, Lv.NodeFG)  --moves no considerados para coalescing
                 , worklistMoves :: Set.Set (Lv.NodeFG, Lv.NodeFG)  --moves listos para coalescing
                 , activeMoves :: Set.Set (Lv.NodeFG, Lv.NodeFG)  --moves no listos para coalescing
                 -- Aditional structures
                 , degree :: M.Map Lv.NodeFG Int
                 , adjSet :: Set.Set (Lv.NodeFG, Lv.NodeFG)
                 , adjList :: M.Map Lv.NodeFG (Set.Set Lv.NodeFG)
                 , moveList :: M.Map Lv.NodeFG (Set.Set (Lv.NodeFG, Lv.NodeFG)) 
                 , alias :: M.Map Lv.NodeFG Lv.NodeFG
                 , k :: Int
                 }

-- WorkSets y MoveSets estan todos dentro de algo llamado ColorSets
-- Luego nuestra ColorMonad debe llevar estos sets junto al grafo original,
-- y también generar colores únicos para coloreo.

type ColorMonad = State ColorSets

-- coloring es la función general, que toma el codigo assembler y
-- busca un coloreo factible para los nodos de dicho código
-- coloring :: [Mn.Instr] -> ColorMonad
-- coloring inss = do let flowgraph = instrs2graph inss
--                        livegraph   = interferenceGraph flowgraph
--                        -- build construye el grafo de interferencia y llena el
--                        -- conjunto worklistMovesk
--                        build livegraph
--                        -- makeWorkList llena todas los conjuntos worklist
--                        -- (pasamos el flowgraph para que pueda ver los degree)
--                        makeWorkList flowgraph
--                        -- intentamos simplify, coalesce, freeze y selectspill
--                        -- hasta que no tengamos mas nodos para trabajar
--                        until (coloreoCondition ({-ColorSets-})) coloreoLoop
--                        -- asignamos colores
--                        -- (pasamos el flowgraph para que pueda ver los degree)
--                        assignColors flowgraph
--                        -- si la asignación hace spilll, alocamos memoria para los
--                        -- temporarios spilled y reintentamos
--                        if MONADA.spilledNodes != empty 
--                        then do rewriteProgram ({-?-})
--                                coloring inss
--                        -- sino, hemos finalizado
--                        return ({-?-})

-- coloreoCondition nos informa si ya no tenemos nada que hacer en el loop
-- principal de coloring
coloreoCondition :: ColorSets -> Bool
coloreoCondition = undefined

-- coloreoLoop hace una pasada de simplify, coalesce, freeze o selectspill
coloreoLoop :: ColorMonad ()
coloreoLoop = undefined



addWorkList :: Lv.NodeFG -> ColorMonad ()
addWorkList n = do color_set <- get
                   move_related <- moveRelated n
                   let freezeWorklist' = Set.difference (freezeWorklist color_set) (Set.singleton n)
                       simplifyWorklist' =  Set.union (simplifyWorklist color_set) (Set.singleton n) 
                       n_degree = maybe (error "dalequenosovo9") id (M.lookup n (degree color_set))
                   when (Set.member n (precolored color_set) && (not move_related) && (n_degree < k color_set)) (put (color_set { freezeWorklist = freezeWorklist',
                                                                                                                        simplifyWorklist = simplifyWorklist' }))

ok :: Lv.NodeFG -> Lv.NodeFG -> ColorMonad Bool
ok t r = do color_set <- get
            let n_degree = maybe (error "dalequenosovo7") id (M.lookup t (degree color_set))
            return $ (n_degree < k color_set) || (Set.member t (precolored color_set)) || (Set.member (t,r) (adjSet color_set)) 



makeWorklist :: [Lv.NodeFG] -> ColorMonad ()
makeWorkList []     = return () 
makeWorklist (x:xs) = do color_set <- get
                         let spillWorklist' = Set.insert x (spillWorklist color_set)
                             freezeWorklist' = Set.insert x (freezeWorklist color_set)
                             simplifyWorklist' = Set.insert x (simplifyWorklist color_set)
                             x_degree = maybe (error "dalequenosovo154") id (M.lookup x (degree color_set))
                         is_move_related <- moveRelated x
                         if x_degree >= k color_set
                         then do put (color_set { spillWorklist = spillWorklist' })
                                 makeWorklist xs
                         else if is_move_related
                         then do put (color_set { freezeWorklist = freezeWorklist' })
                                 makeWorklist xs
                         else 
                                 do put (color_set { simplifyWorklist = simplifyWorklist' })
                                    makeWorklist xs              
                                    
nodeMoves :: Lv.NodeFG -> ColorMonad (Set.Set (Lv.NodeFG, Lv.NodeFG))
nodeMoves n = do color_set <- get
                 let move_list = moveList color_set
                     node_moves = maybe (error "dalequenosovo87") id (M.lookup n move_list)
                 return (Set.intersection node_moves (Set.union (activeMoves color_set) (worklistMoves color_set)))



moveRelated :: Lv.NodeFG -> ColorMonad Bool
moveRelated n = do node_moves <- nodeMoves n
                   return $ Set.null node_moves
                                    
simpleAlloc :: a
simpleAlloc = undefined
rewriteProgram :: a
rewriteProgram = undefined
sethiUllman :: a
sethiUllman = undefined