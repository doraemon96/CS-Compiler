{- 
 - Estructuras de datos y algoritmos de Coloreo
 -
 -}

--Tiger Imports
import TigerMunch                 as Mn

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

data ColorSets = { precolored :: ()
                 , initial :: ()
                 , simplifyWorklist:: ()
                 , freezeWorklist :: ()
                 , spillWorklist :: ()
                 , spilledNodes :: ()
                 , coalescedNodes :: ()
                 , coloredNodes :: ()
                 , selectStack :: ()
                 , coalescedMoves :: ()
                 , constrainedMoves :: ()
                 , frozenMoves :: ()
                 , worklistMoves :: ()
                 , activeMoves :: ()
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

