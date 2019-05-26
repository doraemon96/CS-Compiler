{- 
 - Estructuras de datos y algoritmos de Coloreo
 -
 -}

import Control.Monad.State.Strict as ST

type ColorGen = ST.State Int

data WorkSets = { precolored :: ()
                , initial :: ()
                , simplifyWorklist:: ()
                , freezeWorklist :: ()
                , spillWorklist :: ()
                , spilledNodes :: ()
                , coalescedNodes :: ()
                , coloredNodes :: ()
                , selectStack :: ()
                }

data MoveSets = { coalescedMoves :: ()
                , constrainedMoves :: ()
                , frozenMoves :: ()
                , worklistMoves :: ()
                , activeMoves :: ()
                }

-- Suponer que WorkSets y MoveSets estan todos dentro de algo llamado ColorState

type ColorMonad = ST.StateT _ ColorGen

coloring :: ()
coloring = do flowGraph <- ???
              (igraph, nodemap) <- interferenceGraph flowGraph
              makeWorkList
              until coloreoCondition coloreoLoop
              assignColors
              if MONADA.spilledNodes != empty then do rewriteProgram(_)
                                                      coloring()

coloreoCondition :: MONADA -> Bool
coloreoCondition = undefined

coloreoLoop :: MONADA
