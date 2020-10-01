{- 
 - Estructuras de datos y algoritmos de Coloreo
 -
 -}
module TigerColoring where

--Tiger Imports
import TigerTemp
import TigerMunch2                as Mn
import TigerLiveness              as Lv
import TigerAsm                   as As
import TigerFrame                 as Fr

import qualified Data.Map         as M
import qualified Data.Set         as Set
import qualified Data.Stack       as Stack
import Control.Monad.State.Strict as ST
import Control.Monad.Loops


availableColors :: Set.Set Temp
availableColors = Set.fromList [] --FIXME!!!!!!!!!!!!!!!!!!!!!!!!!

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

data ColorSets = ColorSetConstructor { 
    precolored :: Set.Set Temp  --nodos que ya poseen un color
    , initial :: Set.Set Temp  --nodos no procesados
    , simplifyWorklist:: Set.Set Temp  --nodos low-degree non-moves
    , freezeWorklist :: Set.Set Temp  --nodos low-degree moves
    , spillWorklist :: Set.Set Temp  --nodos high-degree
    , spilledNodes :: Set.Set Temp  --nodos potential spill
    , coalescedNodes :: Set.Set Temp  --nodos coalescidos
    , coloredNodes :: Set.Set Temp  --nodos coloreados con exito
    , selectStack :: Stack.Stack Temp --stack de temporarios removidos del grafo
    -- MoveSets
    , coalescedMoves :: Set.Set As.Instr --moves coalescidos
    , constrainedMoves :: Set.Set As.Instr  --moves cuyo source y target interfieren
    , frozenMoves :: Set.Set As.Instr  --moves no considerados para coalescing
    , worklistMoves :: Set.Set As.Instr  --moves listos para coalescing
    , activeMoves :: Set.Set As.Instr  --moves no listos para coalescing
    -- Aditional structures
    , degree :: M.Map Temp Int
    , adjSet :: Set.Set (Temp, Temp) --conjunto de aristas de interferencia
    , adjList :: M.Map Temp (Set.Set Temp) --adjList[u] := nodos que interfieren con u
    , moveList :: M.Map Temp (Set.Set As.Instr) --mapa de nodo a lista de moves con las que esta asociado
    , alias :: M.Map Temp Temp
    , k :: Int
    , color :: M.Map Temp Temp -- color asignado a un nodo
    -- Aditional states
--    , livenessMap :: LivenessMap
--    , flowGraph :: FlowGraph
    , live :: Set.Set Temp
    , okColors :: Set.Set Temp -- colors internal state
}

-- WorkSets y MoveSets estan todos dentro de algo llamado ColorSets
-- Luego nuestra ColorMonad debe llevar estos sets junto al grafo original,
-- y también generar colores únicos para coloreo.
type ColorMonad = ST.State ColorSets

-- -- coloring es la función general, que toma el codigo assembler y
-- -- busca un coloreo factible para los nodos de dicho código
-- coloring :: [As.Instr] -> ColorMonad ()
-- coloring inss = do 
--                    -- build construye el grafo de interferencia y llena el
--                    -- conjunto worklistMoves
--                    build
--                    -- makeWorkList llena todas los conjuntos worklist
--                    makeWorkList
--                    -- intentamos simplify, coalesce, freeze y selectspill
--                    -- hasta que no tengamos mas nodos para trabajar
--                    until (coloreoCondition ({-ColorSets-})) coloreoLoop
--                    -- asignamos colores
--                    assignColors
--                    -- si la asignación hace spilll, alocamos memoria para los
--                    -- temporarios spilled y reintentamos
--                    if MONADA.spilledNodes /= empty 
--                    then do rewriteProgram ({-?-})
--                            coloring ({-?-})
--                    -- sino, hemos finalizado
--                    return ()

-- coloreoCondition nos informa si ya no tenemos nada que hacer en el loop
-- principal de coloring
coloreoCondition :: ColorSets -> Bool
coloreoCondition = undefined

-- coloreoLoop hace una pasada de simplify, coalesce, freeze o selectspill
coloreoLoop :: ColorMonad ()
coloreoLoop = undefined


-- INVARIANT CHECKING --
degreeInvariant :: ColorMonad Bool
degreeInvariant = undefined

simplifyWorklistInvariant :: ColorMonad Bool
simplifyWorklistInvariant = undefined

freezeWorklistInvariant :: ColorMonad Bool
freezeWorklistInvariant = undefined

spillWorklistInvariant :: ColorMonad Bool
spillWorklistInvariant = undefined


-- FUNCTIONS DEFINITION --
-- initialize :: [As.Instr] -> ColorMonad ()
-- initialize = do
--     let precolored = Fr.registers
--         initial = [] --TODO
--         degree = []
--         adjSet = []
--         adjList = []
--         moveList = []
--         alias = []
--         k = 0
--         livenessmap = []
--         flowgraph = []
--     put { precolored = Set.fromList precolored  --nodos que ya poseen un color
--         , initial = Set.fromList initial  --nodos no procesados
--         , simplifyWorklist = Set.empty  --nodos low-degree non-moves
--         , freezeWorklist = Set.empty  --nodos low-degree moves
--         , spillWorklist = Set.empty  --nodos high-degree
--         , spilledNodes = Set.empty  --nodos potential spill
--         , coalescedNodes = Set.empty  --nodos coalescidos
--         , coloredNodes = Set.empty  --nodos coloreados con exito
--         , selectStack = Stack.stackNew --stack de temporarios removidos del grafo
--         -- MoveSets
--         , coalescedMoves = Set.empty --moves coalescidos
--         , constrainedMoves = Set.empty  --moves cuyo source y target interfieren
--         , frozenMoves = Set.empty  --moves no considerados para coalescing
--         , worklistMoves = Set.empty  --moves listos para coalescing
--         , activeMoves = Set.empty  --moves no listos para coalescing
--         -- Aditional structures
--         , degree :: M.Map Temp Int
--         , adjSet :: Set.Set (Temp, Temp) --conjunto de aristas de interferencia
--         , adjList :: M.Map Temp (Set.Set Temp) --adjList[u] := nodos que interfieren con u
--         , moveList :: M.Map Temp (Set.Set As.Instr) --mapa de nodo a lista de moves con las que esta asociado
--         , alias :: M.Map Temp Temp
--         , k :: Int
--         -- Aditional states
--         , livenessMap :: LivenessMap
--         , flowGraph :: FlowGraph
--         , live :: Set.Set Temp
--     }

-- build :: ColorMonad ()
-- build = do
    

-- buildBlock :: [Temp] -> ColorMonad ()
-- buildBlock inss = do
--     let live = 

-- buildInstr :: As.Instr -> ColorMonad 
-- buildInstr ins@(IMOVE{}) = do
--     st <- get
--     let uses = (use (flowGraph st)) ! ins
--     put (st{live = (live st) Set.\\ })

addEdge :: Temp -> Temp -> ColorMonad ()
addEdge u v = do
    st <- get
    when ((not (Set.member (u,v) (adjSet st))) && (u /= v)) $ do
        put (st{adjSet = (adjSet st) `Set.union` (Set.fromList [(u,v),(v,u)])})
        when (not (Set.member u (precolored st))) $ do
            st <- get
            let adjListU = (adjList st) M.! u
            let degreeU = (degree st) M.! u
            put (st{adjList = M.insert u (adjListU `Set.union` (Set.singleton v)) (adjList st),
                    degree = M.insert u (degreeU + 1) (degree st)})
        when (not (Set.member v (precolored st))) $ do
            st <- get
            let adjListV = (adjList st) M.! v
            let degreeV = (degree st) M.! v
            put (st{adjList = M.insert v (adjListV `Set.union` (Set.singleton u)) (adjList st),
                    degree = M.insert v (degreeV + 1) (degree st)})


-- MakeWorklist
makeWorkList :: ColorMonad ()
makeWorkList = do
    st <- get
    mapM_ (\n -> do
            st' <- get
            put (st'{initial = (initial st') Set.\\ (Set.singleton n)})
            moveRelatedN <- moveRelated n
            if ((degree st') M.! n) >= (k st') then
                do modify (\st -> st{spillWorklist = (spillWorklist st) `Set.union` (Set.singleton n)})
            else if moveRelatedN then
                do modify (\st -> st{freezeWorklist = (freezeWorklist st) `Set.union` (Set.singleton n)})
            else do modify (\st -> st{simplifyWorklist = (simplifyWorklist st) `Set.union` (Set.singleton n)})
         )
         (initial st)

adjacent :: Temp -> ColorMonad (Set.Set Temp)
adjacent n = do
    st <- get
    let adjListN = (adjList st) M.! n --TODO: check that it never fails
    return $ adjListN Set.\\ ((toSet (selectStack st)) `Set.union` (coalescedNodes st))

nodeMoves :: Temp -> ColorMonad (Set.Set As.Instr)
nodeMoves n = do
    st <- get
    let moveListN = (moveList st) M.! n --TODO: check that it never fails
    return $ moveListN `Set.intersection` ((activeMoves st) `Set.union` (worklistMoves st))

moveRelated :: Temp -> ColorMonad Bool
moveRelated n = do
    st <- get
    nodeMovesN <- nodeMoves n
    return $ Set.null nodeMovesN


-- Simplify
simplify :: ColorMonad ()
simplify = do
    st <- get
    let n = head $ Set.elems (simplifyWorklist st)
    put (st{simplifyWorklist = Set.delete n (simplifyWorklist st),
            selectStack = Stack.stackPush (selectStack st) n})
    adjacentN <- adjacent n
    mapM_ decrementDegree adjacentN --if this doesn't work do elems of the set

decrementDegree :: Temp -> ColorMonad ()
decrementDegree m = do
    st <- get
    let d = (degree st) M.! m
    put (st{degree = M.insert m (d-1) (degree st)})
    when (d == (k st)) $ do
        adjacentM <- adjacent m
        enableMoves $ (Set.singleton m) `Set.union` adjacentM
        modify (\st -> st{spillWorklist = (spillWorklist st) Set.\\ (Set.singleton m)})
        moveRelatedM <- moveRelated m
        if moveRelatedM then do
            modify (\st -> st{freezeWorklist = (freezeWorklist st) `Set.union` (Set.singleton m)})
        else do
            modify (\st -> st{simplifyWorklist = (simplifyWorklist st) `Set.union` (Set.singleton m)})

enableMoves :: Set.Set Temp ->  ColorMonad ()
enableMoves set =
    mapM_ (\n -> do
        nodeMovesN <- nodeMoves n
        mapM_ (\m -> do
            st <- get
            when (Set.member m (activeMoves st)) $ do
                modify (\st -> st{activeMoves = (activeMoves st) Set.\\ (Set.singleton m),
                                  worklistMoves = (worklistMoves st) `Set.union` (Set.singleton m)})
            )
            (nodeMovesN)
        )
        set


-- Coalesce
coalesce :: ColorMonad ()
coalesce = do
    st <- get
    let m = head $ Set.elems (worklistMoves st)
    put (st{worklistMoves = (worklistMoves st) Set.\\ (Set.singleton m)})
    x <- getAlias (getSrc m)
    y <- getAlias (getDst m)
    if (Set.member y (precolored st)) then do
        coalesce' m y x
    else do
        coalesce' m x y

getSrc :: As.Instr -> Temp
getSrc (As.IMOVE _ msrc _) = msrc
getSrc _ = error "Error al buscar source del move #4433."
getDst :: As.Instr -> Temp
getDst (As.IMOVE _ _ mdst) = mdst
getDst _ = error "Error al buscar dest del move #4444"

coalesce' :: As.Instr -> Temp -> Temp -> ColorMonad ()
coalesce' m u v = do
    st <- get
    adjacentV <- adjacent v
    adjacentU <- adjacent u
    cond1 <- mapM (\t -> ok t u) (Set.elems adjacentV)
    cond2 <- conservative $ adjacentU `Set.union` adjacentV
    if u == v then do
        put (st{coalescedMoves = (coalescedMoves st) `Set.union` (Set.singleton m)})
        addWorkList u
    else if (Set.member v (precolored st)) then do
        put (st{constrainedMoves = (constrainedMoves st) `Set.union` (Set.singleton m)})
        addWorkList u
        addWorkList v
    else if ((Set.member u (precolored st)) && (and cond1)) || ((not (Set.member u (precolored st))) && cond2) then do
        put (st{coalescedMoves = (coalescedMoves st) `Set.union` (Set.singleton m)})
        combine u v
        addWorkList u
    else do
        put (st{activeMoves = (activeMoves st) `Set.union` (Set.singleton m)})

addWorkList :: Temp -> ColorMonad ()
addWorkList u = do
    st <- get
    moveRelatedU <- moveRelated u
    let degreeU = (degree st) M.! u
    when ((not (Set.member u (precolored st))) && (not moveRelatedU) && (degreeU < (k st))) $ do
        put (st{freezeWorklist = (freezeWorklist st) Set.\\ (Set.singleton u),
                simplifyWorklist = (simplifyWorklist st) `Set.union` (Set.singleton u)})

ok :: Temp -> Temp -> ColorMonad Bool
ok t r = do
    st <- get
    let degreeT = (degree st) M.! t
    return $ (degreeT < (k st)) || (Set.member t (precolored st)) || (Set.member (t,r) (adjSet st))

conservative :: Set.Set Temp -> ColorMonad Bool
conservative nodes = do
    filtered <- filterM (\n -> do
                    st <- get
                    return $ ((degree st) M.! n) >= (k st)
                    )
                    (Set.elems nodes)
    st <- get
    return $ (length filtered) < (k st)

getAlias :: Temp -> ColorMonad Temp
getAlias n = do
    st <- get
    if (Set.member n (coalescedNodes st)) then do
        getAlias ((alias st) M.! n)
    else
        return n

combine :: Temp -> Temp -> ColorMonad ()
combine u v = do
    st <- get
    if Set.member v (freezeWorklist st) then do
        put (st{freezeWorklist = (freezeWorklist st) Set.\\ (Set.singleton v)})
    else do
        put (st{spillWorklist = (spillWorklist st) Set.\\ (Set.singleton v)})  
    st <- get
    let moveListU = (moveList st) M.! u
    let moveListV = (moveList st) M.! v
    put (st{alias = M.insert v u (alias st),
            moveList = M.insert u (moveListU `Set.union` moveListV) (moveList st)})
    st <- get
    adjacentV <- adjacent v
    mapM_ (\t -> do
            addEdge t u
            decrementDegree t
            )
            (adjacentV)
    st <- get
    let degreeU = (degree st) M.! u
    when ((degreeU >= (k st)) && (Set.member u (freezeWorklist st))) $ do
        put (st{freezeWorklist = (freezeWorklist st) Set.\\ (Set.singleton u),
                spillWorklist = (spillWorklist st) `Set.union` (Set.singleton u)})


-- Freeze
freeze :: ColorMonad ()
freeze = do
    st <- get
    let u = head $ Set.elems (freezeWorklist st)
    put (st{freezeWorklist = (freezeWorklist st) Set.\\ (Set.singleton u),
            simplifyWorklist = (simplifyWorklist st) `Set.union` (Set.singleton u)})
    freezeMoves u

freezeMoves :: Temp -> ColorMonad ()
freezeMoves u = do
    st <- get
    nodeMovesU <- nodeMoves u
    let m = head $ Set.elems nodeMovesU
    put (st{activeMoves = (activeMoves st) Set.\\ (Set.singleton m),
            frozenMoves = (frozenMoves st) `Set.union` (Set.singleton m)})
    let (x,y) = (getSrc m, getDst m)
    aliasX <- getAlias x
    aliasY <- getAlias y
    aliasU <- getAlias u
    if aliasY == aliasU then do
        freezeMoves' aliasX
    else
        freezeMoves' aliasY

freezeMoves' :: Temp -> ColorMonad ()
freezeMoves' v = do
    st <- get
    nodeMovesV <- nodeMoves v
    let degreeV = (degree st) M.! v
    when ((Set.null nodeMovesV) && (degreeV < (k st))) $ do
        put (st{freezeWorklist = (freezeWorklist st) Set.\\ (Set.singleton v),
                simplifyWorklist = (simplifyWorklist st) `Set.union` (Set.singleton v)})


-- SelectSpill

selectSpill :: ColorMonad ()
selectSpill = do
    m <- selectSpillHeuristic
    st <- get
    put (st{spillWorklist = (spillWorklist st) Set.\\ (Set.singleton m),
            simplifyWorklist = (simplifyWorklist st) `Set.union` (Set.singleton m)})
    freezeMoves m


selectSpillHeuristic :: ColorMonad Temp
selectSpillHeuristic = do
    st <- get
    return $ head $ Set.elems (spillWorklist st)


-- AssignColors
assignColors :: ColorMonad ()
assignColors = do
    whileM_ ((not . Stack.stackIsEmpty) <$> (gets selectStack))
            (do
                st <- get
                let (stack',n) = maybe (error "Impossible #3333") (id) $ Stack.stackPop (selectStack st)
                put (st{selectStack = stack',
                        okColors = availableColors})
                st' <- get
                let adjListN = (adjList st') M.! n
                mapM_ (\w -> do
                        aliasW <- getAlias w
                        st <- get
                        when (Set.member aliasW ((coloredNodes st) `Set.union` (precolored st)))
                            $ put (st{okColors = (okColors st) Set.\\ (Set.singleton aliasW)})
                    )
                    (Set.elems adjListN)
                st'' <- get
                if (Set.null (okColors st'')) then do
                    put (st''{spilledNodes = (spilledNodes st'') `Set.union` (Set.singleton n)})
                else do
                    let c = head $ Set.elems (okColors st'')
                    put (st''{coloredNodes = (coloredNodes st'') `Set.union` (Set.singleton n),
                              color = M.insert n c (color st'')})
            )
    st <- get
    mapM_ (\n -> do
            st <- get
            aliasN <- getAlias n
            put (st{color = M.insert n aliasN (color st)})
            )
            (coalescedNodes st)


-- RewriteProgram
rewriteProgram :: ColorMonad ()
rewriteProgram = undefined


-- Stack helper functions
toList :: Stack.Stack a -> [a]
toList stack =
    if Stack.stackIsEmpty stack then []
    else maybe (error "WTF") (\(stack',a) -> a : (toList stack')) (Stack.stackPop stack)

toSet :: (Ord a) => Stack.Stack a -> Set.Set a
toSet stack = Set.fromList (toList stack)