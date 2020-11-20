{- 
 - Estructuras de datos y algoritmos de Coloreo
 -
 -}
module TigerColoring where

--Tiger Imports
import TigerTemp
import TigerMunch2                as Mn
import TigerLiveness3             as Lv
import TigerGraph                 as Gr
import TigerAsm                   as As
import TigerFrame                 as Fr
import TigerAbs                   as Abs
import TigerUnique                as Uq

import qualified Data.Map         as M
import qualified Data.Set         as Set
import qualified Data.Stack       as Stack
import Control.Monad.State.Strict as ST
import Control.Monad.Loops

import Debug.Trace

availableColors :: Set.Set Temp
availableColors = Set.fromList (Fr.callersaves ++ Fr.calleesaves ++ Fr.argregs)


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
    --  input states
    , instructions :: [As.Instr]
    , frame :: Fr.Frame
    --  graph states
    , livenessMap :: Lv.LivenessMap
    , flowGraph :: Lv.FlowGraph
    --  build states
    , live :: Set.Set Temp
    , defs :: Set.Set Temp
    , uses :: Set.Set Temp
    --  color states
    , okColors :: Set.Set Temp -- colors internal
    --  rewrite states
    , newTempMap :: M.Map Temp Temp
    --  temp generation states
    , unique :: Uq.Unique
}

-- initColorSets
initColorSets :: Fr.Frame -> [As.Instr] -> Uq.Unique -> ColorSets
initColorSets fr inss unq = ColorSetConstructor { 
    precolored = Set.fromList Fr.registers -- Set.Set Temp  --nodos que ya poseen un color
    , initial = (foldl (\nodes ins -> nodes `Set.union` (getNodes ins)) (Set.empty) inss) Set.\\ (Set.fromList Fr.registers) -- Set.Set Temp  --nodos no procesados
    , simplifyWorklist = Set.empty -- Set.Set Temp  --nodos low-degree non-moves
    , freezeWorklist = Set.empty -- Set.Set Temp  --nodos low-degree moves
    , spillWorklist = Set.empty -- Set.Set Temp  --nodos high-degree
    , spilledNodes = Set.empty -- Set.Set Temp  --nodos potential spill
    , coalescedNodes = Set.empty -- Set.Set Temp  --nodos coalescidos
    , coloredNodes = Set.empty -- Set.Set Temp  --nodos coloreados con exito
    , selectStack = Stack.stackNew -- Stack.Stack Temp --stack de temporarios removidos del grafo
    -- MoveSets
    , coalescedMoves = Set.empty -- Set.Set As.Instr --moves coalescidos
    , constrainedMoves = Set.empty -- Set.Set As.Instr  --moves cuyo source y target interfieren
    , frozenMoves = Set.empty -- Set.Set As.Instr  --moves no considerados para coalescing
    , worklistMoves = Set.empty -- Set.Set As.Instr  --moves listos para coalescing
    , activeMoves = Set.empty -- Set.Set As.Instr  --moves no listos para coalescing
    -- Aditional structures
    , degree = M.empty -- M.Map Temp Int
    , adjSet = Set.empty -- Set.Set (Temp, Temp) --conjunto de aristas de interferencia
    , adjList = M.empty -- M.Map Temp (Set.Set Temp) --adjList[u] := nodos que interfieren con u
    , moveList = M.empty -- M.Map Temp (Set.Set As.Instr) --mapa de nodo a lista de moves con las que esta asociado
    , alias = M.empty -- M.Map Temp Temp
    , k = length availableColors -- Int
    , color = M.fromList $ zip Fr.registers Fr.registers -- M.Map Temp Temp -- color asignado a un nodo
    -- Aditional states
    --  input states
    , instructions = inss -- [As.Instr]
    , frame = fr -- Fr.Frame
    --  graph states
    , livenessMap = M.empty -- Lv.LivenessMap
    , flowGraph = Lv.emptyFG -- Lv.FlowGraph
    --  build states
    , live = Set.empty -- Set.Set Temp
    , defs = Set.empty -- Set.Set Temp
    , uses = Set.empty -- Set.Set Temp
    --  color states
    , okColors = Set.empty -- Set.Set Temp -- colors internal
    --  rewrite states
    , newTempMap = M.empty -- M.Map Temp Temp
    --  temp generation states
    , unique = unq -- Uq.Unique
}
    where getNodes (IOPER _ odst osrc _) = (Set.fromList odst) `Set.union` (Set.fromList osrc)
          getNodes (IMOVE _ mdst msrc)   = (Set.singleton mdst) `Set.union` (Set.singleton msrc)
          getNodes _                     = Set.empty

-- WorkSets y MoveSets estan todos dentro de algo llamado ColorSets
-- Luego nuestra ColorMonad debe llevar estos sets junto al grafo original,
-- y también generar colores únicos para coloreo.
type ColorMonad = ST.State ColorSets

-- runColoring corre todo
runColoring :: Fr.Frame -> [As.Instr] -> Uq.Unique -> (Fr.Frame, [As.Instr])
runColoring fr inss unq = 
    let (_, st) = ST.runState coloring (initColorSets fr inss unq)
    in ((frame st),(instructions st))

-- TLGenerator para colormonad por rewriteProgram
instance {-# OVERLAPS #-} TLGenerator ColorMonad where
    newTemp = detgenTemp <$> mkUnique
    newLabel = detgenLabel <$> mkUnique

-- UniqueGenerator para colormonad por TLGenerator
instance {-# OVERLAPS #-} UniqueGenerator ColorMonad where
    mkUnique = do
        modify (\st -> st{unique = (unique st)+1})
        gets unique
      

-- coloring es la función general, que toma el codigo assembler y
-- busca un coloreo factible para los nodos de dicho código
coloring :: ColorMonad ()
coloring = do
            inss <- gets instructions
            let flowGraph = instrs2graph inss
            let livenessMap = interferenceGraph2 flowGraph
            modify (\st -> st{flowGraph = flowGraph,
                                livenessMap = livenessMap})
            -- build construye el grafo de interferencia y llena el
            -- conjunto worklistMoves
            build
            -- makeWorkList llena todas los conjuntos worklist
            makeWorkList
            st <- get
            -- intentamos simplify, coalesce, freeze y selectspill
            -- hasta que no tengamos mas nodos para trabajar
            whileM_ coloreoCondition coloreoLoop
            -- asignamos colores
            assignColors
            -- si la asignación hace spilll, alocamos memoria para los
            -- temporarios spilled y reintentamos
            rewrite <- rewriteCondition
            if rewrite then do
                    rewriteProgram
                    rewriteReset
                    coloring
            else do
                st <- get
                applyColors
            -- sino, hemos finalizado


-- coloreoCondition nos informa si ya no tenemos nada que hacer en el loop
-- principal de coloring
coloreoCondition :: ColorMonad Bool
coloreoCondition = do
    st <- get
    let siw = Set.null (simplifyWorklist st)
    let wom = Set.null (worklistMoves st)
    let frw = Set.null (freezeWorklist st)
    let spw = Set.null (spillWorklist st)
    return $ not $ and [siw, wom, frw, spw]

-- coloreoLoop hace una pasada de simplify, coalesce, freeze o selectspill
coloreoLoop :: ColorMonad ()
coloreoLoop = do
    st <- get
    if (not $ Set.null $ simplifyWorklist st) then simplify
    else if (not $ Set.null $ worklistMoves st) then coalesce
    else if (not $ Set.null $ freezeWorklist st) then freeze
    else if (not $ Set.null $ spillWorklist st) then selectSpill
    else error "How did you manage to get here? You are a monster! #ffff"


-- rewriteCondition
rewriteCondition :: ColorMonad Bool
rewriteCondition = do
    st <- get
    return $ not $ Set.null (spilledNodes st)

rewriteReset :: ColorMonad ()
rewriteReset = do
    modify (\st -> st{
                    simplifyWorklist = Set.empty -- Set.Set Temp  --nodos low-degree non-moves
                    , freezeWorklist = Set.empty -- Set.Set Temp  --nodos low-degree moves
                    , spillWorklist = Set.empty -- Set.Set Temp  --nodos high-degree
                    , spilledNodes = Set.empty -- Set.Set Temp  --nodos potential spill
                    , coalescedNodes = Set.empty -- Set.Set Temp  --nodos coalescidos
                    , coloredNodes = Set.empty -- Set.Set Temp  --nodos coloreados con exito
                    , selectStack = Stack.stackNew -- Stack.Stack Temp --stack de temporarios removidos del grafo
                    , coalescedMoves = Set.empty -- Set.Set As.Instr --moves coalescidos
                    , constrainedMoves = Set.empty -- Set.Set As.Instr  --moves cuyo source y target interfieren
                    , frozenMoves = Set.empty -- Set.Set As.Instr  --moves no considerados para coalescing
                    , worklistMoves = Set.empty -- Set.Set As.Instr  --moves listos para coalescing
                    , activeMoves = Set.empty -- Set.Set As.Instr  --moves no listos para coalescing
                    , degree = M.empty -- M.Map Temp Int
                    , adjSet = Set.empty -- Set.Set (Temp, Temp) --conjunto de aristas de interferencia
                    , adjList = M.empty -- M.Map Temp (Set.Set Temp) --adjList[u] := nodos que interfieren con u
                    , moveList = M.empty -- M.Map Temp (Set.Set As.Instr) --mapa de nodo a lista de moves con las que esta asociado
                    , alias = M.empty -- M.Map Temp Temp
                    , color = M.fromList $ zip Fr.registers Fr.registers
                    , okColors = Set.empty -- Set.Set Temp -- colors internal
                    })


-- INVARIANT CHECKING --
degreeInvariant :: ColorMonad Bool
degreeInvariant = undefined

simplifyWorklistInvariant :: ColorMonad Bool
simplifyWorklistInvariant = undefined

freezeWorklistInvariant :: ColorMonad Bool
freezeWorklistInvariant = undefined

spillWorklistInvariant :: ColorMonad Bool
spillWorklistInvariant = undefined


-- Build
build :: ColorMonad ()
build = do
    lv <- gets livenessMap
    -- put all liveOut, defs and uses in an internal state
    mapM_ (\(node, (liveIn, liveOut)) -> do
            modify (\st -> st{live = liveOut})
            fg <- gets flowGraph
            let defN = maybe (error "!.555") (id) $ (Lv.def fg) M.!? node
            let useN = maybe (error "!.556") (id) $ (Lv.use fg) M.!? node
            modify (\st -> st{defs = Set.fromList defN,
                              uses = Set.fromList useN})
            buildInstr ((Lv.info fg) M.! node)
        )
        (M.toList lv)
    -- treat precolored as of infinite degree
    prec <- gets precolored
    mapM_ (\pre -> do --TODO: adjList adjSet?
            st <- get
            put (st{degree = M.insert pre maxBound (degree st)})
        ) 
        (Set.elems prec)

buildInstr :: As.Instr -> ColorMonad ()
buildInstr ins = do
    case ins of 
        (IMOVE{}) -> do
            modify (\st -> st{live = (live st) Set.\\ (uses st)})
            st <- get
            mapM_ (\n -> do
                    st <- get
                    let moveListN = maybe (Set.empty) (id) $ (moveList st) M.!? n
                    modify (\st -> st{moveList = M.insert n (moveListN `Set.union` (Set.singleton ins)) (moveList st)})
                ) 
                (Set.elems $ (defs st) `Set.union` (uses st))
            modify (\st -> st{worklistMoves = (worklistMoves st) `Set.union` (Set.singleton ins)})
        _   -> return ()
    modify (\st -> st{live = (live st) `Set.union` (defs st)})
    st <- get
    mapM_ (\d -> do
            st <- get
            mapM_ (\l -> do
                    addEdge l d
                )
                (Set.elems $ live st)
        )
        (Set.elems $ defs st)

addEdge :: Temp -> Temp -> ColorMonad ()
addEdge u v = do
    st <- get
    when ((not (Set.member (u,v) (adjSet st))) && (u /= v)) $ do
        put (st{adjSet = (adjSet st) `Set.union` (Set.fromList [(u,v),(v,u)])})
        (when (not (Set.member u (precolored st))) $ do
            st <- get
            let adjListU = maybe (Set.empty) (id) $ (adjList st) M.!? u
            let degreeU = maybe (0) (id) $ (degree st) M.!? u
            put (st{adjList = M.insert u (adjListU `Set.union` (Set.singleton v)) (adjList st),
                    degree = M.insert u (degreeU + 1) (degree st)}))
        (when (not (Set.member v (precolored st))) $ do
            st <- get
            let adjListV = maybe (Set.empty) (id) $ (adjList st) M.!? v
            let degreeV = maybe (0) (id) $ (degree st) M.!? v
            put (st{adjList = M.insert v (adjListV `Set.union` (Set.singleton u)) (adjList st),
                    degree = M.insert v (degreeV + 1) (degree st)}))


-- MakeWorklist
makeWorkList :: ColorMonad ()
makeWorkList = do
    st <- get
    mapM_ (\n -> do
            st' <- get
            put (st'{initial = (initial st') Set.\\ (Set.singleton n)})
            moveRelatedN <- moveRelated n
            let degreeN = maybe (error $ "No degree?! #401"++(show n)) (id) $ (degree st') M.!? n
            if (degreeN >= (k st')) then
                do modify (\st -> st{spillWorklist = (spillWorklist st) `Set.union` (Set.singleton n)})
            else if moveRelatedN then
                do modify (\st -> st{freezeWorklist = (freezeWorklist st) `Set.union` (Set.singleton n)})
            else do modify (\st -> st{simplifyWorklist = (simplifyWorklist st) `Set.union` (Set.singleton n)})
         )
         (initial st)

adjacent :: Temp -> ColorMonad (Set.Set Temp)
adjacent n = do
    st <- get
    let adjListN = maybe (Set.empty) (id) $ (adjList st) M.!? n  --NOTE: doy empty por default, capaz no esté bien
    return $ adjListN Set.\\ ((toSet (selectStack st)) `Set.union` (coalescedNodes st))

nodeMoves :: Temp -> ColorMonad (Set.Set As.Instr)
nodeMoves n = do
    st <- get
    let moveListN = maybe (Set.empty) (id) $ (moveList st) M.!? n
    return $ moveListN `Set.intersection` ((activeMoves st) `Set.union` (worklistMoves st))

moveRelated :: Temp -> ColorMonad Bool
moveRelated n = do
    st <- get
    nodeMovesN <- nodeMoves n
    return $ not $ Set.null nodeMovesN


-- Simplify
simplify :: ColorMonad ()
simplify = do
    st <- get
    let n = head $ Set.elems (simplifyWorklist st)
    put (st{simplifyWorklist = Set.delete n (simplifyWorklist st),
            selectStack = Stack.stackPush (selectStack st) n})
    adjacentN <- adjacent n
    mapM_ decrementDegree adjacentN

decrementDegree :: Temp -> ColorMonad ()
decrementDegree m = do
    st <- get
    let d = maybe (error ("degree not found #3324"++(show m))) (id) $ (degree st) M.!? m
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
    else if ((Set.member v (precolored st)) || (Set.member (u,v) (adjSet st))) then do
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
    let degreeU = maybe (error "!.232") (id) $ (degree st) M.!? u
    when ((not (Set.member u (precolored st))) && (not moveRelatedU) && (degreeU < (k st))) $ do
        put (st{freezeWorklist = (freezeWorklist st) Set.\\ (Set.singleton u),
                simplifyWorklist = (simplifyWorklist st) `Set.union` (Set.singleton u)})

ok :: Temp -> Temp -> ColorMonad Bool
ok t r = do
    st <- get
    let degreeT = maybe (error "!.233") (id) $ (degree st) M.!? t
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
    let moveListU = maybe (error "!.234") (id) $ (moveList st) M.!? u
    let moveListV = maybe (error "!.235") (id) $ (moveList st) M.!? v
    put (st{coalescedNodes = (coalescedNodes st) `Set.union` (Set.singleton v),
            alias = M.insert v u (alias st),
            moveList = M.insert u (moveListU `Set.union` moveListV) (moveList st)})
    enableMoves $ Set.singleton v --errata
    st <- get
    adjacentV <- adjacent v
    mapM_ (\t -> do
            addEdge t u
            decrementDegree t
            )
            (adjacentV)
    st <- get
    let degreeU = maybe (error "!.236") (id) $ (degree st) M.!? u
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
    mapM_ (\m -> do
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
        )
        (Set.elems nodeMovesU)

freezeMoves' :: Temp -> ColorMonad ()
freezeMoves' v = do
    st <- get
    nodeMovesV <- nodeMoves v
    let degreeV = maybe (error "!.237") (id) $ (degree st) M.!? v
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
                let adjListN = maybe (Set.empty) (id) $ (adjList st') M.!? n --NOTE: doy empty por default, capaz no esté bien
                mapM_ (\w -> do
                        aliasW <- getAlias w
                        st <- get
                        when (Set.member aliasW ((coloredNodes st) `Set.union` (precolored st)))
                            $ do
                                stc <- get --la 'c' de color (porque no quiero mas comillas)
                                let colorW = maybe (error "Impossible #3335") (id) $ (color stc) M.!? aliasW
                                put (st{okColors = (okColors st) Set.\\ (Set.singleton colorW)})
                    )
                    (Set.elems adjListN)
                st'' <- get
                if (Set.null (okColors st'')) then do
                    put (st''{spilledNodes = (spilledNodes st'') `Set.union` (Set.singleton n)})
                else do
                    let c = last $ Set.elems (okColors st'')
                    put (st''{coloredNodes = (coloredNodes st'') `Set.union` (Set.singleton n),
                              color = M.insert n c (color st'')})
            )
    st <- get
    mapM_ (\n -> do
            st <- get
            aliasN <- getAlias n
            let colorAliasN = maybe (error "M.!#3334") (id) $ (color st) M.!? aliasN
            put (st{color = M.insert n colorAliasN (color st)})
            )
            (coalescedNodes st)


-- RewriteProgram
rewriteProgram :: ColorMonad ()
rewriteProgram = do
    spills <- gets spilledNodes
    let spillslist = Set.elems spills --ordered list
    -- allocate memory locations
    accesses <- mapM allocSpilled spillslist
    -- create a new temp for each def and use
    -- in instruction insert store after each definition of v
    -- in instruction insert fetch before each use of v
    inss <- gets instructions
    (insss,temps) <- unzip <$> (mapM (rewriteSpilled2 accesses) (inss))
    -- put all v in a set newTemps
    let newTemps = Set.fromList $ concat temps
    modify (\st -> st{spilledNodes = Set.empty
                      ,initial = ((coloredNodes st) `Set.union` (coalescedNodes st)) `Set.union` newTemps
                      ,coloredNodes = Set.empty
                      ,coalescedNodes = Set.empty
                      ,instructions = concat insss
                      ,newTempMap = M.empty
                      })

allocSpilled :: Temp -> ColorMonad (Temp, Fr.Access)
allocSpilled t = do
    fr <- gets frame
    (fr', acc) <- allocLocal fr Abs.Escapa
    modify (\st -> st{frame = fr'})
    return (t,acc)

rewriteSpilled2 :: [(Temp, Fr.Access)] -> As.Instr -> ColorMonad ([As.Instr], [Temp])
rewriteSpilled2 accmap (IOPER oassem odst osrc ojmp) = do
    spilled <- gets spilledNodes
    let dstSpills = spilled `Set.intersection` (Set.fromList odst)
    let srcSpills = spilled `Set.intersection` (Set.fromList osrc)
    let spilledInstr = Set.elems $ dstSpills `Set.union` srcSpills -- get all spilled uniquely
    tempMap <- createTemps2 spilledInstr
    storeInss <- mapM (createStore2 accmap tempMap) $ Set.elems dstSpills
    fetchInss <- mapM (createFetch2 accmap tempMap) $ Set.elems srcSpills
    let reAssem = (As.replaceAssemMissing (M.fromList tempMap) oassem)
    let reDst = rewriteTemps2 tempMap odst
    let reSrc = rewriteTemps2 tempMap osrc
    let rewrittenOp = As.IOPER reAssem reDst reSrc ojmp
    return (fetchInss ++ [rewrittenOp] ++ storeInss, snd <$> tempMap)
rewriteSpilled2 accmap (IMOVE massem mdst msrc) = do
    spilled <- gets spilledNodes
    let dstSpills = spilled `Set.intersection` (Set.singleton mdst)
    let srcSpills = spilled `Set.intersection` (Set.singleton msrc)
    let spilledInstr = Set.elems $ dstSpills `Set.union` srcSpills -- get all spilled uniquely
    tempMap <- createTemps2 spilledInstr
    storeInss <- mapM (createStore2 accmap tempMap) $ Set.elems dstSpills
    fetchInss <- mapM (createFetch2 accmap tempMap) $ Set.elems srcSpills
    let reAssem = (As.replaceAssemMissing (M.fromList tempMap) massem)
    let reDst = rewriteTemp2 tempMap mdst
    let reSrc = rewriteTemp2 tempMap msrc
    let rewrittenOp = As.IMOVE reAssem reDst reSrc
    return (fetchInss ++ [rewrittenOp] ++ storeInss, snd <$> tempMap)
rewriteSpilled2 accmap other = return ([other],[])

createTemps2 :: [Temp] -> ColorMonad [(Temp,Temp)]
createTemps2  []     = return []
createTemps2 (t:ts) = do
    ts' <- createTemps2 ts
    s <- newTemp
    return $ (t,s):ts'

createFetch2 :: [(Temp, Fr.Access)] -> [(Temp, Temp)] -> Temp -> ColorMonad As.Instr
createFetch2 accmap tmpmap t = do
    let accmap' = M.fromList accmap
    let tmpmap' = M.fromList tmpmap
    let acc = maybe (error $ show ("CS2: accmap not found",t)) (id) $ accmap' M.!? t
    let newt = maybe (error $ show ("CS2: tmpmap not found",t)) (id) $ tmpmap' M.!? t
    let offset = case acc of (InFrame dir) -> dir
                             _             -> error "Esto no debe pasar #998"
    return $ IOPER{oassem = LW newt offset Fr.fp
                    , odst = [newt]
                    , osrc = [Fr.sp]
                    , ojmp = Nothing}

createStore2 :: [(Temp, Fr.Access)] -> [(Temp, Temp)] -> Temp -> ColorMonad As.Instr
createStore2 accmap tmpmap t = do
    let accmap' = M.fromList accmap
    let tmpmap' = M.fromList tmpmap
    let acc = maybe (error $ show ("CS2: accmap not found",t)) (id) $ accmap' M.!? t
    let newt = maybe (error $ show ("CS2: tmpmap not found",t)) (id) $ tmpmap' M.!? t
    let offset = case acc of (InFrame dir) -> dir
                             _             -> error "Esto no debe pasar #999"
    return $ IOPER{oassem = SW newt offset Fr.fp
                    , odst = []
                    , osrc = [Fr.fp, newt]
                    , ojmp = Nothing}

rewriteTemp2 :: [(Temp, Temp)] -> Temp -> Temp
rewriteTemp2 tmpmap t =
    let tmpmap' = M.fromList tmpmap
        newt = maybe (t) (id) (M.lookup t tmpmap')
    in newt

rewriteTemps2 :: [(Temp, Temp)] -> [Temp] -> [Temp]
rewriteTemps2 tmpmap []     = []
rewriteTemps2 tmpmap (t:ts) =
    let tmpmap' = M.fromList tmpmap
        newt = maybe (t) (id) (M.lookup t tmpmap')
        newts = rewriteTemps2 tmpmap ts
    in newt:newts



rewriteSpilled :: [(Temp, Fr.Access)] -> As.Instr -> ColorMonad ([As.Instr], [Temp])
rewriteSpilled accmap (IOPER oassem odst osrc ojmp) = do
    spilled <- gets spilledNodes
    -- create store after each definition
    let dstSpills = spilled `Set.intersection` (Set.fromList odst)
    dstTemps <- mapM createTemp $ Set.elems dstSpills
    storeInss <- mapM (createStore accmap) $ Set.elems dstSpills
    -- create fetch before each use
    let srcSpills = spilled `Set.intersection` (Set.fromList osrc)
    srcTemps <- mapM createTemp $ Set.elems srcSpills
    fetchInss <- mapM (createFetch accmap) $ Set.elems srcSpills
    reDst <- (rewriteTemps odst)
    reSrc <- (rewriteTemps osrc)
    ntempmap <- gets newTempMap
    let reAssem = (As.replaceAssemMissing ntempmap oassem)
    let rewrittenOp = As.IOPER reAssem reDst reSrc ojmp
    return (fetchInss ++ [rewrittenOp] ++ storeInss, concat $ dstTemps ++ srcTemps)
rewriteSpilled accmap (IMOVE massem mdst msrc) = do
    spilled <- gets spilledNodes
    -- create store after each definition
    let dstSpills = spilled `Set.intersection` (Set.singleton mdst)
    dstTemps <- mapM createTemp $ Set.elems dstSpills
    storeInss <- mapM (createStore accmap) $ Set.elems dstSpills
    -- create fetch before each use
    let srcSpills = spilled `Set.intersection` (Set.singleton msrc)
    srcTemps <- mapM createTemp $ Set.elems srcSpills
    fetchInss <- mapM (createFetch accmap) $ Set.elems srcSpills
    reDst <- (rewriteTemp mdst)
    reSrc <- (rewriteTemp msrc)
    ntempmap <- gets newTempMap
    let reAssem = (As.replaceAssemMissing ntempmap massem)
    let rewrittenOp = As.IMOVE reAssem reDst reSrc
    return (fetchInss ++ [rewrittenOp] ++ storeInss, concat $ dstTemps ++ srcTemps)
rewriteSpilled accmap other = return ([other],[])

createTemp :: Temp -> ColorMonad [Temp]
createTemp t = do
    ntempmap <- gets newTempMap
    if M.member t ntempmap then do
        return []
    else do
        s <- newTemp
        modify (\st -> st{newTempMap = M.insert t s (newTempMap st)})
        return $ [s]

createStore :: [(Temp, Fr.Access)] -> Temp -> ColorMonad As.Instr
createStore accmap t = do
    newTMap <- gets newTempMap
    let accmap' = M.fromList accmap
    let acc = maybe (error $ show ("createStore",t)) (id) $ accmap' M.!? t
    let newt = maybe (error $ show ("createStore2",t)) (id) $ newTMap M.!? t
    let offset = case acc of (InFrame dir) -> dir
                             _                -> error "Esto no debe pasar #199"
    return $ IOPER{oassem = SW newt offset Fr.fp
                    , odst = []
                    , osrc = [Fr.fp, newt]
                    , ojmp = Nothing}

createFetch :: [(Temp, Fr.Access)] -> Temp -> ColorMonad As.Instr
createFetch accmap t = do
    newTMap <- gets newTempMap
    let accmap' = M.fromList accmap
    let acc = maybe (error $ show ("createFetch",t)) (id) $ accmap' M.!? t
    let newt = maybe (error $ show ("createStore2",t)) (id) $ newTMap M.!? t
    let offset = case acc of (InFrame dir) -> dir
                             _              -> error "Esto no debe pasar #198"
    return $ IOPER{oassem = LW newt offset Fr.fp
                    , odst = [newt]
                    , osrc = [Fr.sp]
                    , ojmp = Nothing}

rewriteTemp :: Temp -> ColorMonad Temp
rewriteTemp t = do
    ntempmap <- gets newTempMap
    let newt = maybe (t) (id) (M.lookup t ntempmap)
    return $ newt

rewriteTemps :: [Temp] -> ColorMonad [Temp]
rewriteTemps []     = return []
rewriteTemps (t:ts) = do
    ntempmap <- gets newTempMap
    let newt = maybe (t) (id) (M.lookup t ntempmap)
    newts <- rewriteTemps ts
    return $ newt:newts


-- applyColors
applyColors :: ColorMonad ()
applyColors = do
    inss <- gets instructions
    color <- gets color
    let inss' = map (\ins -> As.replaceInstr color ins) inss
    modify (\st -> st{instructions = inss'})



-- Stack helper functions
toList :: Stack.Stack a -> [a]
toList stack =
    if Stack.stackIsEmpty stack then []
    else maybe (error "WTF") (\(stack',a) -> a : (toList stack')) (Stack.stackPop stack)

toSet :: (Ord a) => Stack.Stack a -> Set.Set a
toSet stack = Set.fromList (toList stack)