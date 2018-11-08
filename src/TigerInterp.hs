{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module TigerInterp where

import           Prelude                 hiding ( EQ
                                                , compare
                                                , (!!)
                                                )

import           TigerFrame
import           TigerSymbol
import           TigerTemp
import           TigerTree

import           Data.Map                      as M hiding (null)

import           Control.Arrow
import           Control.Monad.State

import           Debug.Trace

-- | Datos a almacenar en memoria.
data Dato
    -- | String constantes.
    = Str Symbol
    -- | Cuerpos de funciones constantes.
    | FBody (
        -- | Lista de acceso de los posibles argumentos.
        [Access]
        -- | Body de la función.
        , [Stm])
    -- | O puedo almacenar un entero.
    | DInt Int
    deriving (Show)

-- | Funcinoes auxiliares de proyección con errores significativos
getInt :: Dato -> Int
getStr :: Dato -> Symbol
getFBody :: Dato -> ([Access], [Stm])

getInt (DInt i) = i
getInt _        = error "NOT AN INT"

getStr (Str s) = s
getStr _       = error "NOT A Symbol?"

getFBody (FBody sts) = sts
getFBody _           = error "NOT A FUN"

data CPU = CPU
    { -- | Mem representa la memoria del CPU, básicamente los registros.
      mem    :: M.Map Temp Int
      -- | Representa la memoria RAM, mapea direcciones de memoria a datos.
    , wat    :: M.Map Int Dato
      -- | Mapea Labels a direcciones de memoria.
    , dat    :: M.Map Label Int
      -- | Buffer de salida, a donde imprime la llamada a print.
    , output :: [Symbol]
      -- | Buffer de entrada, de donde sacamos la entrada cuando thacemos getchar.
    , input  :: [Symbol]
    } deriving Show

getDat :: Label -> CPU -> Dato
getDat l cpu = wat cpu !! (dat cpu !! l)

uTemp :: Temp -> Int -> CPU -> CPU
uTemp t i cpu = cpu{mem = M.insert t i (mem cpu)}

printCpu :: CPU -> String
printCpu cpu =
  "----------\n" ++
  "RV : " ++ show (mem cpu ! rv) ++
               "\n" ++
               "Output: " ++ show (output cpu) ++
  "\n----------\n"

type RC = State CPU

-- Map helper test function
(!!) :: (Ord k , Show k, Show v) => Map k v -> k -> v
m !! k = maybe (error $ "No encontrada la clave: "
                      ++ show k
               ) id $ m !? k


-- | Función para chequear si es una función externa.
extCall :: Label -> Bool
extCall l = or $ fmap ((== l) . pack) ["print", "flush", "getchar"]

-- | Llamada externa a print.
-- Dada una dirección de memoria |i : Int| buscamos a que
-- label | mblab : Label | hace referencias y la concatenamos en
-- output (que sería la salida estandard)
printExec :: Int -> RC Int
printExec i = trace ("Llamada a print con argumento: " ++ show i) $ do
  env <- get
  let mblab = wat env !! i
  -- let mbstr = dat env !! getStr mblab
  put (env { output = output env ++ [getStr mblab] })
  return 1

-- | Dispatcher de funciones externas.
extDispatcher :: Label -> [Int] -> RC Int
extDispatcher "print" (x : _) = printExec x

-- | |compute| ejecuta las operaciones aritmeticas básicas.
compute :: BOp -> Int -> Int -> Int
compute Plus = (+)
compute _    = error "TODO"

-- | |compare| ejecuta las operaciones de relación entre enteros.
compare :: Relop -> Int -> Int -> Bool
compare EQ = (==)
compare _  = error "TODO"

-- | Exp :: TInt
-- Ejecución de expresiones, son instrucciones que retornan un entero.
iexp :: Exp -> RC Int
-- | Una expresión |Const i| retorna a |i|
iexp (Const i     ) = trace "Const" $ return i
-- | |Name l| representa lo que tenga asignado la label |l|, es decir, hay que buscarlo en memoria.
iexp (Name  n     ) = trace ("NAME " ++ show n) $
                      get >>= \e -> return $ (dat e) !! n
-- | Devolvemos lo que tenga el temporario.
iexp (Temp  t     ) = trace "Temp" $ get >>= \e -> return $ mem e !! t
-- | Computamos la operación |op|, viendo que valor toman los argumentos.
iexp (Binop op x y) = trace "Binop" $ do
  -- Evaluamos a |x|
  x' <- iexp x
  -- Evaluamos a |y|
  y' <- iexp y
  -- Computamos op con |x'| e |y'|
  return $ compute op x' y'
-- | Básicamente desreferenciamos a lo que apunte |e|
iexp (Mem e) = trace "Mem" $ do
  e'  <- iexp e
  env <- get
  return $ getInt $ wat env !! e'
-- | LLamada a función |f| con argumentos |es|.
-- Esto no está implementado totalmente, y posiblemente tampoco correctamente.
iexp (Call (Name f) es) = trace "Call" $ do
  -- Evaluamos cada uno de los argumentos.
  es'  <- mapM iexp es
  cpu <- get
  -- Chequeamos si es externa
  if extCall f
    -- En el caso que sea llamamos al dispatcher.
    then extDispatcher f es'
    else do
    -- En el caso que no sea externa, tendremos que hacer un poco más de trabajo.
    -- Esto depende de varias cosas, y lo que falta conectar es saber cómo se conectan
    -- los argumentos desde el punto de vista del llamante y del llamado. Acá se debería
    -- definir correctamente la convención de llamada.
    -- TODO: completar
    -- Buscamos la info de |f| cargada en la CPU. Esto nos da un |acc| y el |body|.
      let (acc, body) = getFBody $ getDat f cpu
      -- Deberíamos preparar bien la info de los argumentos, los access de estos
      -- con los argumentos reales que están en |es'|.
      ----------------------------------------
      -- WARNING! Seguramente hay que mejorar esto.
      mapM_ step
        $ zipWith (\a i -> Move (TigerFrame.exp a 0) (Mem (Const i))) acc es'
      ----------------------------------------
      -- TODO: Ejecutar el main?
      -- Buscar el resultado en rv y devolverlo.
      return $ mem cpu !! rv
-- En ppio no puede pasar otra cosa. A menos que estemos en un leng funcional ;)
iexp (Call _ _) = error "Puede pasar?"
-- | |Eseq| es la ejecución secuencial de los pasos.
iexp (Eseq s e) = trace "Eseq" $ step s >> iexp e

-- | Cada paso de la máquina es la ejecución de un |Stm| que puede derivar en la
-- necesidad de continuar ejecutando aún más |[Stm]|
step :: Stm -> RC [Stm]
-- Un label no hace nada.
step (Label _              ) = trace "Label" $ return []
-- Ejecutamos primeo a |l| y dsp |r|
step (Seq  l        r      ) = trace "Seq" $ return [l, r]
-- | Assm load
step (Move (Temp t) (Mem m)) = trace "Move" $ do
  -- Búscamos que entero representa a |m|
  dir  <- iexp m
  -- Desreferenciamos esa dirección
  wats <- gets wat
  let info = (getInt $ wats !! dir)
  -- Lo movemos a |t|
  modify $ \env -> env { mem = M.insert t info (mem env) }
  return []
-- step (Move (Temp t) (Name l)) = trace "Move" $ do
--   -- Búscamos la dirección a la que apunta "l"
--   dats <- gets dat
--   let dir = getInt $ dats !! l
--   -- Desreferenciamos esa dirección
--   wats <- gets wat
--   let info = (getInt $ wats !! dir)
--   -- Lo movemos a |t|
--   modify $ \env -> env { mem = M.insert t info (mem env) }
--   return []
-- El casogeneral del |Move| (en el que __no__ tenemos que desreferencias memoria),
-- es más sencillo.
step (Move (Temp t) src) = trace "Move" $ do
  -- Ejecutamos |src|
  val <- iexp src
  -- y movemos el resultado a |t|
  modify $ \env -> env { mem = M.insert t val (mem env) }
  return []
-- | Assm store
-- Igual que antes hacer un |Move| a una memoria
-- es lo mismo que hacer un /Store/ en assembler. Y por ende un cambia el mapa que usamos.
step (Move (Mem t) src) = trace "Move" $ do
  -- Búscamos que dirección de memoria representa |t|
  dir <- iexp t
  -- ejecutamos |src|
  val <- iexp src
  -- actualizamos la memoria.
  modify $ \env -> env { wat = M.insert dir (DInt val) (wat env) }
  return []
-- Move en el caso que no sea a memoria.
step (Move dst src) = trace "Move" $ do
  -- Computamos |dst| y |src|
  src' <- iexp src
  dst' <- iexp dst
  -- actualizamos la memoria.
  modify (\env -> env { wat = M.insert dst' (wat env ! src') (wat env) })
  return []
-- Ejecutar una expresión tirando el resultado.
step (ExpS e) = trace "ExpS" $ iexp e >> return []
-- El |Jump| queda sencillo, es simplemente búscar el código a ejecutar, y devolverlo.
step (Jump _ l) = trace "Jump"
                  $ gets (getDat l) >>= (return . snd . getFBody)
-- |CJump| es un jump condicional, no creo que lo usen pero es fácil de implementar.
step (CJump bop x y tt ff) = trace "CJump" $ do
  x' <- iexp x
  y' <- iexp y
  return
    $ if compare bop x' y' then [Jump (Const 0) tt] else [Jump (Const 0) ff]

-- | |runPC| es la función que va ejecutando paso a paso los diferentes |Stm|.
-- la idea es que toma una lista de |Stm| y la va ejecutando paso a paso con |step|,
-- ahora |step :: Stm -> RC [Stm] |, es decir que un paso puede devolvernos más |Stm|
-- a ejecutar, y lo único que hacemos es concternarlos, /salvo/ en el caso del |Jump|
-- que se olvida de lo que tenía que ejecutar después.
runPc :: [Stm] -> RC ()
runPc []              = return ()
runPc (l@Jump{} : _ ) = step l >>= runPc
runPc (x        : xs) = step x >>= \ys -> runPc (ys ++ xs)

--------------------------------------------------------------------------------
-- Definiciones para que la CPU corra.
--------------------------------------------------------------------------------

-- | Estado inicial de la CPU.
-- fp, sp, rv = 0.
emptyCPU :: CPU
emptyCPU = CPU
            M.empty
            M.empty M.empty [] []

-- | Dada una |CPU| y una lista de |[Stm]| ejecutamos dicha lista y obtenemos la
-- |CPU| resultante.
runInitial :: CPU -> [Stm] -> CPU
runInitial cpu prog = execState (runPc prog) cpu

-- | Función que búsca los posibles labels dentro de una sequencia de stms.
splitStms
  :: [Stm]
  ->
          -- | Lista de stms hasta encontrar un Label.
     ([Stm]
          -- | Segmentos Lable lista de Stmts debajo de él.
           , [(Label, [Stm])])
splitStms []               = ([], [])
splitStms ((Label l) : ts) = ([], splitLbls ts (l, []))
splitStms (t         : ts) = let (res, lbls) = splitStms ts in (t : res, lbls)

-- | Función auxiliar que claramente hace todo el trabajo. Básicamente va
-- acumulando hasta encontrar un Label, lo agrega al final de la lista, y pasa a
-- acumular otro label.
splitLbls :: [Stm] -> (Label, [Stm]) -> [(Label, [Stm])]
splitLbls []               ts      = [second reverse ts]
splitLbls ((Label l) : ts) rs      = (second reverse rs) : splitLbls ts (l, [])
splitLbls (t         : ts) (l, rs) = splitLbls ts (l, t : rs)


----------------------------------------
-- | Función que genera una nueva dirección de memoria. Lo usamos
-- para la asignación de direcciones a nombres en la CPU.
newDir :: State Int Int
newDir = do
    i <- get
    put (i+1)
    return i

loadLabels :: [(Label, Symbol)] -> State Int CPU -> State Int CPU
loadLabels [] st = st
loadLabels ((lbl, sym) : defs) st = do
    st' <- st
    dir <- newDir
    return (st'{dat = M.insert lbl dir (dat st')
               , wat = M.insert dir (Str sym) (wat st')})

loadLabCod :: [(Label, [Stm])] -> State Int CPU -> State Int CPU
loadLabCod [] cpu = cpu
loadLabCod ((lbl, cod) : res) cpu = do
  st' <- cpu
  dir <- newDir
  return (st'{dat = M.insert lbl dir (dat st')
             , wat = M.insert dir (FBody ([], cod)) (wat st')})

loadProcs :: [(Frame, [Stm])] -> State Int CPU -> State Int CPU
loadProcs [] cpu = cpu
loadProcs ((fr , fbody) : procs) cpu = do
  let fname = name fr
      (factBody, rests) = splitStms fbody
  fdir <- newDir
  cpu' <- loadLabCod rests cpu
  return (cpu'{
               dat = M.insert fname fdir (dat cpu')
             , wat = M.insert fdir (FBody (prepFormals fr , fbody)) (wat cpu')
              })

----------------------------------------
----------------------------------------
-- | Preparamos la CPU para que corra desde un estado inicial.
loadCPU
  ::
        -- | Fragmentos de funciones ya definidas. (fuera del main)
     [(Frame, [Stm])]
        -- | Strings.
  -> [(Label, Symbol)]
        -- | Básicamente el main.
  -> [Stm]
  -> CPU
loadCPU fs ss tmain
  = let
       (factMain , rests ) = splitStms tmain
       (cpuInit' , me) = runState (loadProcs fs $ loadLabels ss $ loadLabCod rests $ return emptyCPU ) 0
       -- Esto huele a caca infinito
       cpuInit = uTemp rv me $ uTemp fp me $ uTemp sp me cpuInit'
    in if null factMain
       then runInitial cpuInit (snd $ head rests)
       else runInitial cpuInit factMain
