{-# Language OverloadedStrings #-}

module TigerTopSort (kahnSorter) where

import           TigerAbs
import           TigerSymbol                    ( Symbol )

import           Control.Monad.State
import           Data.List

import Debug.Trace

import qualified Data.Map            as M

-- Implementación simple de el algoritmo de
-- [Kahn](https://en.wikipedia.org/wiki/Topological_sorting)
-- Deberían obviamente mejorar el código, particularmente si quieren dar errores
-- significativos

type DepMap = M.Map Symbol [Symbol]

data GraphRet = GR { deps :: DepMap
                   , ret  :: [Symbol]
                   }

addT :: Symbol -> StateT GraphRet Maybe ()
addT x = modify (\st -> st{ret = x : ret st})

seen :: Symbol -> DepMap -> DepMap
seen s = M.insertWith (++) s []
-- | buildDepMap construye el mapa de dependencia, asumiendo que los nombres que
-- encuentra en el camino están definidos previamente. Notar además que también
-- computa las dependencias generadas por los |RecordTy| que es lo que en el
-- compilador deberíamos evitar.

buildDepMap :: [(Symbol , Ty)] -> DepMap
buildDepMap [] = M.empty
buildDepMap ((sTy, NameTy s) : xs) = seen s $ M.insertWith (++) sTy [s] (buildDepMap xs)
buildDepMap ((sTy, RecordTy ss) : xs) = M.insertWith (++) sTy [] (buildDepMap xs)
--buildDepMap ((sTy, RecordTy ss) : xs) = buildDepMap (zip (repeat sTy) (fmap snd ss) ++ xs)
buildDepMap ((sTy, ArrayTy s) : xs) = seen s $ M.insertWith (++) sTy [s] (buildDepMap xs)

-- | removeSym saca complementamente un |Symbol| del mapa de dependencias
removeSym :: Symbol -> DepMap -> DepMap
removeSym s =
  M.delete s
              -- ^ Borramos la lista de deps de s. No creo que sea necesario
             . M.map (delete s)
              -- ^ Sacamos las deps sobre s

-- | Chequeamos que un nodo (a.k.a. |Symbol|) tenga o no una arista entrante
checkIncoming :: Symbol -> DepMap -> Bool
checkIncoming s = M.foldl (\b ss -> b || elem s ss) False

-- | Función que nos permite decidir si ya hemos terminado bien
-- nuestro trabajo
-- básicamente chequear que no haya más aristas.
noEdges :: DepMap -> Bool
noEdges = M.foldl (\b rs -> b && null rs) True

iterador :: [Symbol] -- Símbolos sin dependencias. Aka no incoming edges.
         -> StateT GraphRet Maybe ()
         -- ^ANTES: -> State GraphRet ()
iterador [] = do -- Si no tenemos que insertar nada nuevo
  depMap <- gets deps
  if noEdges depMap -- Chequeamos que ya no haya nada en el grafo.
    then return () -- Todo listo
    else lift Nothing -- Quedaron cosas, y eso significa ciclo!
    -- ^ANTES: else error "Ciclo!"
iterador (s:ss) = do
  addT s -- Metemos a [s] a la lista de resultado
  ds <- gets (flip (M.!) s . deps) -- Lista de dependencias de [s]
  modify (\st -> st { deps = M.delete s (deps st) }) -- Borramos la lista de dependencias.
  dps <- gets deps -- Vemos si quedó algún símbolo sin dependencias
  let s' = filter (not . flip checkIncoming dps) ds
  iterador (s' ++ ss) -- Y lo metemos

-- La idea es implementar el algoritmo de Kahn que pueden encontrar en la web
kahnSort :: [(Symbol, Ty)] -> Maybe [Symbol]
kahnSort xs = fmap ret $ execStateT (iterador initialSyms) (GR initialDeps [])
  where
    initialDeps = buildDepMap xs
    initialSyms = filter (not . flip checkIncoming initialDeps) $ map fst xs

kahnSorter :: [(Symbol,Ty)] -> Maybe [(Symbol,Ty)]
kahnSorter xs = do ks  <- kahnSort xs
                   ks' <- return $ filter (\k -> elem k (map fst xs)) ks 
                   return $ map (\k -> maybe (error "WTF") (k,) (lookup k xs)) ks'
