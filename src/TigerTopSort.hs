{-# Language OverloadedStrings #-}
module TigerTopSort where

import           TigerAbs
import           TigerSymbol         (Symbol)

import           Control.Monad.State
import           Data.List

import qualified Data.Map            as M

type DepMap = M.Map Symbol [Symbol]

data GraphRet = GR { deps :: DepMap
                   , ret  :: [Symbol]
                   }

addT :: Symbol -> State GraphRet ()
addT x = modify (\st -> st{ret = x : ret st})

buildDepMap :: [(Symbol , Ty)] -> DepMap
buildDepMap [] = M.empty
buildDepMap ((sTy, NameTy s) : xs) = M.insertWith (++) sTy [s] (M.insertWith (++) s [] (buildDepMap xs))
buildDepMap ((sTy, RecordTy ss) : xs) = M.insertWith (++) sTy [] (buildDepMap xs)
--buildDepMap ((sTy, RecordTy ss) : xs) = buildDepMap (zip (repeat sTy) (fmap snd ss) ++ xs)
buildDepMap ((sTy, ArrayTy s) : xs) = M.insertWith (++) sTy [s] (buildDepMap xs)

removeSym :: Symbol -> DepMap -> DepMap
removeSym s = M.delete s
              -- ^ Borramos la lista de deps de s. No creo que sea necesario
              . M.map (delete s) -- Sacamos las deps sobre s

-- Es parecida, pero más rápida.
checkIncoming :: Symbol -> DepMap -> Bool
checkIncoming s = M.foldl (\b ss -> b || elem s ss) False

-- | Función que nos permite decidir si ya hemos terminado bien
-- nuestro trabajo
noEdges :: DepMap -> Bool
noEdges = M.foldl (\b rs -> b && null rs) True

iterador :: [Symbol] -- Símbolos sin dependencias. Aka no incoming edges.
         -> State GraphRet ()
iterador [] = do -- Si no tenemos que insertar nada nuevo
  depMap <- gets deps
  if noEdges depMap -- Chequeamos que ya no haya nada en el grafo.
    then return () -- Todo listo
    else error "Ciclo" -- Quedaron cosas, y eso significa ciclo!
iterador (s:ss) = do
  addT s -- Metemos a [s] a la lista de resultado
  ds <- gets ( flip (M.!) s . deps) -- Lista de dependencias de [s]
  modify (\st -> st{deps = M.delete s (deps st)}) -- Borramos la lista de dependencias.
  dps <- gets deps -- Vemos si quedó algún símbolo sin dependencias
  let s' = filter (not . flip checkIncoming dps) ds
  iterador (s' ++ ss) -- Y lo metemos

-- La idea es implementar el algoritmo de Kahn que pueden encontrar en la web
kahnSort :: [(Symbol, Ty)] -> [Symbol]
kahnSort xs = ret $ execState (iterador initialSyms) (GR initialDeps [])
  where
    initialDeps = buildDepMap xs
    initialSyms = filter (not . flip checkIncoming initialDeps) $ map fst xs

kahnSorter :: [(Symbol,Ty)] -> [(Symbol,Ty)]
kahnSorter xs = let ks = kahnSort xs
                in  map (\k -> maybe (error "WTF") (k,) (lookup k xs)) ks
