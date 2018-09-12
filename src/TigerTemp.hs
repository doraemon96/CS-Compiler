{-# Language UndecidableInstances #-}
module TigerTemp where

import           Control.Monad.State

import           TigerSymbol
import           TigerUnique

type Label = Symbol
type Temp  = Symbol

makeStringT :: Temp -> String
makeStringT = unpack

makeStringL :: Label -> String
makeStringL = unpack

detgenTemp :: Integer -> Temp
detgenTemp i = pack ("T"++show i)

detgenLabel :: Integer -> Label
detgenLabel i = pack ("L"++show i)

-- | Clase generadora de temps, y labels
class TLGenerator w where
    newTemp :: w Temp
    newLabel :: w Label

instance (Monad m, UniqueGen m) => TLGenerator m where
  newTemp = detgenTemp <$> mkUnique
  newLabel = detgenLabel <$> mkUnique

instance (Monad m, TLGenerator m, MonadTrans t) => TLGenerator (t m) where
  newTemp = lift newTemp
  newLabel = lift newLabel
