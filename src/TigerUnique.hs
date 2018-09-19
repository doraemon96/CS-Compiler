module TigerUnique where

import Control.Monad.State

class UniqueGen w where
    mkUnique :: w Integer

type StGen = State Integer
instance {-# OVERLAPS #-} UniqueGen StGen where
    mkUnique = modify (+1) >> get

instance (Monad m, UniqueGen m, MonadTrans t) => UniqueGen (t m) where
    mkUnique = lift mkUnique
