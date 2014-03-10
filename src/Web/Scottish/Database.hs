{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Web.Scottish.Database where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity

import Data.Functor
import Data.Pool (Pool)

import Web.Scottish

class HasDatabaseConnectionPool conn config | config -> conn where
    poolLens :: ALens' config (Pool conn)

instance HasDatabaseConnectionPool conn (Pool conn) where
    poolLens = simple

instance HasDatabaseConnectionPool conn (Pool conn, a) where
    poolLens = _1

instance HasDatabaseConnectionPool conn (a, Pool conn) where
    poolLens = _2

instance HasDatabaseConnectionPool conn (Pool conn, a, b) where
    poolLens = _1

instance HasDatabaseConnectionPool conn (a, Pool conn, b) where
    poolLens = _2

instance HasDatabaseConnectionPool conn (a, b, Pool conn) where
    poolLens = _3

getPool :: (MonadTrans t, HasDatabaseConnectionPool conn config)
        => t (Scottish config s s') (Pool conn)
getPool = lift $ (^#poolLens) <$> runIdentityT getConfig

createPool :: (HasDatabaseConnectionPool conn config)
           => IO (Pool conn) -> ScottishM e config s s' ()
createPool f = liftIO f >>= modifyConfig . set (cloneLens poolLens)
