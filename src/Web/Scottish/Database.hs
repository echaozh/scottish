{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Database connection pool as an easy part of 'Scottish' app configuration.
-}
module Web.Scottish.Database where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

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
getPool = return . (^#poolLens) >$< getConfig

setPool :: (HasDatabaseConnectionPool conn config)
           => IO (Pool conn) -- ^ database connection pool creator in IO monad
           -> ScottishM e config s s' ()
setPool f = liftIO f >>= modifyConfig . set (cloneLens poolLens)
