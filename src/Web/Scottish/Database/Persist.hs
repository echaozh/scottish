{-# LANGUAGE FlexibleContexts #-}

module Web.Scottish.Database.Persist where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity

import Database.Persist.Sql

import Web.Scottish
import Web.Scottish.Database

runSql :: (MonadTrans t, HasDatabaseConnectionPool Connection config)
         => SqlPersistM a -> t (Scottish config s s') a
runSql = lift . (runIdentityT getPool>>=) . (liftIO.) . runSqlPersistMPool
