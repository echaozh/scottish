{-# LANGUAGE FlexibleContexts #-}

{- | Make running Persistent based database transactions easy in Scottish apps.
-}
module Web.Scottish.Database.Persist where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Database.Persist.Sql

import Web.Scottish
import Web.Scottish.Database

runSql :: (MonadTrans t, HasDatabaseConnectionPool Connection config)
         => SqlPersistM a -> t (Scottish config s s') a
runSql s = liftIO . runSqlPersistMPool s >$< getPool
