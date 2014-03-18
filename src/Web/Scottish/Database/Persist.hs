{-# LANGUAGE FlexibleContexts #-}

{- | Make running Persistent based database transactions easy in Scottish apps.
-}
module Web.Scottish.Database.Persist where

import Control.Exception.Enclosed
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Functor

import Database.Persist.Sql

import Network.HTTP.Types.Status

import Web.Scottish
import Web.Scottish.Database

runSql :: (MonadTrans t, HasDatabaseConnectionPool Connection config)
       => SqlPersistM a -> t (Scottish config s s') a
runSql s = liftIO . runSqlPersistMPool s >$< getPool

-- | Catches any synchronous exceptions thrown when executing the database
-- transaction, and 'raise' an 'internalServerError500' instead.
runSql' :: (HasDatabaseConnectionPool Connection config)
        => SqlPersistM a -> ScottishActionM' config s' a
runSql' s = (runSql $ (Just <$> s) `catchAny` \_ -> return Nothing)
            >>= maybe (raise internalServerError500) return
