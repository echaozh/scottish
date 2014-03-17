{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Scotty apps with configuration and states
-}
module Web.Scottish (
    -- * Types
    Scottish, Scottish'
  , ScottishM, ScottishActionM, ScottishM', ScottishActionM'
    -- * App runners/converters
  , scottish, scottishApp, scottishOpts
  , scottish', scottishApp', scottishOpts', handleRaisedStatus
    -- * Configuratio/State accessors
    -- ** Shared by 'ScottyM' & 'ScottyActionM'
  , getConfig, getGlobalState, getLocalState, (>$<)
    -- ** 'ScottyActionM' only
  , setLocalState, modifyLocalState
    -- ** 'ScottyM' only
  , setConfig, modifyConfig
  , setGlobalState, modifyGlobalState
    -- * Re-exports from Web.Scotty.Trans
  , module Trans
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Identity

import           Data.Default
import           Data.IORef
import qualified Data.Text.Lazy as T

import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp (Port)

import           Web.Scotty.Trans
import qualified Web.Scotty.Trans as Trans hiding (scottyAppT, scottyOptsT,
                                                   scottyT)

data ScottishState c s s' = ScottishState { _config      :: c
                                          , _globalState :: s
                                          , _localState  :: s'
                                          }

$(makeLenses ''ScottishState)

instance (Default c, Default s, Default s')
      => Default (ScottishState c s s') where
    def = ScottishState def def def

-- | 'config' is read-only in 'ActionT', but read-write in 'ScottyT' for
-- initialization.
--
-- 'localState' is reinitialized to 'def' for every execution of each 'ActionT'.
--
-- 'globalState' may be a TVar, or other monadic mutable data types. Normally,
-- 'globalState' should not be necessary for server apps, as there may be
-- multiple instance of the server running, even across machines, and you sure
-- cannot make them share the same state. However, you may be able to do some
-- process-local caching or user interaction (say in games), with 'globalState'.
newtype Scottish config globalState localState a =
    Scottish { unScottish :: StateT (ScottishState config globalState
                                     localState) IO a }
    deriving (Functor, Monad, Applicative, MonadIO,
              MonadState (ScottishState config globalState localState))

-- | 'Scottish' monad without 'globalState'
type Scottish' c s' = Scottish c () s'

type ScottishM e c s s' = ScottyT e (Scottish c s s')
type ScottishActionM e c s s' = ActionT e (Scottish c s s')

instance ScottyError Status where
    -- for now, Scotty only `raise`s when input is bad
    stringError = either (const badRequest400) id . (toEnum<$>) . readEither
                  . T.pack
    showError   = T.pack . show

type ScottishM' c s' = ScottyT Status (Scottish' c s')
type ScottishActionM' c s' = ActionT Status (Scottish' c s')

-- | Run a scottish app with warp.
scottish :: (Default c, Default s, Default s')
         => Port
         -> ScottishM e c s s' ()
         -> IO ()
scottish p = (mkScottishRunners>>=) . flip (uncurry $ scottyT p)

-- | Turn a scottish app into a WAI one, which can be run with any WAI handler.
scottishApp :: (Default c, Default s, Default s')
            => ScottishM e c s s' ()
            -> IO Application
scottishApp = (mkScottishRunners>>=) . flip (uncurry scottyAppT)

-- | Run a scottish app with extra options.
scottishOpts :: (Default c, Default s, Default s')
             => Options -> ScottishM e c s s' () -> IO ()
scottishOpts opts = (mkScottishRunners>>=) . flip (uncurry $ scottyOptsT opts)

-- | Scottish app runner with 'Status' handler installed.
scottish' :: (Default c, Default s, Default s')
          => Port -> ScottishM Status c s s' () -> IO ()
scottish' p = (mkScottishRunners>>=) . flip (uncurry $ scottyT p)
              . handleRaisedStatus

-- | Scottish app converter with 'Status' handler installed.
scottishApp' :: (Default c, Default s, Default s')
             => ScottishM Status c s s' () -> IO Application
scottishApp' = (mkScottishRunners>>=) . flip (uncurry scottyAppT)
               . handleRaisedStatus

-- | Scottish app runner with 'Status' handler installed.
scottishOpts' :: (Default c, Default s, Default s')
              => Options -> ScottishM Status c s s' () -> IO ()
scottishOpts' opts = (mkScottishRunners>>=) . flip (uncurry $ scottyOptsT opts)
                     . handleRaisedStatus

mkScottishRunners :: (Default c, Default s, Default s', MonadIO n)
                  => IO (forall a. Scottish c s s' a -> n a,
                         Scottish c s s' Response -> IO Response)
mkScottishRunners = do
    shared <- newIORef undefined
    let initializer m = liftIO $ do
            (r, ss) <- runStateT (unScottish m) def
            writeIORef shared ss
            return r
        actionRunner m = do
            ss <- readIORef shared
            evalStateT (unScottish m) $ set localState def ss
    return (initializer, actionRunner)

getConfig :: (MonadTrans t) => t (Scottish c s s') c
getConfig = lift . use $ config

getGlobalState :: (MonadTrans t) => t (Scottish c s s') s
getGlobalState = lift . use $ globalState

setConfig :: c -> ScottishM e c s s' ()
setConfig = lift . assign config

modifyConfig :: (c -> c) -> ScottishM e c s s' ()
modifyConfig = lift . (config%=)

setGlobalState :: s -> ScottishM e c s s' ()
setGlobalState = lift . assign globalState

modifyGlobalState :: (s -> s) -> ScottishM e c s s' ()
modifyGlobalState = lift . (globalState%=)

getLocalState :: (ScottyError e) => ScottishActionM e c s s' s'
getLocalState = lift . use $ localState

setLocalState :: (ScottyError e) => s' -> ScottishActionM e c s s' ()
setLocalState = lift . assign localState

modifyLocalState:: (ScottyError e) => (s' -> s') -> ScottishActionM e c s s' ()
modifyLocalState = lift . (localState%=)

-- | 'Status' is a good candidate as an 'ScottyError' instance by itself. Call
-- this function to install a default handler to report the 'Status' when one is
-- raised.
--
-- Also, you may want to define instances of 'ScottyError' with tuples/records
-- containing 'Status', to provide more informative error pages.
handleRaisedStatus :: ScottishM Status c s s' () -> ScottishM Status c s s' ()
handleRaisedStatus = ((defaultHandler $ \e -> status e)>>)

-- | Lift a 'Scottish' function to a 'MonadTrans' wrapped 'Scottish' one.
(>$<) :: MonadTrans t
      => (a -> Scottish c s s' b)
      -> IdentityT (Scottish c s s') a
      -> t (Scottish c s s') b
infixl 4 >$<
f >$< x = lift $ runIdentityT x >>= f
