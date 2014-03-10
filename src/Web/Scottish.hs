{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Scottish (
    Scotty.Options, Scotty.ScottyError(..),
    Scottish, Scottish', ScottishM, ScottishActionM,
    scottish, scottishApp, scottishOpts,
    getConfig, getGlobalState, getLocalState,
    setConfig, modifyConfig,
    setGlobalState, modifyGlobalState
    ) where

import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Default
import           Data.Functor
import           Data.IORef
import qualified Data.Text.Lazy as T

import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp (Port)

import           Web.Scotty.Trans as Scotty (ActionT, Options, ScottyError(..),
                                             ScottyT,
                                             readEither, scottyAppT,
                                             scottyOptsT, scottyT)

data ScottishState c s s' = ScottishState { _config      :: c
                                          , _globalState :: s
                                          , _localState  :: s'
                                          }

$(makeLenses ''ScottishState)

instance (Default c, Default s, Default s')
      => Default (ScottishState c s s') where
    def = ScottishState def def def

newtype Scottish c s s' a =
    Scottish { unScottish :: StateT (ScottishState c s s') IO a }
    deriving (Functor, Monad, Applicative, MonadState (ScottishState c s s'),
              MonadIO)

instance MonadReader (ScottishState c s s') (Scottish c s s') where
    ask = get
    local f m = do
        s <- get
        modify f
        x <- m
        put s
        return x

type Scottish' c s' = StateT (ScottishState c () s') IO

type ScottishM e c s s' = ScottyT e (Scottish c s s')
type ScottishActionM e c s s' = ActionT e (Scottish c s s')

instance ScottyError Status where
    -- for now, Scotty only `raise`s when input is bad
    stringError = either (const badRequest400) id . (toEnum<$>) . readEither
                  . T.pack
    showError   = T.pack . show

type ScottishM' e c s' = ScottyT Status (Scottish' c s')
type ScottishActionM' e c s' = ActionT Status (Scottish' c s')

scottish :: (Default c, Default s, Default s')
         => Port -> ScottishM e c s s' () -> IO ()
scottish p = (mkScottishRunners>>=) . flip (uncurry $ scottyT p)

scottishApp :: (Default c, Default s, Default s')
            => ScottishM e c s s' () -> IO Application
scottishApp = (mkScottishRunners>>=) . flip (uncurry scottyAppT)

scottishOpts :: (Default c, Default s, Default s')
            => Options -> ScottishM e c s s' () -> IO ()
scottishOpts opts = (mkScottishRunners>>=) . flip (uncurry $ scottyOptsT opts)

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
getConfig = lift . view $ config

getGlobalState :: (MonadTrans t) => t (Scottish c s s') s
getGlobalState = lift . view $ globalState

setConfig :: c -> ScottishM e c s s' ()
setConfig = lift . assign config

modifyConfig :: (c -> c) -> ScottishM e c s s' ()
modifyConfig = lift . (config%=)

setGlobalState :: s -> ScottishM e c s s' ()
setGlobalState = lift . assign globalState

modifyGlobalState :: (s -> s) -> ScottishM e c s s' ()
modifyGlobalState = lift . (globalState%=)

getLocalState :: (ScottyError e) => ScottishActionM e c s s' s'
getLocalState = lift . view $ localState

setLocalState :: (ScottyError e) => s' -> ScottishActionM e c s s' ()
setLocalState = lift . assign localState

modifyLocalState:: (ScottyError e) => (s' -> s') -> ScottishActionM e c s s' ()
modifyLocalState = lift . (localState%=)
