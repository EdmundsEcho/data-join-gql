{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- |
-- Module      : AppTypes
-- Description : Types specific for the Servant context
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- ** Overview
--
-- Provides the custom features/context for the app running in the 'Servant'
-- context.
--
-- Use in combination with 'WithAppContext'.
--
module AppTypes
  ( module AppTypes
  , module WithAppContext
  , module Config
  )
  where
--------------------------------------------------------------------------------
import           Protolude      hiding (State, Handler)
import           Servant        (Handler)
--------------------------------------------------------------------------------
-- App specific
import           Config
import           WithAppContext
--------------------------------------------------------------------------------
  --
-- ** The WebApp integrated into the GraphQL capacity.
-- |
-- Wraps the `Servant.Handler` with the Obs app context.
-- Note: the Handler monad is: ExceptT ServantErr IO a
--
newtype AppObs a =
    AppObs
        { iniApp :: ReaderT Env (LoggingT Handler) a
        }
    deriving newtype
             ( Functor
             , Applicative
             , Monad
             , MonadReader Env
             , MonadIO
             , MonadLogger
             , MonadThrow
             , MonadCatch
             -- , MonadError Text Already defined by Server
             )

--------------------------------------------------------------------------------
-- ** > Custom monad -> Handler
-- |
-- Natural transformation required to map the custom monad back to `Handler`.
--
-- Production version excludes debug log entries.
--
-- >:: AppObs a -> Handler a
--
nat :: Env -> AppObs a -> Handler a
nat env app = runStdoutLoggingT $ filterNoDebug  (
                 runReaderT (iniApp app) env
                 )

--------------------------------------------------------------------------------
  --
