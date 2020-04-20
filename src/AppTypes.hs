{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : AppObsTypes
Description : The WebApp type definitions
-}
module AppTypes where

import           Control.Concurrent.STM (TVar)
import           Control.Exception.Safe hiding (Handler)
import           Control.Monad.Logger
import           Prelude                (Show (..))
import           Protolude              hiding (State)
import           Servant
--------------------------------------------------------------------------------
-- Data stored in Database
import           Model.ETL.ObsETL
import           Model.ObsTest
--------------------------------------------------------------------------------
-- |
-- == The WebApp integrated into the GraphQL capacity.
--
newtype AppObs a =
    AppObs
        { runApp :: LoggingT (ReaderT Env Handler) a
        }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Env
             , MonadIO
             , MonadLogger
             )

-- |
-- == > Custom monad -> Handler
-- Natural transformation required to map the custom monad back to `Handler`.
--
-- >:: AppObs a -> Handler a
--
nat :: Env -> AppObs a -> Handler a
nat env app = runReaderT (
               runStderrLoggingT (runApp app) ) env

--------------------------------------------------------------------------------
-- |
-- == Context for the GraphQL capacity
--
data Env =
    Env
        { database   :: TVar Database
        , reqHeaders :: forall a b . Maybe (Headers a b)
        , config     :: AppConfig
        }

--------------------------------------------------------------------------------
-- |
-- == Mostly a placeholder for now
-- Provides capacity to configure the app at startup
--
newtype AppConfig =
  AppConfig
    { port :: Int
    }

--------------------------------------------------------------------------------
-- |
-- == > Database a
-- > Database :: ObsETL Model
-- > Database :: ObsTest Model
--
data Database =
  Database
    { db     :: Data
    , status :: Text
    }

-- | Extensable Sum Type to host predefined state objects
data Data
  = DataObsETL ObsETL
  | DataObsTest ObsTest
  | DataEmpty

dbInit :: Database
dbInit =
  Database
    { db = DataEmpty
    , status = "Empty"
    }

-------------------------------------------------------------------------------
-- |
-- == Not yet implemented
--
data ObsException
  = ObsNotInitialized
  | MalformedConfig
  | ObsOtherException

-- |
instance Show ObsException where
  show ObsNotInitialized = "The server was not initialized with a valid state object."
  show ObsOtherException = "Something else when wrong"
  show MalformedConfig = "The input is not properly formatted"

-- |
instance Exception ObsException


-------------------------------------------------------------------------------
