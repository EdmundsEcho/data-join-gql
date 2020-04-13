{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Concurrent.STM (TVar)
import           Control.Exception.Safe hiding (Handler)
import           Control.Monad.Logger
import           Prelude                (Show (..))
import           Protolude              hiding (State)
import           Servant

import qualified Model.Types            as Model

--------------------------------------------------------------------------------
-- Database :: Model
data Database =
  Database
    { obsEtl :: Maybe Model.ObsEtl
    , status :: Text
    }

dbInit :: Database
dbInit =
  Database
    { obsEtl = Nothing
    , status = "Empty"
    }

-------------------------------------------------------------------------------
data Env =
    Env
        { database   :: TVar Database
        , reqHeaders :: forall a b . Maybe (Headers a b)
        , config     :: AppConfig
        }

newtype AppConfig =
  AppConfig
    { port :: Int
    }


type ErrorCode = Int

newtype AppObs a =
    AppObs
        { runApp :: ReaderT Env (LoggingT Handler) a
        }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Env
             , MonadIO
             , MonadLogger
             )

-- | Natural transformation required to map the custom monad back to `Handler`.
-- @:: AppObs a -> Handler a@
nat :: Env -> AppObs a -> Handler a
nat env app = runStderrLoggingT (
                 runReaderT (runApp app) env)

data ObsException
  = ObsNotInitialized
  | MalformedConfig
  | ObsOtherException

instance Show ObsException where
  show ObsNotInitialized = "The server was not initialized with a valid ObsEtl object."
  show ObsOtherException = "Something else when wrong"
  show MalformedConfig = "The input is not properly formatted"

instance Exception ObsException
