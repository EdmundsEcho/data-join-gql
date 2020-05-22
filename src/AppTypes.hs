{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : AppObsTypes
Description : The WebApp type definitions
-}
module AppTypes where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Protolude              hiding (State)
--------------------------------------------------------------------------------
import           Control.Concurrent.STM (TVar)
--------------------------------------------------------------------------------
  -- Logging
--------------------------------------------------------------------------------
  -- Monad stack
import           Control.Exception.Safe hiding (Handler)
import           Control.Monad.Logger
--------------------------------------------------------------------------------
import           Servant
--------------------------------------------------------------------------------
-- App specific
import           Model.ETL.ObsETL
import           Model.ObsTest
--------------------------------------------------------------------------------
-- |
-- == The WebApp integrated into the GraphQL capacity.
--
newtype AppObs a =
    AppObs
        { iniApp :: ReaderT Env (LoggingT Handler) a
        }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Env
             , MonadIO
             , MonadLogger
             , MonadThrow
             , MonadCatch
             -- , MonadError Text Already defined by Server
             )

-- statelessResolver
--   :: (Monad m, RootResCon m event query mut sub)
--   => GQLRootResolver m event query mut sub
--   -> GQLRequest
--   -> m GQLResponse
-- statelessResolver root req =
--     renderResponse <$> runResultT (coreResolver root req)
--
-- coreResolver
--   :: forall event m query mut sub
--    . (Monad m, RootResCon m event query mut sub)
--   => GQLRootResolver m event query mut sub
--   -> GQLRequest
--   -> ResponseStream event m ValidValue
--
--
-- |
-- == > Custom monad -> Handler
-- Natural transformation required to map the custom monad back to `Handler`.
--
-- >:: AppObs a -> Handler a
--
nat :: Env -> AppObs a -> Handler a
nat env app = runStderrLoggingT (
                 runReaderT (iniApp app) env)

-- defaultFormatter = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Msg
-- defaultFormatterTH = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Loc <:> ": " <:> Msg
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
data LogMessage = LogMessage {
  message        :: !Text
  -- , timestamp    :: !UTCTime
  , level        :: !Text
  , lversion     :: !Text
  , lenvironment :: !Text
} deriving (Eq, Show, Generic)

instance FromJSON LogMessage
instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

-------------------------------------------------------------------------------
