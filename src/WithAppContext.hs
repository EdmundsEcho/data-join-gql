{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      : WithAppContext
-- Description : The set of type classes available to the application
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
module WithAppContext
  ( module WithAppContext,
    -- re-exports
    module ObsExceptions,
    ToJSON,
    -- Exception handling
    MonadThrow,
    MonadCatch,
    -- Logging
    LoggingT,
    NoLoggingT,
    MonadLogger,
    filterLogger,
    runStderrLoggingT,
    runNoLoggingT,
    newTVarIO,
    logDebugN,
    logErrorN,
    logInfoN,
    logWarnN,
    throw,
    -- State
    atomically,
    writeTVar,
    readTVar,
  )
where

---------------------------------------------------------------------------------
import Protolude
---------------------------------------------------------------------------------
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception.Safe
import Control.Monad.Logger
import Data.Aeson
  ( FromJSON,
    ToJSON,
    defaultOptions,
    encode,
    genericToEncoding,
    toEncoding,
  )
---------------------------------------------------------------------------------
import Data.Aeson.Encode.Pretty       hiding(Config)
import qualified Data.ByteString.Lazy as B
---------------------------------------------------------------------------------
import Model.ETL.ObsETL
import Model.ObsTest
import ObsExceptions
import Config
---------------------------------------------------------------------------------

-- |
-- Generic context required to run the GQL resolvers
--
-- Import to modules required to maintain a generic monad that can be
-- lifted into different contexts. For instance, `Servant` and testing.
type WithAppContext m =
  ( Typeable m,
    MonadReader Env m,
    MonadIO m,
    MonadCatch m,
    MonadLogger m,
    MonadThrow m
  )

--------------------------------------------------------------------------------

-- ** > Database a

-- |
-- > Database :: ObsETL Model
-- > Database :: ObsTest Model
data Database = Database
  { db :: !Data,
    status :: !Text
  }
  deriving (Show)

-- |
-- Extensible Sum Type to host predefined state objects
data Data
  = DataObsETL !ObsETL
  | DataObsTest !ObsTest
  | DataEmpty
  deriving (Show)

dbInit :: Database
dbInit =
  Database
    { db = DataEmpty,
      status = "Empty"
    }

dbNew :: ObsETL -> Database
dbNew obsETL =
  Database
    { db = DataObsETL obsETL,
      status = "Loaded"
    }

--------------------------------------------------------------------------------

-- ** Context for the GraphQL capacity

-- |
data Env = Env
  { database :: !(TVar Database),
    config :: !Config
  }

--------------------------------------------------------------------------------
-- == Logging capacity

-- |
--
-- TODO: The function converts Lazy -> Strict.  This is an expensive computation
-- that should be avoided.  Does encodePretty have a strict version?
--
-- /Note from David@morpheus/ To access the AppObs logging capacity
-- ~ runReaderT (runApp resolver) readerContext
logDebugF :: (ToJSON a, MonadLogger m) => a -> m ()
logDebugF = $(logDebug) . decodeUtf8 . B.toStrict . encodePretty

-------------------------------------------------------------------------------

-- |
-- Filter-out the debugging log messages
filterNoDebug :: LoggingT m a -> LoggingT m a
filterNoDebug = filterLogger noDebug
  where
    noDebug :: LogSource -> LogLevel -> Bool
    noDebug _ LevelDebug = False
    noDebug _ _ = True

-------------------------------------------------------------------------------

-- |
-- Custom log message
data LogMessage = LogMessage
  { message :: !Text,
    -- , timestamp    :: !UTCTime
    level :: !Text,
    lversion :: !Text,
    lenvironment :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON LogMessage

instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

-------------------------------------------------------------------------------
