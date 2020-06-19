{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      : WithAppContext
-- Description : Constraints required to run the GQL resolvers
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
module WithAppContext
  ( module WithAppContext

  -- re-exports
  , module ObsExceptions
  , ToJSON

  -- Exception handling
  , MonadThrow
  , MonadCatch

  -- Logging
  , LoggingT
  , NoLoggingT
  , MonadLogger
  , filterLogger
  , runStderrLoggingT
  , runNoLoggingT
  , newTVarIO
  , logDebugN
  , logErrorN
  , logInfoN
  , logWarnN
  , throw

  -- State
  , atomically
  , writeTVar
  , readTVar
  )
  where
---------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy     as B
import           Protolude
---------------------------------------------------------------------------------
import           Data.Aeson               (FromJSON, ToJSON, defaultOptions,
                                           encode, genericToEncoding,
                                           toEncoding)
import           Data.Aeson.Encode.Pretty
---------------------------------------------------------------------------------
import           Control.Concurrent.STM   (TVar, newTVarIO, readTVar, writeTVar)
import           Control.Exception.Safe
import           Control.Monad.Logger
---------------------------------------------------------------------------------
import           Model.ETL.ObsETL
import           Model.ObsTest
import           ObsExceptions
---------------------------------------------------------------------------------
-- |
-- Generic context required to run the GQL resolvers
--
-- Import to modules required to maintain a generic monad that can be
-- lifted into different contexts. For instance, `Servant` and testing.
--
type WithAppContext m = ( Typeable m, MonadReader Env m, MonadIO m,
                          MonadCatch m, MonadLogger m, MonadThrow m )

--------------------------------------------------------------------------------
-- ** > Database a
-- |
-- > Database :: ObsETL Model
-- > Database :: ObsTest Model
--
data Database =
  Database
    { db     :: Data
    , status :: Text
    } deriving (Show)

-- |
-- Extensable Sum Type to host predefined state objects
data Data
  = DataObsETL  ObsETL
  | DataObsTest ObsTest
  | DataEmpty
  deriving (Show)

dbInit :: Database
dbInit =
  Database { db = DataEmpty
           , status = "Empty"
           }

--------------------------------------------------------------------------------
-- ** Context for the GraphQL capacity
-- |
--
data Env =
    Env { database :: TVar Database
        , config   :: Maybe AppConfig
        }

--------------------------------------------------------------------------------
-- ** AppConfig
-- |
-- Provides capacity to configure the app at startup.  Mostly a placeholder
-- for now
--
newtype AppConfig = AppConfig { port :: Int }

--------------------------------------------------------------------------------
-- == Logging capacity
-- |
--
-- TODO: The function converts Lazy -> Strict.  This is an expensive computation
-- that should be avoided.  Does encodePretty have a strict version?
--
-- /Note from David@morpheus/ To access the AppObs logging capacity
-- ~ runReaderT (runApp resolver) readerContext
--
logDebugF :: (ToJSON a, MonadLogger m) => a -> m ()
logDebugF = $(logDebug) . decodeUtf8 . B.toStrict . encodePretty

-------------------------------------------------------------------------------
-- |
-- TODO
-- Why the approach for filtering log messages is not made explicitely clear in
-- the docs is annoying.
--
filterNoDebug :: LoggingT m a -> LoggingT m a
filterNoDebug = filterLogger noDebug
  where
    noDebug :: LogSource -> LogLevel -> Bool
    noDebug = undefined
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
