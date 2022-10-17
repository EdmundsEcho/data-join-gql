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
import Conduit
---------------------------------------------------------------------------------
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
import qualified Amazonka             as S3
import Config
---------------------------------------------------------------------------------
import Model.ETL.ObsETL
import Model.ObsTest
import ObsExceptions
import HttpClient (mkS3Env)
---------------------------------------------------------------------------------

-- |
-- Generic context required to run the GQL resolvers
-- (qualifies m)
--
-- Import to modules required to maintain a generic monad that can be
-- lifted into different contexts. For instance, `Servant` and testing.
type WithAppContext m =
  ( Typeable m,
    MonadReader Env m,
    MonadIO m,
    MonadUnliftIO m,
    MonadResource m,
    MonadCatch m,
    MonadLogger m,
    -- MonadUnliftIO m,
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
-- Static and intialized values
data Env = Env
  { -- | database to fodder the graphql request
    database :: !(TVar Database),
    -- | various constants
    config   :: !Config,
    -- | global, resusable session
    s3env    :: !S3.Env
  }

-- |
-- Use the app config to instantiate the Env that contains
-- the global s3session. Include a pointer to the database
-- placeholder.
mkAppEnv :: MonadIO m => TVar Database -> Config -> m Env
mkAppEnv db cfg = do
    s3env <- mkS3Env cfg

    pure $ Env { database = db
               , config = cfg
               , s3env = s3env
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
-- logger S3.Error . B.byteString $ encodeUtf8("Failed secret: " <> pack e)
-- endpoint: nyc3.digitaloceanspaces.com
-- bucket: spaces.lucivia.net
-- encryption_key: PEhJZkFNKlSnNV/dMtZYuQ==
-- https://luci-nyc-space.nyc3.cdn.digitaloceanspaces.com/
-- https://luci-space.sfo3.digitaloceanspaces.com/
--
-- logger S3.Error . B.byteString $ encodeUtf8("Failed secret: " <> pack e)
-- endpoint: nyc3.digitaloceanspaces.com
-- bucket: spaces.lucivia.net
-- encryption_key: PEhJZkFNKlSnNV/dMtZYuQ==
-- host_base sfo3.digitaloceanspaces.com
-- host_bucket luci-space.sfo3.digitaloceanspaces.com/
--
-- [default]
-- access_key = DO00GX6YWPRKWHTW97U6
-- host_base = nyc3.digitaloceanspaces.com
-- host_bucket = spaces.lucivia.net
-- add_headers =
-- bucket_location = US
-- check_ssl_certificate = True
-- check_ssl_hostname = True
-- cloudfront_host = cloudfront.amazonaws.com
-- connection_max_age = 5
-- connection_pooling = True
-- default_mime_type = binary/octet-stream
-- delay_updates = False
-- delete_after = False
-- delete_after_fetch = False
-- delete_removed = False
-- dry_run = False
-- enable_multipart = True
-- encoding = UTF-8
-- encrypt = False
-- follow_symlinks = False
-- force = False
-- get_continue = False
-- gpg_command = /usr/local/bin/gpg
-- gpg_decrypt = %(gpg_command)s -d --verbose --no-use-agent --batch --yes --passphrase-fd %(passphrase_fd)s -o %(output_file)s %(input_file)s
-- gpg_encrypt = %(gpg_command)s -c --verbose --no-use-agent --batch --yes --passphrase-fd %(passphrase_fd)s -o %(output_file)s %(input_file)s
-- gpg_passphrase = PEhJZkFNKlSnNV/dMtZYuQ==
-- guess_mime_type = True
-- host_base = nyc3.digitaloceanspaces.com
-- host_bucket = spaces.lucivia.net
-- human_readable_sizes = False
-- max_delete = -1
-- multipart_chunk_size_mb = 15
-- multipart_copy_chunk_size_mb = 1024
-- multipart_max_chunks = 10000
-- preserve_attrs = True
-- progress_meter = True
-- recv_chunk = 65536
-- reduced_redundancy = False
-- restore_days = 1
-- restore_priority = Standard
-- secret_key = 0PoX0nRBUSiY4aJbqRYaSLucRS2Nz+F4EOwpqEAtIRY
-- send_chunk = 65536
-- server_side_encryption = False
-- signature_v2 = False
-- signurl_use_https = False
-- skip_existing = False
-- socket_timeout = 300
-- ssl_client_cert_file =
-- ssl_client_key_file =
-- stats = False
-- stop_on_error = False
-- storage_class =
-- throttle_max = 100
-- upload_id =
-- urlencoding_mode = normal
-- use_http_expect = False
-- use_https = True
-- use_mime_magic = True
-- verbosity = WARNING
