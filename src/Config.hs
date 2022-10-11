-- |
-- Module      : Config
-- Copyright   : (c) Lucivia LLC, 2022
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
module Config (Config (..)) where

--------------------------------------------------------------------------------

import System.Environment
import Protolude
import Prelude (error, String)
-- import HttpClient (HttpManager)

--------------------------------------------------------------------------------

-- digital ocean space (using boto3)
-- S3_ACCESS_ID = "DO003ZZWAE34HMFMRCQ3"
-- S3_SECRET_KEY = "Y+4Ld8Cu/PCOgtMUmWTFR1O00T4g5YpR15tAX177PJI"
-- S3_URL = "https://luci-space.sfo3.digitaloceanspaces.com"
-- S3_REGION = "sfo3"

-- ** Config

-- |
-- Provides capacity to configure the app at startup.
data Config = Config
  { port         :: !Int,
    mountPoint   :: !Text,
    dataDir      :: !Text,
    -- fileShareCfg :: !FileShareCfg
    fileShareUri :: !String,
    region       :: !String,
    secret       :: !String,
    accessId     :: !String
  }

--data FileShareCfg = FileShareCfg
--  { url :: !Text,
--    region :: !Text,
--    secret :: !Text,
--    accessId :: !Text
--  }

--fileShareInstance :: FileShareCfg
--fileShareInstance = FileShareCfg
--  { accessId = "DO003ZZWAE34HMFMRCQ3"
--  , secret = "Y+4Ld8Cu/PCOgtMUmWTFR1O00T4g5YpR15tAX177PJI"
--  , url = "https://luci-space.sfo3.digitaloceanspaces.com"
--  , region = "sfo3"
--  }

-- |
-- Not utilized
envRead :: Read a => String -> IO a
envRead key = do
  rawVal <- getEnv key
  case readMaybe rawVal of
    Just val -> return val
    Nothing -> error $ key <> ": Unable to parse " <> rawVal

-- |
-- Not utilized
--fromEnv :: IO Config
--fromEnv = Config
--    <$> envRead "PORT"
--    <*> envRead "MOUNT_POINT"
--    <*> envRead "DATA_DIR"

