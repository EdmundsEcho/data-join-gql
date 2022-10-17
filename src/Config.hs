-- |
-- Module      : Config
-- Copyright   : (c) Lucivia LLC, 2022
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
module Config (
    -- * types
    Config (..),
    ProjectId,
    FileShareCfg (..),
    -- * functions
    fileShareCfgFromEnv,
    mkBucketName,    -- :: ProjectId -> S3.BucketName
    toString         -- :: BucketName -> String
) where

--------------------------------------------------------------------------------
import System.Environment (getEnv)
import Data.String
import Protolude
import Prelude (error)
import Data.Text (pack, unpack)
--------------------------------------------------------------------------------
import qualified Amazonka.S3 as S3 (BucketName(..))
--------------------------------------------------------------------------------

-- ** Config

-- |
-- Provides capacity to configure the app at startup.
data Config = Config
  { port         :: !Int,
    mountPoint   :: !Text,
    dataDir      :: !Text,
    fileShareCfg :: !FileShareCfg
  }

-- |
-- warehouse.json data source
-- used to instantiate the graphql server
data FileShareCfg = FileShareCfg
 { region     :: !Text,
   hostBase   :: !Text,
   hostBucket :: !Text,
   accessId   :: !Text,
   secret     :: !Text
 }

-- |
-- Index value within the hostBucket
type ProjectId = Text

mkBucketName :: ProjectId -> S3.BucketName
mkBucketName = S3.BucketName

toString :: S3.BucketName -> String
toString (S3.BucketName txt) = unpack txt

-- |
-- Instantiate from ENV
-- These values need to be present in the ENV
fileShareCfgFromEnv :: IO FileShareCfg
fileShareCfgFromEnv = FileShareCfg
    <$> (pack <$> getEnv "S3_REGION")
    <*> (pack <$> getEnv "S3_HOST_BASE")
    <*> (pack <$> getEnv "S3_HOST_BUCKET")
    <*> (pack <$> getEnv "AWS_ACCESS_KEY_ID")
    <*> (pack <$> getEnv "AWS_SECRET_ACCESS_KEY")

-- |
-- Find and parse ENV values; throws exception when either
-- not found or fails to convert read :: String -> a
envRead :: Read a => String -> IO a
envRead key = do
  rawVal <- getEnv key
  case readMaybe rawVal of
    Just val -> return val
    Nothing -> error $ key <> ": Unable to parse " <> rawVal

-- |
-- Retrieve a value from ENV
-- Throws an exception if the value is not found
envFromString :: (IsString a) => String -> IO a
envFromString key = fromString <$> getEnv key
