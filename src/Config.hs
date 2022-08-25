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

--------------------------------------------------------------------------------

-- ** Config

-- |
-- Provides capacity to configure the app at startup.
data Config = Config
  { port :: Int,
    mountPoint :: Text,
    dataDir :: Text
  }

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
fromEnv :: IO Config
fromEnv = Config
    <$> envRead "PORT"
    <*> envRead "MOUNT_POINT"
    <*> envRead "DATA_DIR"

