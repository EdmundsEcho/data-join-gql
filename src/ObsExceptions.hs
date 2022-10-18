{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module     : ObsExceptions
-- Description: Exceptions for the App
--
--
module ObsExceptions
  where
-------------------------------------------------------------------------------
import           Data.Text (unpack)
import           Protolude
-------------------------------------------------------------------------------
import           Prelude   (Show (..), String)
-------------------------------------------------------------------------------
-- |
--
data ObsException
  = ObsNotInitialized
  | ConfigError        Text
  | TypeException      (Maybe Text)  -- ^ Should never happen
  | ValueException     (Maybe Text)  -- ^ From request input
  | ObsOtherException
  | NoValueFound       (Maybe Text)  -- ^ From request input

-- |
instance Show ObsException where
  show ObsNotInitialized     = "The server was not initialized with a valid state object."
  show ObsOtherException     = "Something else when wrong"
  show (ConfigError mess)    = "The config is not properly formatted: " <> unpack mess
  show (ValueException mess) = "The value cannot be processed by this function: " <> toStr mess
  show (NoValueFound mess)   = "No value found: " <> toStr mess
  show (TypeException mess)  = "Type mismatch: " <> toStr mess

-- Local helper function
toStr :: Maybe Text -> String
toStr mess = unpack (fromMaybe (""::Text) mess)

-- |
instance Exception ObsException
