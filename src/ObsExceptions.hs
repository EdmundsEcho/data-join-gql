module ObsExceptions where

-------------------------------------------------------------------------------
import           Data.Text (unpack)
import           Protolude
-------------------------------------------------------------------------------
import           Prelude   (Show (..), String)
-------------------------------------------------------------------------------
-- |
-- == Not yet implemented
--
data ObsException
  = ObsNotInitialized
  | MalformedConfig
  | TypeException (Maybe Text)
  | ValueException (Maybe Text)
  | ObsOtherException
  | NoValueFound (Maybe Text)

-- |
instance Show ObsException where
  show ObsNotInitialized     = "The server was not initialized with a valid state object."
  show ObsOtherException     = "Something else when wrong"
  show MalformedConfig       = "The input is not properly formatted"
  show (ValueException mess) = "The value cannot be processed by this function: " <> toStr mess
  show (NoValueFound mess)   = "No value found: " <> toStr mess
  show (TypeException mess)  = "Type mismatch: " <> toStr mess

toStr :: Maybe Text -> String
toStr mess = unpack (fromMaybe (""::Text) mess)

-- |
instance Exception ObsException
