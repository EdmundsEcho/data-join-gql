{-
   Unique ID data type.  Works with a Node ID generator.
-}
module Model.ETL.ID
  where
--------------------------------------------------------------------------------
import           Protolude
--------------------------------------------------------------------------------
import           Data.Aeson (ToJSON (..), Value (String))
--------------------------------------------------------------------------------

-- | Private
-- Unifies new generation ObsKeys with wrapping
-- previously generated IDs under type ID.
data ID = ProjectId Text
        | KeyID Text  deriving (Show, Ord, Generic)

instance ToJSON ID where
  toJSON = String . unID

instance Eq ID where
  (ProjectId a) == (ProjectId b) = a == b
  (KeyID a) == (KeyID b) = a == b
  (ProjectId a) == (KeyID b) = a == b
  (KeyID a) == (ProjectId b) = a == b

-- | Public
unID :: ID -> Text
unID (ProjectId i) = i
unID (KeyID i) = i

-- | Temporary fixed ID
mkID :: Text -> ID
mkID = ProjectId
