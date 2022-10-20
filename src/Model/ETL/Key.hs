{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module     : Model.ETL.Key
-- Description: UI view
--
module Model.ETL.Key
  where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Aeson   (ToJSON (..), ToJSONKey (..))
-------------------------------------------------------------------------------
import           Model.ETL.ID
-------------------------------------------------------------------------------

-- ** Observation Node Keys
-- |
-- These are the node specific "hooks" used to search and retrieve the data.
--
-- /Note/: The order of the keys start from root. Each one maps to a
-- lookup function for the corresponding collection.
data Key =
      OIKey   !(Maybe ID)    -- ^ Obs ID Key
    | SubKey  !Text
    | QualKey !Text
    | MeaKey  !Text
    | CompKey !Text
    | SpanKey
    deriving (Show, Eq, Ord, Generic)

instance ToJSONKey Key
instance ToJSON Key

-- *** Key-related types and constuctors
-- **** Type synonyms = constructor names
-- |
--
type OIKey     = Key
type SubKey    = Key
type QualKey   = Key
type MeaKey    = Key
type CompKey   = Key
type SpanKey   = Key
type CoSpKey   = Key  -- ^ Comp or Span key
type FieldName = Key
-- others
type CompName  = Key
type QualName  = Key

-- *** Unwrap key values
-- |
-- Used to generate Response (outgoing) from the API data store.
--
unKey :: Key -> Text
unKey (OIKey (Just id)) = unID id
unKey (OIKey Nothing)   = "OID"        -- Less relevant
unKey (SubKey txt)      = txt
unKey (QualKey txt)     = txt
unKey (MeaKey txt)      = txt
unKey (CompKey txt)     = txt
unKey SpanKey           = "SpanKey"

-- *** Key-constructors from @Text@ values
-- |
-- /Note/: The keys do not always return non-null values.
--
mkOIKey :: Maybe Text -> OIKey
mkOIKey Nothing    = OIKey Nothing
mkOIKey (Just txt) = OIKey (Just (KeyID txt))

-- |
mkSubKey :: Text -> SubKey
mkSubKey = SubKey

-- |
mkQualKey :: Text -> QualKey
mkQualKey = QualKey

-- |
mkMeaKey :: Text -> MeaKey
mkMeaKey = MeaKey

-- |
mkCompKey :: Text -> CompKey
mkCompKey = CompKey

-- |
mkSpanKey :: SpanKey
mkSpanKey = SpanKey




---------------------------------------------------------------------------------
  --
