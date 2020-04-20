{-
   Unique ID data type.  Works with a Node ID generator.
-}
module Model.ETL.ID
  where

import           Protolude

-- | Private
-- Unifies new generation ObsKeys with wrapping
-- previously generated IDs under type ID.
data ID = NewID Text
        | KeyID Text  deriving (Show, Ord, Generic)

instance Eq ID where
  (NewID a) == (NewID b) = a == b
  (KeyID a) == (KeyID b) = a == b
  (NewID a) == (KeyID b) = a == b
  (KeyID a) == (NewID b) = a == b

-- | Public
unID :: ID -> Text
unID (NewID i) = i
unID (KeyID i) = i

-- | Temporary fixed ID
mkID :: ID
mkID = NewID "OID001"
