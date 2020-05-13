-- |
-- Module     : Model.ETL.TagRedExp
-- Description: Reduced vs Expressed computation
--
-- Express or Reduce a subset of component values when computing the
-- associated Measurement value (organization EtlUnit :: Measurement).
-- Expressed will create a series of values, whereas Reduced will create
-- a single, summary computation.
--
-- The concept applies to time-periods 'Span' in the hosted ETL data and
-- 'CompReqValues'.
--
module Model.ETL.TagRedExp
  where

-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Aeson (ToJSON)
-------------------------------------------------------------------------------
  --
data TagRedExp a
  = Red a   -- ^ Reduced, summary computation
  | Exp a   -- ^ Expressed series
  deriving (Show, Eq, Ord, Generic)

instance ToJSON a => ToJSON (TagRedExp a)

-- |
-- Utility function
isRed :: TagRedExp a -> Bool
isRed (Red _) = True
isRed _       = False

-- |
-- Utility function
unTag :: TagRedExp a -> a
unTag (Red a) = a
unTag (Exp a) = a
