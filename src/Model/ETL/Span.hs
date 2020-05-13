{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module     : Model.ETL.Span
-- Description: Component for representing Time
--
-- This module is a separate version of the 'Components'. It relates to
-- 'Components' in that it is a way to slice a 'Measurements' into parts.
--
-- It defines the structure required both for the ETL Observation and
-- the requested version of time ('FilterRange'). In the 'Matrix' it has
-- unique processing features:
--
-- > Span -> [FilterRange]
--
module Model.ETL.Span
  ( Span(..)
  , Range(..)
  , FilterRange(FilterRange) -- pattern synonym
  , mkFilterRange            -- smart constructor
  , filterStart              -- record
  , filterEnd                -- record
  , showFilterRange
  , isWithin
  , filterSize
  )
  where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Aeson          (ToJSON)
-------------------------------------------------------------------------------
import           Model.ETL.TagRedExp
-------------------------------------------------------------------------------

-- | Span
-- Exp ~ Expressed, Red ~ Reduced
newtype Span = Span { span :: TagRedExp Range } deriving (Show, Eq, Generic)
instance ToJSON Span

-- | Ord used to test for a Subset relation between Spans.
instance Ord Span where
  Span (Exp (Range s1 l1)) <= Span (Exp (Range s2 l2)) = (s1 >= s2) && ((s1+l1) <= l2)
  Span (Red (Range s1 l1)) <= Span (Exp (Range s2 l2)) = (s1 >= s2) && ((s1+l1) <= l2)
  Span (Red ra1) <= Span (Red ra2) = ra1 <= ra2
  Span (Exp _)   <= Span (Red _)   = False   -- chosen to be consistent with subset

-- |
-- Predicate that returns `True` when a subset relation exists between Spans.
isWithin :: Span -> Span -> Bool
Span (Exp ra1) `isWithin` Span (Exp ra2) = ra1 <= ra2  --  Exp Exp uses subset
Span (Red ra1) `isWithin` Span (Exp ra2) = ra1 <= ra2  --  Red Exp uses subset
Span (Exp _  ) `isWithin` Span (Red _  ) = False      --  Exp Red is always False
Span (Red ra1) `isWithin` Span (Red ra2) = ra1 == ra2  --  Red Red uses equality

spanDes :: Text
spanDes = "A describes the time period over which the measurement value is relevant.\n\
          \ Each Span has a Range object that describes the start and length of the time.\n\
          \ Length is the number of units of time used to describe the data. Finally,\n\
          \ a measurement sample can be either a summary over time or taken on regular\n\
          \ intervals.  Span :: RED summary | EXP repeated measure."

-- |
-- Construct to host start and length the measurement value.
-- /Note/: Range only has meaning inside a Span construct.  Use FilterRange
-- to host information outside of the Span context.
data Range = Range
  { rangeStart  :: !Int
  , rangeLength :: !Int
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Range

-- | Support function to access Range values
spanEnd :: Range -> Int
spanEnd (Range s l) = s + l - 1

-- |
-- Structure that clearly defines the inclusive min and max values of a filter{{{
-- value.  e.g., FilterRange 0 3 = Where timeSpan BETWEEN 0 AND 3 (inclusive)
-- Span -> FilterRange
-- Red Range 0 6 = [FilterRange 0 5]
-- Red Range 2 3 = [FilterRange 2 4]
-- Exp Range 0 6 = [FilterRange 0 0, FilterRange 1 1, FilterRange 2 2]}}}
--
data FilterRange = FilterRange_ !Start !End
  deriving (Show, Eq, Ord, Generic)

-- |
-- Pattern match for FilterRange
--
pattern FilterRange :: Start -> End -> FilterRange
pattern FilterRange { filterStart, filterEnd } <- FilterRange_ filterStart filterEnd
{-# COMPLETE FilterRange :: FilterRange #-}

-- | FilterRange constructor
mkFilterRange :: Span -> [FilterRange]
mkFilterRange (Span (Red r)) = [FilterRange_ (rangeStart r) (spanEnd r)]
mkFilterRange (Span (Exp r)) = expand r
  where
    expand r' = go <$> [(rangeStart r')..(spanEnd r')]
    go s = FilterRange_ s s

-- | Utilized by Models.Expression
showFilterRange :: FilterRange -> Text
showFilterRange fr = show (filterStart fr) <> "_" <> show (filterEnd fr)

-- | Utilized by Models.Expression
filterSize :: FilterRange -> Size
filterSize (FilterRange s e) = e - s + 1

-- Private
type Start = Int -- inclusive
type Len   = Int -- useful to validate
type End   = Int -- inclusive
type Size  = Len
