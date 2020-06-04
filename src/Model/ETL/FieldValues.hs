{-# OPTIONS_HADDOCK prune #-}
-- |
-- Module      : Model.ETL.FieldValues
-- Description : Hosts the unique levels of a data field
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
module Model.ETL.FieldValues
  (
  -- * Model FieldValues
    FieldValues(..)
  , QualValues
  , CompValues
  , getSpanValues
  , areSpanValues
  , subset
  , toCompValuesList
  , toQualValues
  , ToCompValues(..)

  -- * Re-exports of Span-related values
  , Span
  , FilterRange
  , mkSpan
  , mkSpanM
  , mkFilterRange
  , filterStart
  , filterEnd
  )
  where
-------------------------------------------------------------------------------
import           Protolude      hiding (toList)
-------------------------------------------------------------------------------
import           Data.Aeson     (ToJSON)
-------------------------------------------------------------------------------
import qualified Data.Set       as Set
-------------------------------------------------------------------------------
import           Model.ETL.Span (FilterRange, Span, filterEnd, filterStart,
                                 mkFilterRange, mkSpan, mkSpanM)
import qualified Model.ETL.Span as Span (fromListEtl, subset)
-------------------------------------------------------------------------------
  --
-- ** Overview
-- |
--  Defines the a unifying data type to host the range of data types
--  that describe a subject and measurement. The values represent
--  the unique values of a field that either describes a 'Model.ObsETL.Subject'
--  or 'Model.ObsETL.Measurements'.
--
--  /The value of the measurement itself can be anything (more often a @float@
--  or @Int@).  The value itself is not specified./
--
--  The expandable range of data types include
--
--  * Int
--  * Text
--  * 'Model.ETL.Span' - measurement-related only
--
-- The type is a sum type that is designed to capture every way possible of
-- describing the data.  Nothwithstanding, the sum type can be expanded as needed.
--
-- /Note/: The design requires that a any one instance of 'Model.ETL.FieldVales'
-- host a single type.
--
data FieldValues
    = TxtSet  (Set Text)
    | IntSet  (Set Int)
    | SpanSet (Set Span)
    | Empty   deriving (Show, Eq, Generic)

-- |
-- Engine that determines a relationship for any pair of sets
-- of the same type.
--
instance Ord FieldValues where
  TxtSet  s1 <= TxtSet  s2 = Set.isSubsetOf s1 s2
  IntSet  s1 <= IntSet  s2 = Set.isSubsetOf s1 s2
  SpanSet s1 <= SpanSet s2 = Set.toList s1 `isSubsetOf` Set.toList s2
    where
      isSubsetOf :: [Span] -> [Span] -> Bool
      isSubsetOf get' from' =
        length get' == length (mapMaybe (`go` from') get')

      -- subroutine for each item in the get' collection
      -- Does it have a subset in the collection of from'?
      go :: Span -> [Span] -> Maybe Span
      go getIt = find (Span.subset getIt)

      --
  _ <= _ = panic "Ord: Tried to compare two sets with different types"

instance Semigroup FieldValues where
  TxtSet  s1 <> TxtSet  s2 = TxtSet $ s1 <> s2
  IntSet  s1 <> IntSet  s2 = IntSet $ s1 <> s2
  SpanSet s1 <> SpanSet s2 = SpanSet . Set.fromList $ Set.toList s1 <> Set.toList s2
  _ <> _ = panic "Tried to combine values of different types"

instance Monoid FieldValues where
  mempty = Empty
  mappend = (<>)

-- |
-- The use of Ord to define a subset relation works where the universe is set by
-- the ETL data. When working with 'Model.ETL.Span' 'Ord' is insufficient to
-- capture the subset relation.
--
subset :: FieldValues -> FieldValues -> Bool
subset = (<=)

instance ToJSON FieldValues

-- |
-- Instantiation for Quality Values
--
class ToQualValues a where
  toQualValues :: a -> QualValues

instance ToQualValues [Text] where
  toQualValues = TxtSet . Set.fromList

instance ToQualValues [Int] where
  toQualValues = IntSet . Set.fromList

-- |
-- Instantiation for Component Values
--
class ToCompValues a where
  toCompValues :: a -> CompValues

instance ToCompValues [Text] where
  toCompValues = TxtSet . Set.fromList
instance ToCompValues Text where
  toCompValues = TxtSet . Set.singleton

instance ToCompValues [Int] where
  toCompValues = IntSet . Set.fromList
instance ToCompValues Int where
  toCompValues = IntSet . Set.singleton

instance ToCompValues [Span] where
  toCompValues = SpanSet . Span.fromListEtl
instance ToCompValues Span where
  toCompValues = SpanSet . Set.singleton

instance ToCompValues FieldValue where
  toCompValues (TxtValue v)  = TxtSet  $ Set.singleton v
  toCompValues (IntValue v)  = IntSet  $ Set.singleton v
  toCompValues (SpanValue v) = SpanSet $ Set.singleton v
  toCompValues EmptyValue    = Empty

-- |
-- Used to interpret CompReqValues Exp
--
toCompValuesList :: CompValues -> [CompValues]
toCompValuesList = fmap toCompValues . toList
  where
     toList :: FieldValues -> [FieldValue]
     toList (TxtSet vs)  = fmap TxtValue  (Set.toList vs)
     toList (IntSet vs)  = fmap IntValue  (Set.toList vs)
     toList (SpanSet vs) = fmap SpanValue (Set.toList vs)
     toList Empty        = []
-- |
-- transition state for CompReqValues Exp
--
data FieldValue
  = TxtValue Text
  | IntValue Int
  | SpanValue Span
  | EmptyValue

-- ** Type synonyms
-- |
-- Utilized by filterSpanValues
-- (not enforced by the compiler)
--
-- type SpanValues = FieldValues

-- |
-- QualValues only includes TxtSet and IntSet
type QualValues = FieldValues

-- | CompValues synonym
type CompValues = FieldValues

-- ** Flexible type synonyms
-- |
-- Improve the specificity of the type signature
--
-- type TxtSet  = FieldValues
-- type IntSet  = FieldValues
-- type SpanSet = FieldValues

-- ** Span-specific support function
-- |
-- Used to access the non-primitive Span data from the request context.
--
areSpanValues :: FieldValues -> Bool
areSpanValues (SpanSet _) = True
areSpanValues _           = False

-- |
--
getSpanValues :: FieldValues -> Maybe [Span]
getSpanValues (SpanSet vs) = Just $ Set.toList vs
getSpanValues _            = Nothing


---------------------------------------------------------------------------------
