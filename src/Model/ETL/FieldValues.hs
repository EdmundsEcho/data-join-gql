{-# OPTIONS_HADDOCK prune #-}
-- |
-- Module      : Model.ETL.FieldValues
-- Description : Hosts the unique levels of a data field
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
--
module Model.ETL.FieldValues
  (
    FieldValues(..)
  , QualValues
  , CompValues
  , mkSpan
  , mkSpanM
  , getSpanValues
  , areSpanValues
  , ToQualValues(..)
  )
  where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Aeson     (ToJSON)
-------------------------------------------------------------------------------
-- import           Data.Set           (Set)
import qualified Data.Set       as Set
import qualified Data.Vector    as V
-------------------------------------------------------------------------------
import           Model.ETL.Span (Span, mkSpan, mkSpanM)
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
    | SpanSet (Set Span) deriving (Show, Eq, Generic)

-- |
-- Engine that determines a relationship for any pair of sets
-- of the same type.
--
instance Ord FieldValues where
  TxtSet s1 <= TxtSet s2 = Set.isSubsetOf s1 s2
  IntSet s1 <= IntSet s2 = Set.isSubsetOf s1 s2
  SpanSet s1 <= SpanSet s2 = s1 <= s2
  _ <= _ = panic "Ord: Tried to compare two sets with different types"

instance ToJSON FieldValues

-- |
-- Placeholder
--
-- May be better to enforce what types get instantiated
-- for Components and Qualities.
--
-- Use to try and limit the types that can be used to instantiate
-- CompValues.
--
class ToQualValues a where
  toQualValues :: a -> QualValues

-- |
-- Placeholder
--
-- Use to restrict types that can be used to instantiate CompValues.
-- For instance, 'Model.ETL.Span' is only a 'Model.ETL.Measurenents' relevant
-- concept.
--
class ToCompValues a where
  toCompValues :: a -> CompValues

-- Private support function
fromVector :: (Ord a) => V.Vector a -> Set.Set a
fromVector = Set.fromList . V.toList

-- ** Type synonyms
-- |
-- Utilized by filterSpanValues
-- (not enforced by the compiler)
--
type SpanValues = FieldValues

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

-- ** Other field values suport functions
-- |
--
unTxtSet :: FieldValues -> Set Text
unTxtSet (TxtSet s) = s
unTxtSet _          = unwrapSetErr
{-# DEPRECATED unTxtSet "" #-}

-- |
--
unIntSet :: FieldValues -> Set Int
unIntSet (IntSet s) = s
unIntSet _          = unwrapSetErr
{-# DEPRECATED unIntSet "" #-}

-- |
--
unSpanSet :: FieldValues -> Set Span
unSpanSet (SpanSet s) = s
unSpanSet _           = unwrapSetErr
{-# DEPRECATED unSpanSet "" #-}

-- local support
unwrapSetErr :: a
unwrapSetErr = panic "unTxtSet: wrong type"
{-# DEPRECATED unwrapSetErr "" #-}

-- |
-- Utilized by the "Models.Matrix.Filter"
--
count :: FieldValues -> Int
count (TxtSet set)  = Set.size set
count (IntSet set)  = Set.size set
count (SpanSet set) = Set.size set
{-# DEPRECATED count "use 'len' in the Fragment type class" #-}

-- |
-- Utilized by Expression (Matrix)
toSetInt :: FieldValues -> Set Int
toSetInt = unIntSet

-- |
-- GraphQL documentation support
--
fieldValuesDes :: Text
fieldValuesDes =
           "A Set collection of values utilized by both Qualities and Components.\n\
          \ It is a sum type :: StrSet | IntSet | SpanSet that unifies the range of\n\
          \ types respresented in the leaves of the Obs object."
{-# DEPRECATED fieldValuesDes "use the GQL schema" #-}



---------------------------------------------------------------------------------
