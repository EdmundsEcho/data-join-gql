{- |
    Defines the data type @FieldValues@.  It is a unifying type to capture
    the range of types hosted in either of:

    * @ObsETL@
      * @Components@
      * @Qualities@

    * @Request@
      * @ReqComponent@

-}
module Model.ETL.FieldValues
  where
-------------------------------------------------------------------------------
import           Prelude            (String)
import           Protolude
-------------------------------------------------------------------------------
import           Data.Aeson         (ToJSON)
-------------------------------------------------------------------------------
-- import           Data.Set           (Set)
import qualified Data.Set           as Set
import qualified Data.Vector        as V
-------------------------------------------------------------------------------
import           Model.ETL.Fragment
import           Model.ETL.Span
-------------------------------------------------------------------------------
  --
{-# ANN module ("HLint: ignore Orphan instance"::String) #-}

{- | Unifying data type for a collection of FieldValues.
     The type is a sum type that unifies the three types that might be used to
     describe a subject's qualities or a measurement's components (including
     @Span@ to describe the time span of a measurement's value).

     /Note/: The design requires that a collection have only one primitive type.
             (e.g., a @component@ has a collection of values (a @Set@) with elements
             of a single type).
-}
data FieldValues
    = TxtSet  (Set Text)
    | IntSet  (Set Int)
    | SpanSet (Set Span) deriving (Show, Eq, Ord, Generic)

instance Fragment FieldValues where
  null (TxtSet set)  = Set.null set
  null (IntSet set)  = Set.null set
  null (SpanSet set) = Set.null set

  len (TxtSet set)  = Set.size set
  len (IntSet set)  = Set.size set
  len (SpanSet set) = Set.size set

instance FragmentPlus FieldValues Text where
  fromList = TxtSet . Set.fromList
  toList (TxtSet vs) = Set.toList vs
  filterF xs (TxtSet vs) = TxtSet $ Set.fromList xs `Set.intersection` vs

instance FragmentPlus FieldValues Int where
  fromList = IntSet . Set.fromList
  toList (IntSet vs) = Set.toList vs
  filterF xs (IntSet vs) = IntSet $ Set.fromList xs `Set.intersection` vs

instance FragmentPlus FieldValues Span where
  fromList = SpanSet . Set.fromList
  toList (SpanSet vs) = Set.toList vs
  filterF xs (SpanSet vs) = SpanSet $ Set.fromList xs `Set.intersection` vs

instance ToJSON FieldValues

-- |
-- Use to try and limit the types that can be used to instantiate
-- CompValues.
-- /Note/: This is a bit of hack compared to creating a separate
-- Sum type to split 'FieldValues' into those for 'Model.ETL.Qualities'
-- and 'Model.ETL.Components'.
class ToQualValues a where
  toQualValues :: a -> QualValues

-- |
-- Use to restrict types that can be used to instantiate CompValues
class ToCompValues a where
  toCompValues :: a -> CompValues

-- | Private support function
fromVector :: (Ord a) => V.Vector a -> Set.Set a
fromVector = Set.fromList . V.toList

-- * Type synonyms
-- (user support, not enforced by the compiler)
-- | Utilized by filterSpanValues
type SpanValues = FieldValues

-- | QualValues only includes TxtSet and IntSet
type QualValues = FieldValues

-- | CompValues synonym
type CompValues = FieldValues

-- * Flexible type synonyms
-- Improve the specificity of the type signature
-- (user support not enforced by the compiler)
type TxtSet  = FieldValues
type IntSet  = FieldValues
type SpanSet = FieldValues

-- * Utilized by the API
unTxtSet :: FieldValues -> Set Text
unTxtSet (TxtSet s) = s
unTxtSet _          = unwrapSetErr

unIntSet :: FieldValues -> Set Int
unIntSet (IntSet s) = s
unIntSet _          = unwrapSetErr

unSpanSet :: FieldValues -> Set Span
unSpanSet (SpanSet s) = s
unSpanSet _           = unwrapSetErr

-- | Private support for un__Set
unwrapSetErr :: a
unwrapSetErr = panic "unTxtSet: wrong type"

-- | Utilized by the "Models.Matrix.Filter"
count :: FieldValues -> Int
count (TxtSet set)  = Set.size set
count (IntSet set)  = Set.size set
count (SpanSet set) = Set.size set

-- | Utilized by Expression
toSetInt :: FieldValues -> Set Int
toSetInt = unIntSet

-- | GraphQL documentation support
fieldValuesDes :: Text
fieldValuesDes =
           "A Set collection of values utilized by both Qualities and Components.\n\
          \ It is a sum type :: StrSet | IntSet | SpanSet that unifies the range of\n\
          \ types respresented in the leaves of the Obs object."
