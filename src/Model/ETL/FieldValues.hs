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
import           Protolude
-------------------------------------------------------------------------------
-- import           Data.Set       (Set)
import qualified Data.Set       as S
import qualified Data.Vector    as V
-------------------------------------------------------------------------------
import           Model.ETL.Span
-------------------------------------------------------------------------------
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
    | SpanSet (Set Span) deriving (Show, Eq, Ord)

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


-- |
-- Is the collection empty/null?
--
null :: FieldValues -> Bool
null (TxtSet set)  = S.null set
null (IntSet set)  = S.null set
null (SpanSet set) = S.null set

-- |
fromListTxtValues :: [Text] -> FieldValues
fromListTxtValues vs = TxtSet $ fromList vs

fromListIntValues :: [Int] -> FieldValues
fromListIntValues vs = IntSet $ fromList vs

fromListSpanValues :: [Span] -> FieldValues
fromListSpanValues vs = SpanSet $ fromList vs

-- | Utilized by "Models.Request.Transformers"
fromList :: (Ord a) => [a] -> Set a
fromList = S.fromList

-- | Utilized by "Models.Request.Transformers"
toList :: (Ord a) => Set a -> [a]
toList = S.toList

-- | Private support function
fromVector :: (Ord a) => V.Vector a -> S.Set a
fromVector = S.fromList . V.toList

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
count (TxtSet set)  = S.size set
count (IntSet set)  = S.size set
count (SpanSet set) = S.size set

-- | Utilized by Expression
toSetInt :: FieldValues -> Set Int
toSetInt = unIntSet

-- | GraphQL documentation support
fieldValuesDes :: Text
fieldValuesDes =
           "A Set collection of values utilized by both Qualities and Components.\n\
          \ It is a sum type :: StrSet | IntSet | SpanSet that unifies the range of\n\
          \ types respresented in the leaves of the Obs object."
