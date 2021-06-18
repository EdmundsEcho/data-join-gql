{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  , FieldValue(..)
  , QualValues
  , CompValues
  , areSpanValues
  , getSpanValues
  , subset
  , toCompValuesList
  , toQualValues
  , ToCompValues(..)
  , ValuePredicate(..)

  -- * Encode Include/Exclude semantic
  , ValuesReqEnum(..)
  , ValuesReq(..)
  , AntiRequestEnum(..)
  , unwrapReqEnum
  , toExcludeRequest
  , toIncludeRequest
  , fromExcludeRequest
  , fromIncludeRequest
  , switchToExclude
  , switchToInclude
  , areSpanValuesReqEnum
  , getSpanValuesReqEnum

  -- * Type-level features (not using ad-hoc polymorphism)
  , elemAt
  , encodeFieldValue
  , findIndex
  , filter
  , member
  , splitAt
  , take

  -- * Re-exports of Span-related values
  , Span
  , FilterRange(..)
  , mkSpan
  , mkSpanM
  , mkFilterRange
  , filterStart
  , filterEnd
  )
where
-------------------------------------------------------------------------------
import           Protolude               hiding ( toList
                                                , filter
                                                , take
                                                , splitAt
                                                )

import           Data.ByteString.Base64         ( decodeBase64Lenient
                                                , encodeBase64
                                                )
-------------------------------------------------------------------------------
import           Data.Aeson                     ( ToJSON )
-------------------------------------------------------------------------------
import qualified Data.Set                      as Set
-------------------------------------------------------------------------------
import           Model.ETL.Span                 ( FilterRange(..)
                                                , Span
                                                , filterEnd
                                                , filterStart
                                                , mkFilterRange
                                                , mkSpan
                                                , mkSpanM
                                                )
import qualified Model.ETL.Span                as Span
                                                ( fromListEtl
                                                , subset
                                                )
-------------------------------------------------------------------------------
--
-- ** Overview
-- | Defines the a unifying data type to host the range of data types
--  that describe a subject and measurement. The values represent
--  the unique values of a field that either describes a 'Model.ETL.ObsETL.Subject'
--  or 'Model.ETL.ObsETL.Measurements'.
--
--  The value of the measurement itself can be anything (more often a @float@
--  or @Int@).  The value itself is not specified.
--
--  The expandable range of data types include
--
--  * Int
--  * Text
--  * 'Model.ETL.Span.Span' - measurement-related only
--
-- The type is a sum type that is designed to capture every way possible of
-- describing the data.  Nothwithstanding, the sum type can be expanded as needed.
--
-- /Note/: The design requires that a any one instance of
-- 'Model.ETL.FieldVales.FieldValues'
-- host a single type.
--
data FieldValues
    = TxtSet  (Set Text)
    | IntSet  (Set Int)
    | SpanSet (Set Span)
    | Empty   deriving (Show, Eq, Generic)

-- | Engine that determines a relationship for any pair of sets
-- of the same type.
--
instance Ord FieldValues where
  TxtSet  s1 <= TxtSet  s2 = Set.isSubsetOf s1 s2
  IntSet  s1 <= IntSet  s2 = Set.isSubsetOf s1 s2
  SpanSet s1 <= SpanSet s2 = Set.toList s1 `isSubsetOf` Set.toList s2
   where
    isSubsetOf :: [Span] -> [Span] -> Bool
    isSubsetOf get' from' = length get' == length (mapMaybe (`go` from') get')

    -- subroutine for each item in the get' collection
    -- Does it have a subset in the collection of from'?
    go :: Span -> [Span] -> Maybe Span
    go getIt = find (Span.subset getIt)

      --
  _ <= _ = panic "Ord: Tried to compare two sets with different types"

instance Semigroup FieldValues where
  TxtSet s1 <> TxtSet s2 = TxtSet $ s1 <> s2
  IntSet s1 <> IntSet s2 = IntSet $ s1 <> s2
  SpanSet s1 <> SpanSet s2 =
    SpanSet . Set.fromList $ Set.toList s1 <> Set.toList s2
  _ <> _ = panic "Tried to combine values of different types"

instance Monoid FieldValues where
  mempty  = Empty
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
  toCompValues (TxtValue  v) = TxtSet $ Set.singleton v
  toCompValues (IntValue  v) = IntSet $ Set.singleton v
  toCompValues (SpanValue v) = SpanSet $ Set.singleton v
  toCompValues EmptyValue    = Empty

-- |
-- Used to interpret CompReqValues Exp
--
toCompValuesList :: CompValues -> [CompValues]
toCompValuesList = fmap toCompValues . valuesToList

-- |
--
valuesToList :: FieldValues -> [FieldValue]
valuesToList (TxtSet  vs) = fmap TxtValue (Set.toList vs)
valuesToList (IntSet  vs) = fmap IntValue (Set.toList vs)
valuesToList (SpanSet vs) = fmap SpanValue (Set.toList vs)
valuesToList Empty        = []


-- |
-- Extra features for working with FieldValues
filter :: ValuePredicate -> FieldValues -> FieldValues
filter (TxtFilter f) (TxtSet values) = TxtSet $ Set.filter f values
filter (IntFilter f) (IntSet values) = IntSet $ Set.filter f values
filter (SpanFilter f) (SpanSet values) = SpanSet $ Set.filter f values
filter _ Empty = panic "Tried to filter an empty collection"
filter _ _ = panic "Filter and FieldValues do not operate on the same type"

-- |
--
take :: Int -> FieldValues -> FieldValues
take num (TxtSet  values) = TxtSet $ Set.take num values
take num (IntSet  values) = IntSet $ Set.take num values
take num (SpanSet values) = SpanSet $ Set.take num values
take _   Empty            = panic "Tried to take from an empty collection"

-- |
-- This can throw an Error (outside the panic)
--
elemAt :: Int -> FieldValues -> Maybe FieldValue
elemAt num' fieldValues | num' < size fieldValues = Just $ go num' fieldValues
                        | otherwise               = Nothing
 where
  go num (TxtSet  values) = TxtValue $ Set.elemAt num values
  go num (IntSet  values) = IntValue $ Set.elemAt num values
  go num (SpanSet values) = SpanValue $ Set.elemAt num values
  go _   Empty            = panic "Unreachable"

-- |
--
size :: FieldValues -> Int
size (TxtSet  values) = Set.size values
size (IntSet  values) = Set.size values
size (SpanSet values) = Set.size values
size Empty            = 0


-- |
--
-- FieldValue -> Base64
--
-- ⚠️  This is not globally unique; requires appending the GraphQL type
--
-- ⬜ If remains useful, implement the global standard
--
-- Part of the pagination specification
-- Utilized by LevelsResolver
--
encodeFieldValue :: FieldValue -> Text
encodeFieldValue (TxtValue  x) = encodeBase64 $ encodeUtf8 x
encodeFieldValue (IntValue  x) = encodeBase64 . encodeUtf8 $ show x
encodeFieldValue (SpanValue x) = encodeBase64 . encodeUtf8 $ show x
encodeFieldValue EmptyValue    = panic "Tried to encode an empty value"

-- |
--
splitAt :: Int -> FieldValues -> (FieldValues, FieldValues)
splitAt num (TxtSet  values) = applyToTuple TxtSet $ Set.splitAt num values
splitAt num (IntSet  values) = applyToTuple IntSet $ Set.splitAt num values
splitAt num (SpanSet values) = applyToTuple SpanSet $ Set.splitAt num values
splitAt _   Empty            = panic "Tried to split an empty collection"

-- |
--    Base64 -> FieldValues -> Bool
--
member :: Text -> FieldValues -> Bool
member x (TxtSet  values) = Set.member (decode64ToTxt x) values
member x (IntSet  values) = Set.member (decode64ToInt x) values
member _ (SpanSet _)      = panic "Not yet supported"
member _ Empty            = False

-- |
--
-- ⬜ Create a wrapper for Base64 Text
--
--    Base64 -> FieldValues -> Int
--
-- Input is the base64 encoded key that is part of the Connection specification.
-- The key was created using a element of the set (a level/field value)
--
-- This can throw an Error
--
-- alternatively: use lookupIndex -> Maybe Int
--
findIndex :: Text -> FieldValues -> Int
findIndex x (TxtSet  values) = Set.findIndex (decode64ToTxt x) values
findIndex x (IntSet  values) = Set.findIndex (decode64ToInt x) values
findIndex _ (SpanSet _     ) = panic "Not yet supported"
findIndex _ Empty            = panic "Tried to view an empty collection"

-- |
-- base64 encoding of the levels
--
-- Base64 -> Text
--
-- ⬜ Make global id by appending the graphql type
--
decode64ToTxt :: Text -> Text
decode64ToTxt = decodeUtf8 . decodeBase64Lenient . encodeUtf8

-- |
-- 🚧  Need to figure out how to manage base64 key with source levels
--
decode64ToInt :: Text -> Int
decode64ToInt = panic "Not yet supported; only page using text values"

applyToTuple :: (a -> b) -> (a, a) -> (b, b)
applyToTuple f (a1, a2) = (f a1, f a2)

data ValuePredicate
  = TxtFilter (Text -> Bool)
  | IntFilter (Int -> Bool)
  | SpanFilter (Span -> Bool)


-- |
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
-- type QualValues = FieldValues
newtype ValuesReq (antiRequest :: AntiRequestEnum) = ValuesReq FieldValues
  deriving (Show, Eq, Ord, Generic)

instance ToJSON (ValuesReq 'Include)
instance ToJSON (ValuesReq 'Exclude)

-- |
-- Analogous to 'Model.ETL.TagRedExp'
--
-- 🚧 This was a scrappy add-on for version 0.1.4.0 that uses a combination
--    of a phantom type and an Enum that unifies the different phantom types
--    to pipe through the process until required.
--
--    The type is set by the @antiRequest@ field found both in the graphql
--    input schema and request view.  The information is used to modify the
--    relation prop in filter used in the MatrixSpec.
--
-- Conceptually,
--
--    👉 only relevant when conducting a "subset" request
--
--    👉 qualifies field values
--
--    👉 does not impact the search results
--
--    👉 scope of change is the relation specified in the filter
--
data AntiRequestEnum
  = Include
  | Exclude
  deriving (Show, Eq, Ord, Generic)

data ValuesReqEnum
  = ExcludeRequest (ValuesReq 'Exclude)
  | IncludeRequest (ValuesReq 'Include)
  | NA FieldValues
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ValuesReqEnum

-- |
-- This is a request-specific version of the FieldValues. The equivalent
-- extraction function for Exp vs Red is @Model.Request.toTupleCompReqValues@
--
unwrapReqEnum :: ValuesReqEnum -> (FieldValues, AntiRequestEnum)
unwrapReqEnum (ExcludeRequest (ValuesReq vs)) = (vs, Exclude)
unwrapReqEnum (IncludeRequest (ValuesReq vs)) = (vs, Include)
unwrapReqEnum (NA             vs            ) = (vs, Include)


toExcludeRequest :: FieldValues -> ValuesReqEnum --ValuesReq 'Exclude
toExcludeRequest = ExcludeRequest . ValuesReq

toIncludeRequest :: FieldValues -> ValuesReqEnum --ValuesReq 'Include
toIncludeRequest = IncludeRequest . ValuesReq

fromExcludeRequest :: ValuesReq 'Exclude -> FieldValues
fromExcludeRequest (ValuesReq vs) = vs

fromIncludeRequest :: ValuesReq 'Include -> FieldValues
fromIncludeRequest (ValuesReq vs) = vs

switchToExclude :: ValuesReq 'Include -> ValuesReq 'Exclude
switchToExclude (ValuesReq vs) = ValuesReq vs

switchToInclude :: ValuesReq 'Exclude -> ValuesReq 'Include
switchToInclude (ValuesReq vs) = ValuesReq vs


-- | QualValues synonym
-- | /Note:/ see also ValuesReqEnum
type QualValues = FieldValues

-- | CompValues synonym
-- | /Note:/ see also ValuesReqEnum
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
areSpanValuesReqEnum :: ValuesReqEnum -> Bool
areSpanValuesReqEnum = areSpanValues . fst . unwrapReqEnum

-- |
--
getSpanValues :: FieldValues -> Maybe [Span]
getSpanValues (SpanSet vs) = Just $ Set.toList vs
getSpanValues _            = Nothing

-- |
--
getSpanValuesReqEnum :: ValuesReqEnum -> Maybe [Span]
getSpanValuesReqEnum = getSpanValues . fst . unwrapReqEnum


---------------------------------------------------------------------------------
