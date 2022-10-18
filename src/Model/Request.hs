{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures        #-}
-- {-# LANGUAGE PatternSynonyms       #-}

-- |
-- Module      : Model.Request
-- Description : User specified in the UI "workbench"
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
-- = Overview
--
--   The @Request@ is instantiated from @ObsETL@, user input and a
--   validation function.
--
--   @ ObsETL -> Request -> Matrix @
--
-- = Summary
--
--  This module echos the @ObsETL@ structure transformed by the user request.
--  The @Request@ data structure is similar to but different than the @ObsETL@.
--
--  The structure is instantiated using a subset of the ETL data.
--
--  subReq arm  -> length of the matrix
--  meaReqs arm -> number of fields in the matrix
--
module Model.Request
  (
  -- * Request
    Request(..)
  , validate

  -- * Branches
  -- ** Quality-related
  , QualityMix(..)
  , ReqQualities(..)
  , toListReqQualities
  , minQualityMix
  , minSubResult
  , getReqQualityNames
  , getReqQualityNames'

  -- ** Component-related
  , ComponentMixes(..)
  , toListComponentMixes
  , ReqComponents(..)
  , toListReqComponents
  , toListCompReqSpans
  , fromCompValues
  , toCompValuesList

  -- ** FieldValues-related
  , CompReqValues(..)
  , mkSpan
  , toTupleCompReqValues
  , Span
  , areSpanValues
  , isRed
  , isExp
  , isExcludeRequest
  , isIncludeRequest
  )
where
---------------------------------------------------------------------------------
import           Protolude               hiding ( null
                                                , toList
                                                )
---------------------------------------------------------------------------------
import           Data.Coerce
import           Data.Maybe                     ( fromJust )
---------------------------------------------------------------------------------
import           Data.Aeson                     ( ToJSON )
---------------------------------------------------------------------------------
import           Data.Map.Strict                ( mapWithKey
                                                , union
                                                )
import qualified Data.Map.Strict               as Map
                                                ( keys
                                                , null
                                                , size
                                                , toList
                                                )
---------------------------------------------------------------------------------
import           Model.ETL.Components    hiding ( null
                                                , toList
                                                )
import           Model.ETL.FieldValues   hiding ( areSpanValues
                                                , toCompValuesList
                                                , filter
                                                )
import qualified Model.ETL.FieldValues         as Values
                                                ( areSpanValuesReqEnum
                                                , toCompValuesList
                                                )
import           Model.ETL.Fragment      hiding ( toList )
import qualified Model.ETL.Fragment            as Fragment
                                                ( toList )

import           Model.ETL.Key
import           Model.ETL.TagRedExp     hiding ( isRed )
import qualified Model.ETL.TagRedExp           as Tag
                                                ( isRed )
---------------------------------------------------------------------------------
import           Model.SearchFragment
import           Model.Status
---------------------------------------------------------------------------------

-- * Request

-- | Request is a subset of the ObsETL
-- @ ~validate :: RequestInput -fetch-> Request @
--
data Request (status::Status) = Request
  { subReq  :: !QualityMix
  , meaReqs :: !ComponentMixes
  , meta    :: ![(Property, Value)]
  } deriving (Show, Eq, Generic)

type Property = Text
type Value = Text

instance ToJSON (Request status)

-- |
-- == Placeholder
--
-- === Ideas for subsequent use
--
-- 1. Validating a structure where changes can only be made by the user
--     * Including at least one 'Model.ETL.Quality' in the request
--     * Ensuring the required, and single, 'Model.ETL.Span' component for
--       every @Measurement@ in the 'Model.Request.ComponentMixes' fragment
--       of the request
--
-- 1. Enforcing a non-redundant structure. For instance:
--     * Consolidating Subject requests
--     * Consolidating each mix in 'Model.Request.ComponentMixes'
--     * Including a default 'Model.ETL.Span' request value
--     * Removing a so called-subset of values Eq to fullset
--
-- === Considerations
--
-- * The position in the workflow for when to optimize the 'Model.Request'
-- needs to consider the user experience; how will the change impact how
-- the user is working and thinking about the request?
--
-- * It will also need to consider how the computation is redundant or
-- compliments the @Request -> Matrix@ computions already in-place.
--
-- * Finally, it will need to consider coordinating with the meta-data reporting.
-- (e.g., 'Model.Request.FieldCount').
--
validate :: Maybe (Request 'Inprocess) -> Maybe (Request 'Success)
validate (Just req) = Just $ coerce req
validate Nothing    = Nothing

-- |
-- Tag whether the request was delivered as expected; true when the request is a
-- subset of the ETL data.
--
data EtlContent
  = Subset       -- ^ Returned the full request
  | Intersection -- ^ Returned a partial request
  | Unknown      -- ^ Status not yet complete
  deriving (Show, Eq, Generic)

instance ToJSON EtlContent

---------------------------------------------------------------------------------
  -- Subject arm
---------------------------------------------------------------------------------
--
-- | @ map :: ETL values -> Requested values @
--
data QualityMix = QualityMix
  { subjectType :: !Key
  , qualityMix  :: !(Maybe ReqQualities)
  } deriving (Show, Eq, Generic)

instance ToJSON QualityMix

-- |
--
minQualityMix, minSubResult :: SubKey -> QualityMix
minQualityMix key = QualityMix { subjectType = key, qualityMix = Nothing }
minSubResult = minQualityMix

-- |
-- Encoding
--
--   - QualKey Nothing => display the quality field, select all levels
--
--   - QualKey Just vs => display the quality field, select levels
--
-- 🔖  QualValues is a type alias for 'Model.ETL.FieldValues.FieldValues'
--
-- 🏷️  0.1.4.0: Update use 'Model.ETL.FieldValues.ValuesReqEnum' instead of
--   'Model.ETL.FieldValues.QualValues' and 'Model.ETL.FieldValues.CompValues'
--
newtype ReqQualities = ReqQualities
        { reqQualities :: Map QualKey (Maybe ValuesReqEnum)
        } deriving (Show, Eq, Ord, Generic)

instance Fragment ReqQualities where
  null (ReqQualities vs) = Map.null vs
  len (ReqQualities vs) = Map.size vs

instance ToJSON ReqQualities

-- |
-- Limited to matching on key values with left bias
--
-- ⬜ Prioritize bias using the value of Maybe
--
instance Semigroup ReqQualities where
  (ReqQualities a) <> (ReqQualities b) = ReqQualities $ union a b

-- | Left bias
instance Monoid ReqQualities where
  mempty = ReqQualities mempty
  (ReqQualities a) `mappend` (ReqQualities b) = ReqQualities $ union a b


-- |
-- Utilized by 'Api.GQL.RequestView'
--
-- 🔖 mkQualityMix is defined in the 'Model.Search' module.
--
-- ⬜ Consider changing the input type to @'ETLSubset@
--
--
toListReqQualities :: ReqQualities -> [(QualKey, Maybe ValuesReqEnum)]
toListReqQualities (ReqQualities vs) = Map.toList vs

getReqQualityNames :: ReqQualities -> [Text]
getReqQualityNames = fmap unKey . Map.keys . reqQualities

getReqQualityNames' :: SearchFragment ReqQualities 'ETLSubset -> [Text]
getReqQualityNames' = getReqQualityNames . coerce

---------------------------------------------------------------------------------
  -- Measurement arm
---------------------------------------------------------------------------------
-- |
-- A collection of specifications for a series of fields to be included
-- in the data request.
--
newtype ComponentMixes = ComponentMixes
        { componentMixes :: Map MeaKey (Maybe ReqComponents)
        } deriving (Show, Eq, Ord, Generic)

instance Fragment ComponentMixes where
  null (ComponentMixes vs) = Map.null vs
  len (ComponentMixes vs) = Map.size vs

instance ToJSON ComponentMixes

instance Semigroup ComponentMixes where
  (ComponentMixes a) <> (ComponentMixes b) = ComponentMixes $ union a b

instance Monoid ComponentMixes where
  mempty = ComponentMixes mempty
  mappend (ComponentMixes a) (ComponentMixes b) = ComponentMixes $ union a b


-- |
-- Utilized by 'Api.GQL.RequestView'
-- /Note/ fromList is defined in the 'Model.Search' module.
--
toListComponentMixes :: ComponentMixes -> [(MeaKey, Maybe ReqComponents)]
toListComponentMixes (ComponentMixes v) = Map.toList v

-- * ReqComponents
-- |
--
-- Each value specifies a series of fields to be included in the data request.
--
-- Encoding
--
--   - CompKey Nothing => create a series of fields using all levels (Exp)
--
--   - CompKey Just vs =>
--
--      - Exp: create a series using the levels specified
--
--      - Red: create a single summary field using the levels specified
--
-- 🔖 @CompReqValues@ are @FieldValues@ tagged using @TagRedExp@.
--
-- 🦀 The Map structure requires merging the request when the key is already
--    registered. This seems to be an extra lift caused by the fact
--    that subset requests that are reduced, cannot be combined.
--
newtype ReqComponents = ReqComponents
        { reqComponents :: Map CompKey (Maybe CompReqValues)
        } deriving (Show, Eq, Ord, Generic)


instance Fragment ReqComponents where
  null (ReqComponents vs) = Map.null vs
  len (ReqComponents vs) = Map.size vs

instance ToJSON ReqComponents

instance Semigroup ReqComponents where
  (ReqComponents a) <> (ReqComponents b) = ReqComponents $ union a b

instance Monoid ReqComponents where
  mempty = ReqComponents mempty
  mappend (ReqComponents a) (ReqComponents b) = ReqComponents $ union a b

-- |
-- 🔖
--
--   * (CompKey, Nothing) may be used to encode all Exp All Values
--
--   * However, 'fromReqComponents' eliminates Nothing
--
toListReqComponents :: ReqComponents -> [(CompKey, Maybe CompReqValues)]
toListReqComponents (ReqComponents vs) = Map.toList vs


-- |
--   Internal wrapper to express Reduced vs Expressed request computation
--   of the associated measurement value. So, just the same FieldValues
--   plus an extra tag.  This fits throughout further down the Request tree.
--
newtype CompReqValues = CompReqValues { values :: TagRedExp ValuesReqEnum }
  deriving (Show, Eq, Ord, Generic)

-- |
--
isRed, isExp :: CompReqValues -> Bool
isRed CompReqValues { values } = Tag.isRed values
isExp = not . isRed

-- |
--
isExcludeRequest, isIncludeRequest :: CompReqValues -> Bool
isExcludeRequest CompReqValues { values } =
  case snd . unwrapReqEnum $ unTag values of
    Exclude -> True
    _       -> False
isIncludeRequest = not . isExcludeRequest

-- |
--
areSpanValues :: CompReqValues -> Bool
areSpanValues (CompReqValues vs) = Values.areSpanValuesReqEnum (unTag vs)

-- |
--
instance Fragment CompReqValues where
  null (CompReqValues (Red vs)) = null vs
  null (CompReqValues (Exp vs)) = null vs
  len (CompReqValues (Red vs)) = len vs
  len (CompReqValues (Exp vs)) = len vs


toListCompReqSpans :: CompReqValues -> [Span]
toListCompReqSpans (CompReqValues tvs) = Fragment.toList $ unTag tvs

-- instance ToList CompReqValues Text where
--   toList (CompReqValues tvs) = Fragment.toList $ unTag tvs

-- instance ToList CompReqValues Int where
--   toList (CompReqValues tvs) = Fragment.toList $ unTag tvs

instance ToList CompReqValues Span where
  toList (CompReqValues tvs) = Fragment.toList $ unTag tvs

instance ToJSON CompReqValues

-- |
-- Used to instantiate a request in-process (not data)
--
toTupleCompReqValues :: CompReqValues -> (ValuesReqEnum, Reduced)
toTupleCompReqValues (CompReqValues (Red vs)) = (vs, True)
toTupleCompReqValues (CompReqValues (Exp vs)) = (vs, False)

-- |
-- Creates a list of singletons (list of list n = 1). Utilized to expand
-- a request with the 'Model.ETL.TagRedExp.Exp'.
--
toCompValuesList :: CompReqValues -> [CompValues]
toCompValuesList = Values.toCompValuesList . toCompValues

-- |
--
fromCompValues :: Reduced -> AntiRequest -> CompValues -> CompReqValues
fromCompValues reduced antiRequest vs
  | reduced && antiRequest         = CompReqValues . Red $ toExcludeRequest vs
  | reduced && not antiRequest     = CompReqValues . Red $ toIncludeRequest vs
  | not reduced && antiRequest     = CompReqValues . Exp $ toExcludeRequest vs
  | not reduced && not antiRequest = CompReqValues . Exp $ toIncludeRequest vs
  | otherwise                      = panic "Unreachable - fromCompValues"

-- |
--
instance ToCompValues CompReqValues where
  -- toCompValues :: a -> CompValues
  toCompValues = fst . unwrapReqEnum . unTag . coerce

-- | Synonym used to set TagRedExp value
type Reduced = Bool

-- | Synonym used to set AntiRequestEnum value
type AntiRequest = Bool


---------------------------------------------------------------------------------
instance Fragment (SearchFragment ReqQualities 'ETL) where
  null = (null @ReqQualities) . coerce
  len  = (len @ReqQualities) . coerce
instance Fragment (SearchFragment ReqQualities 'ETLSubset) where
  null = (null @ReqQualities) . coerce
  len  = (len @ReqQualities) . coerce

instance Fragment (SearchFragment ReqComponents 'ETLSubset) where
  null = (null @ReqComponents) . coerce
  len  = (len @ReqComponents) . coerce
-- length . Model.reqComponents . values <$> subsetResults
--
instance Fragment (SearchFragment CompReqValues 'Req) where
  null = (null @CompReqValues) . coerce
  len  = (len @CompReqValues) . coerce
instance Fragment (SearchFragment CompReqValues 'ETLSubset) where
  null = (null @CompReqValues) . coerce
  len  = (len @CompReqValues) . coerce

---------------------------------------------------------------------------------
-- ** Request FieldCount
-- |
--
-- ==== Contribution to the field count
--
-- Combined counts from each branch of the request.
--
instance FieldCount (Request 'Success) where
  fieldCount Request {..} = fieldCount subReq + fieldCount meaReqs

-- ***  Subject arm
-- |
-- ==== Contribution to the field count
-- Subject (idx) plus number of qualities included in the request.
--
instance FieldCount QualityMix where
  fieldCount QualityMix {..} = 1 + len qualityMix

-- ***  Measurement arm
-- ****  ComponentMixes
-- |
-- ==== Contribution to the field count
--
-- * Sum of the collection of field series specified in each of the ReqComponents
--
instance FieldCount ComponentMixes where
  fieldCount (ComponentMixes vs) = sum (foldr (:) [] (fieldCount <$> vs))

-- |
-- Recall, that a CompMix represents the measurement itself.  The "cuts"
-- are captured in the field name, not a field /per se/.
-- Thus, field count = number of mixes.
--
instance FieldCounts ComponentMixes where
  fieldCounts (ComponentMixes coll) = foldr (:) [] (mapWithKey tup coll)
    where tup k v = (k, fieldCount v)

-- |
-- Nothing means we display a single summary measurement field
--
instance FieldCount (Maybe ReqComponents) where
  fieldCount Nothing  = 1
  fieldCount (Just v) = fieldCount v

-- |
-- Nothing means generate a series with all values in the Component
--
instance FieldCount (Maybe CompReqValues) where
  fieldCount Nothing  = 0
  fieldCount (Just v) = fieldCount v

-- *** ReqComponents
-- |
-- === Contribution to the field count
--
-- For a given series is the combination of values from each component in the
-- collection of components. The number of combinations is the product of
-- the number of values in each component.
--
-- ==== Adjustments
--
-- * Component of len zero does not impact the field count.
--
-- * When all components retun a zero length result, the contribution to the
-- field count is zero.
--
instance FieldCount ReqComponents where
  fieldCount (ReqComponents vs) = product' . filter (/= 0) $ foldr
    (:)
    []
    fieldCounts'
   where
    fieldCounts' = fieldCount <$> vs
    product' [] = 0
    product' xs = product xs

-- **** CompReqValues
-- |
-- ==== Contribution to the field count
--
-- * Reduced is a single summary field when the request is valid
-- * Expressed = number of values in the request result
--
-- Pull whether field values are span values into a _request_ context
-- (the only way to differentiate from ETL).
--
-- ⬜ make sure the first point makes sense.
--
instance FieldCount CompReqValues where
  fieldCount vs
    | areSpanValues vs
    =
      --
      -- Note how the external, redundant, TagRedExp is ignored (Span values)
      -- 🚧 ⬜ Double check logic of concluding is redundant
      --
      let getSpanValues' CompReqValues { values } =
              getSpanValuesReqEnum $ unTag values
          spanList = fromJust (getSpanValues' vs)
                        -- Add the field count from the shared component (SpanType)
      in  sum $ fieldCount <$> spanList
    |

      --
      -- the result depends on the TagRedExp and the number of values
      -- ⚠️  The number of values depends on AntiRequestEnum value
      --
      otherwise
    = case (len vs, isRed vs) of
      (0, _    ) -> 0
      (_, True ) -> 1
      (l, False) -> l





--
---------------------------------------------------------------------------------
