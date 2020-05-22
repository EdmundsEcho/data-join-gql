{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PatternSynonyms       #-}

-- |
-- Module     : Model.Request
-- Description: Specified by the user in the "workbench"
--
-- = Overview
--
--   The @Request@ is instantiated from @ObsETL@, user input and a validation function.
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
  , QualityMix (..)
  , ReqQualities (..)
  , mkQualityMix
  , toListReqQualities
  , minQualityMix
  , minSubResult

  -- ** Component-related
  , ComponentMixes (..)
  , toListComponentMixes
  , ReqComponents(..)
  , toListReqComponents
  , fromComponents
  , fromCompValues

  -- ** FieldValues-related
  , CompReqValues (..)
  , mkSpan
  , toTupleCompReqValues
  , ToQualValues(..)
  , areSpanValues
  , isRed

  ) where
---------------------------------------------------------------------------------
import           Protolude             hiding (null)
---------------------------------------------------------------------------------
import           Data.Coerce
import           Data.Maybe            (fromJust)
---------------------------------------------------------------------------------
import           Data.Aeson            (ToJSON)
---------------------------------------------------------------------------------
import           Data.Map.Strict       (mapWithKey, union)
import qualified Data.Map.Strict       as Map (fromList, null, size, toList)
---------------------------------------------------------------------------------
import           Model.ETL.Components  hiding (len, null)
import           Model.ETL.FieldValues hiding (areSpanValues)
import qualified Model.ETL.FieldValues as Values (areSpanValues)
import           Model.ETL.Fragment
import           Model.ETL.Key
import           Model.ETL.TagRedExp   hiding (isRed)
import qualified Model.ETL.TagRedExp   as Tag (isRed)
import           Model.Status
---------------------------------------------------------------------------------

-- * Request

-- | Request is a subset of the ObsETL
-- @ ~validate :: RequestInput -fetch-> Request @
--
data Request (status::Status) = Request
  { subReq  :: !QualityMix
  , meaReqs :: !ComponentMixes
  -- , status  :: Proxy 'Inprocess
  } deriving (Show, Eq, Generic)

instance ToJSON (Request status)

-- |
-- *** Placeholder
--
-- **** Ideas for subsequent use
--
-- 1. Validating a structure where changes can only be made by the user
--     * Including at least one 'Model.ETL.Quality' in the request
--     * Ensuring the required, and single, 'Model.ETL.Span' component for
--       every @Measurement@ in the 'Model.Request.ComponentMixes' fragment
--       of the request
-- 1. Enforcing a non-redundant structure. For instance:
--     * Consolidating Subject requests
--     * Consolidating each mix in 'Model.Request.ComponentMixes'
--     * Including a default 'Model.ETL.Span' request value
--     * Removing a so called-subset of values Eq to fullset
--
-- **** Considerations
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

---------------------------------------------------------------------------------
  -- Subject arm
---------------------------------------------------------------------------------
-- |
-- ** Branches

-- | @ map :: ETL values -> Requested values @
data QualityMix = QualityMix
  { subjectType :: !Key
  , qualityMix  :: !(Maybe ReqQualities)
  } deriving (Show, Eq, Generic)

instance ToJSON QualityMix

-- |
--
minQualityMix, minSubResult :: SubKey -> QualityMix
minQualityMix key
  = QualityMix { subjectType = key
               , qualityMix = Nothing
               }
minSubResult = minQualityMix

-- *** ReqQualities
-- |
-- Encoding
-- - QualKey Nothing => display the quality field, select all levels
-- - QualKey Just vs => display the quality field, select levels
--
-- /Note/: QualValues are FieldValues.
--
newtype ReqQualities = ReqQualities
        { reqQualities :: Map QualKey (Maybe QualValues)
        } deriving (Show, Eq, Ord, Generic)

instance Fragment ReqQualities where
  null (ReqQualities vs) = Map.null vs
  len  (ReqQualities vs) = Map.size vs

instance ToJSON ReqQualities

-- |
-- Limited to matching on key values with left bias
-- TODO: Prioritize bias using the value of Maybe
--
instance Semigroup ReqQualities where
  (ReqQualities a) <> (ReqQualities b) = ReqQualities $ union a b

-- | Left bias
instance Monoid ReqQualities where
  mempty = ReqQualities mempty
  (ReqQualities a) `mappend` (ReqQualities b) = ReqQualities $ union a b

-- |
-- Instantiation
--
mkQualityMix :: Key -> ReqQualities -> QualityMix
mkQualityMix key@(SubKey _) vs = QualityMix key (Just vs)
mkQualityMix _              _  = panic "mkQualityMix: Tried with wrong type."
{-# DEPRECATED mkQualityMix "Use mkQualityMix in the Search module" #-}

-- |
-- Utilized by 'Api.GQL.RequestView'
--
-- /Notes/
-- * mkQualityMix is defined in the 'Model.Search' module.
-- * TODO: Consider changing the input type to @'ETLSubset@
--
--
--
toListReqQualities :: ReqQualities -> [(QualKey, Maybe QualValues)]
toListReqQualities (ReqQualities vs) = Map.toList vs


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
  len  (ComponentMixes vs) = Map.size vs

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
-- - CompKey Nothing => create a series of fields using all levels (Exp)
-- - CompKey Just vs =>
--     - Exp: create a series using the levels specified
--     - Red: create a single summary field using the levels specified
--
-- /Note/: @CompReqValues@ are @FieldValues@ tagged using @TagRedExp@.
--
newtype ReqComponents = ReqComponents
        { reqComponents :: Map CompKey (Maybe CompReqValues)
        } deriving (Show, Eq, Ord, Generic)


instance Fragment ReqComponents where
  null (ReqComponents vs) = Map.null vs
  len  (ReqComponents vs) = Map.size vs

instance ToJSON ReqComponents

instance Semigroup ReqComponents where
  (ReqComponents a) <> (ReqComponents b) = ReqComponents $ union a b

instance Monoid ReqComponents where
  mempty = ReqComponents mempty
  mappend (ReqComponents a) (ReqComponents b) = ReqComponents $ union a b

-- |
-- Constructor
fromListReqComponents :: [(CompKey, Maybe CompReqValues)] -> ReqComponents
fromListReqComponents = ReqComponents . Map.fromList
{-# DEPRECATED fromListReqComponents "See Search module" #-}

-- |
-- toList
toListReqComponents :: ReqComponents -> [(CompKey, Maybe CompReqValues)]
toListReqComponents (ReqComponents vs) = Map.toList vs

-- |
-- Api helper
-- TODO: Use coerce
--
fromComponents :: (CompValues -> TagRedExp CompValues)
               -> Components -> ReqComponents
-- unwrap CompValues
-- wrap with Tag
-- how access CompValues inside Components? fmap
fromComponents redExp o =
  ReqComponents .  fmap (Just . CompReqValues . redExp) $ components o

-- | Wrapper to express Reduced vs Expressed request computation
--   of the associated measurement value. So, just the same FieldValues
--   plus an extra tag.  This fits throughout further down the Request tree.
newtype CompReqValues = CompReqValues { values :: TagRedExp CompValues }
  deriving (Show, Eq, Ord, Generic)

-- |
--
isRed :: CompReqValues -> Bool
isRed CompReqValues {values} = Tag.isRed values

-- |
--
areSpanValues :: CompReqValues -> Bool
areSpanValues (CompReqValues vs) = Values.areSpanValues (unTag vs)

-- |
--
instance Fragment CompReqValues where
  null (CompReqValues (Red vs)) = null vs
  null (CompReqValues (Exp vs)) = null vs
  len  (CompReqValues (Red vs)) = len vs
  len  (CompReqValues (Exp vs)) = len vs

instance ToJSON CompReqValues

-- |
--
toTupleCompReqValues :: CompReqValues -> (CompValues, Reduced)
toTupleCompReqValues (CompReqValues (Red vs)) = (vs, True)
toTupleCompReqValues (CompReqValues (Exp vs)) = (vs, False)

-- |
--
fromCompValues :: (CompValues -> TagRedExp CompValues)
               -> CompValues -> CompReqValues
fromCompValues redExp = CompReqValues . redExp


-- | Synonym used to set TagRedExp value
type Reduced = Bool


---------------------------------------------------------------------------------
-- ** Request FieldCount
-- |
--
-- ==== Contribution to the field count
--
-- Combined counts from each branch of the request.
--
instance FieldCount (Request 'Success) where
  fieldCount Request {..} = fieldCount subReq
                         + fieldCount meaReqs

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
-- Combination of
--
-- * Sum of the collection of field series specified in each of the ReqComponents
-- * The number of measurements requested
--
instance FieldCount ComponentMixes where
  fieldCount mix@(ComponentMixes vs)
    = sum (foldr (:) [] (fieldCount <$> vs))
    + len mix

-- |
--
instance FieldCounts ComponentMixes where
  fieldCounts (ComponentMixes coll)
    = foldr (:) [] (mapWithKey tup coll)
      where tup k v = (k, fieldCount v + 1)

-- **** ReqComponents
-- |
-- ==== Contribution to the field count
--
-- For a given series is the combination of values from each component in the
-- collection of components. The number of combinations is the product of
-- the number of values in each component.
--
-- ===== Adjustments
--
-- * Component of len zero does not impact the field count.
--
-- * When all components retun a zero length result, the contribution to the
-- field count is zero.
--
instance FieldCount ReqComponents where
  fieldCount (ReqComponents vs)
    = product'
    . filter (/= 0)
    $ foldr (:) [] fieldCounts'
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
-- * Expressed = number of values in the request result
--
-- /Note/: Pull whether field values are span values into a *request* context
-- (the only way to differentiate from ETL).
--
-- TODO: make sure the first point makes sense.
--
instance FieldCount CompReqValues where
  fieldCount vs
    | areSpanValues vs =

      -- Note how the external, redundant, TagRedExp is ignored (Span values)
      let getSpanValues' CompReqValues {values} = getSpanValues $ unTag values
          spanList = fromJust (getSpanValues' vs)
      -- Add the field count from the shared component (SpanType)
       in sum $ fieldCount <$> spanList

      -- the result depends on the TagRedExp and number of values
    | otherwise        = case (len vs, isRed vs) of
                               (0, _)     ->  0
                               (_, True)  ->  1
                               (l, False) ->  l





--
---------------------------------------------------------------------------------
