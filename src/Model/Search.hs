{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Model.Search
-- Description : Formalizes search computation
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
--
-- = Overview
-- This is a critical module for running the 'Api.GQL.Input.Request' module.
-- This module provides the interface for running searches while tracking
-- the source of the data to ensure efficient subsetting of the
-- 'Model.ETL.ObsETL' hosted data.
--
--
module Model.Search
  where
-------------------------------------------------------------------------------
import           Protolude              hiding (null)
-------------------------------------------------------------------------------
import           Data.Aeson             (ToJSON)
-------------------------------------------------------------------------------
import           Data.Coerce
import qualified Data.Map.Strict        as Map (fromList)
import qualified Data.Set               as Set (intersection)
import           Data.Text              (pack)
-------------------------------------------------------------------------------
import           Control.Exception.Safe
import           ObsExceptions
-------------------------------------------------------------------------------
import           Model.ETL.Components   hiding (getValues, len, null)
import qualified Model.ETL.Components   as Comps (getValues)
import           Model.ETL.FieldValues
import           Model.ETL.Key
import           Model.ETL.Qualities    hiding (getValues, len, null)
import qualified Model.ETL.Qualities    as Quals (getValues)
import qualified Model.ETL.Span         as Span (request)
import           Model.ETL.TagRedExp
-------------------------------------------------------------------------------
import           Model.Request          hiding (minQualityMix)
import qualified Model.Request          as Model (minQualityMix)
-------------------------------------------------------------------------------
import           Model.ETL.Fragment
-------------------------------------------------------------------------------
  --
-- |
-- ** Overview
--
-- A phantom type used to track data fragments used to compute a user request
-- for ETL data.
--
--
data Source
     = ETL        -- from Mode.ETL -> FieldValues
     | Req        -- from GqlInput -> FieldValues
     | ETLSubset  -- from FieldValues -> FieldValues -> FieldValues
     deriving (Generic)

-- |
-- Highest level type synonyms for the request
--
type MeaETLSubset     = SearchFragment ReqComponents 'ETLSubset
-- type SubjectETLSubset = SearchFragment ReqQualities  'ETLSubset

-- |
-- The host for the state required to enforce the subsetting constraints.
--
-- Early design: likely limited to 'Model.ETL.FieldValues'.
--
newtype SearchFragment a (source :: Source)
  = SearchFragment { fragment :: a } deriving (Show, Eq, Generic)

-- |
-- Permitted when the types (and sources) match.
--
instance Semigroup a => Semigroup (SearchFragment a b) where
  SearchFragment a1 <> SearchFragment a2 = SearchFragment $ a1 <> a2

instance ToJSON a => ToJSON (SearchFragment a b)

instance Fragment (SearchFragment FieldValues 'ETL) where
  null = (null @FieldValues) . coerce
  len  = (len  @FieldValues) . coerce
instance Fragment (SearchFragment FieldValues 'Req) where
  null = (null @FieldValues) . coerce
  len  = (len  @FieldValues) . coerce
instance Fragment (SearchFragment FieldValues 'ETLSubset) where
  null = (null @FieldValues) . coerce
  len  = (len  @FieldValues) . coerce

instance Fragment (SearchFragment ReqQualities 'ETL) where
  null = (null @ReqQualities) . coerce
  len  = (len  @ReqQualities) . coerce
instance Fragment (SearchFragment ReqQualities 'ETLSubset) where
  null = (null @ReqQualities) . coerce
  len  = (len  @ReqQualities) . coerce

instance Fragment (SearchFragment ReqComponents 'ETLSubset) where
  null = (null @ReqComponents) . coerce
  len  = (len  @ReqComponents) . coerce
-- length . Model.reqComponents . values <$> subsetResults
--
instance Fragment (SearchFragment CompReqValues 'Req) where
  null = (null @CompReqValues) . coerce
  len  = (len  @CompReqValues) . coerce
instance Fragment (SearchFragment CompReqValues 'ETLSubset) where
  null = (null @CompReqValues) . coerce
  len  = (len  @CompReqValues) . coerce

-- |
-- Restrict how to extract a Search result
-- SearchFragment 'Req does **not** implement this class.
--
class ToSearchResult a b where
  toSearchResult  :: SearchFragment a b -> a
  toSearchResultM :: Maybe (SearchFragment a b) -> Maybe a

instance ToSearchResult ReqQualities 'ETL where
  toSearchResult = coerce
  toSearchResultM = coerce

instance ToSearchResult ReqQualities 'ETLSubset where
  toSearchResult = coerce
  toSearchResultM = coerce

instance ToSearchResult CompReqValues 'ETLSubset where
  toSearchResult = coerce
  toSearchResultM = coerce

instance ToSearchResult CompValues 'ETL where
  toSearchResult = coerce
  toSearchResultM = coerce

instance ToSearchResult CompValues 'ETLSubset where
  toSearchResult = coerce
  toSearchResultM = coerce

instance ToSearchResult ReqComponents 'ETLSubset where
  toSearchResult = coerce
  toSearchResultM = coerce

-- |
-- Host for FieldValues 'Req
--
class ToFragmentReq a where
  toFragmentReq :: a -> SearchFragment a 'Req

instance ToFragmentReq FieldValues where
  toFragmentReq = SearchFragment

instance ToFragmentReq CompReqValues where
  toFragmentReq = SearchFragment

-- |
-- Host for Fieldvalues 'ETL
--
class ToFragmentETL a where
  toFragmentETL :: a -> SearchFragment a 'ETL

instance ToFragmentETL FieldValues where
  toFragmentETL = SearchFragment

-- |
-- The return value depends on this Search typeclass module
--
instance GetEtlFragment Components CompKey (SearchFragment CompValues 'ETL) where
  getValues components k
    = coerce <$> Comps.getValues k components

-- |
-- The return value depends on this Search typeclass module
--
instance GetEtlFragment Qualities QualKey (SearchFragment QualValues 'ETL) where
  getValues qualities k
    = coerce <$> Quals.getValues k qualities

-- ** Module benefit - structured subset function
-- |
-- The client can only ever request what is a subset of what is available in the
-- ETL data.
--
-- This function enforces how to determine a subset relation between
-- sets of FieldValues.
--
-- This is critical for  'Model.ETL.FieldValues' hosting 'Model.ETL.Span'
-- values.
--
-- Otherwise, this capacity augments the capcity to account during the request
-- process.
--
-- TODO: Review the concept here with 'Model.ETL.Fragment.filterF'
--
-- /Note/: SearchFragment cannot implement Ord because the computation
-- involves two types.
--
request :: MonadThrow m
        => SearchFragment FieldValues 'Req -> SearchFragment FieldValues 'ETL
        -> m (SearchFragment FieldValues 'ETLSubset)

request req etl
  | isSubsetOf req etl = pure $ coerce req
  | otherwise          = intersection req etl

isSubsetOf :: SearchFragment FieldValues 'Req
           -> SearchFragment FieldValues 'ETL -> Bool
isSubsetOf (SearchFragment search) (SearchFragment etl) = search <= etl

intersection :: (MonadThrow m)
             => SearchFragment FieldValues 'Req -> SearchFragment FieldValues 'ETL
             -> m (SearchFragment FieldValues 'ETLSubset)

intersection (SearchFragment (TxtSet s))
             (SearchFragment (TxtSet vs))  = pure . SearchFragment . TxtSet
                                           $ Set.intersection s vs
intersection (SearchFragment (IntSet s))
             (SearchFragment (IntSet vs))  = pure . SearchFragment . IntSet
                                           $ Set.intersection s vs
intersection (SearchFragment (SpanSet s))
             (SearchFragment (SpanSet vs)) = pure . SearchFragment . SpanSet
                                           $ Span.request s vs

intersection s _ = throw $ TypeException (Just . pack $ show s)


-- |
--
fromFieldCompReqValues :: Reduced
                       -> SearchFragment CompValues    'ETLSubset
                       -> SearchFragment CompReqValues 'ETLSubset
fromFieldCompReqValues red vs
  | red       = SearchFragment $ CompReqValues { values = Red (coerce vs) }
  | otherwise = SearchFragment $ CompReqValues { values = Exp (coerce vs) }

type Reduced = Bool

---------------------------------------------------------------------------------
-- ** Ad-hoc support function
-- *** Conversion
-- |
--
toCompReqValues :: SearchFragment CompValues 'ETL
                -> SearchFragment CompReqValues 'ETLSubset
toCompReqValues (SearchFragment vs) = coerce . CompReqValues $ Exp vs

-- *** Constructors
-- **** Measurement related
-- ***** Fullset request.
-- |
--
-- Gateway to generating output from a user request.  Limits the the
-- constructor using the 'ETLSubset source
--
-- Augment the CompKey only request with the 'Exp' tag.
--
fromListExpComponents :: [(CompKey, SearchFragment CompValues 'ETL)]
                      -> SearchFragment ReqComponents 'ETLSubset
fromListExpComponents vs = coerce . Map.fromList
                         $ fmap (Just . CompReqValues . Exp . coerce) <$> vs

-- ***** Subset request.
-- |
-- Subset request. A mix of 'Exp' and or 'Red' computations
-- ('Model.ETL.TagRedExp')
--
fromListReqComponents :: [(CompKey, Maybe (SearchFragment CompReqValues 'ETLSubset))]
                      -> SearchFragment ReqComponents 'ETLSubset
fromListReqComponents = coerce . Map.fromList

-- **** Measurement Request
-- |
-- This is the exit from the source type scope for a @Measurement@.
--
fromListComponentMixes :: [(MeaKey, Maybe (SearchFragment ReqComponents 'ETLSubset))]
                       -> ComponentMixes
fromListComponentMixes = ComponentMixes . Map.fromList . coerce

-- **** Subject related
-- |
--
-- Gateway to generating output from a user request.  Limits the the
-- constructor using the 'ETLSubset source
--
fromListReqQualities :: [(QualKey, Maybe (SearchFragment QualValues 'ETLSubset))]
                     -> SearchFragment ReqQualities 'ETLSubset
fromListReqQualities vs = SearchFragment .  ReqQualities
                        $ Map.fromList (fmap coerce <$> vs)

-- **** Subject Request
-- |
-- This is the exit from the source type scope for a @Subject@.
--
mkQualityMix :: SubKey
             -> SearchFragment ReqQualities 'ETLSubset
             -> QualityMix
mkQualityMix key@(SubKey _) vs = QualityMix key (Just $ coerce vs)
mkQualityMix _              _  = panic "mkQualityMix: Tried with wrong type."

-- |
-- Minimum viable request result for the subject.  It is possible to generate
-- because there is only one 'Model.ObsETL.Subject'.
--
minQualityMix, minSubResult :: SubKey -> QualityMix
minQualityMix = Model.minQualityMix
minSubResult = minQualityMix



---------------------------------------------------------------------------------
  --
