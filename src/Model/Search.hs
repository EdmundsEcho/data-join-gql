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
-- = Background
--
-- There are three versions of data
--
-- 1. ETL - succinct, non-redundant representation of data
-- 2. Request input - user specified, expects one result (a subset of ETL) for each request
-- 3. Request for matrix - non-redundant in the context of the matrix it
-- produces
--
module Model.Search
  ( module Model.Search
  , module Model.SearchFragment
  )
  where
-------------------------------------------------------------------------------
import           Protolude              hiding (null)
-------------------------------------------------------------------------------
import           Data.Coerce
import qualified Data.Map.Strict        as Map (fromList)
import           Data.Maybe             (fromJust)
-------------------------------------------------------------------------------
import           Control.Exception.Safe
import           Control.Monad.Logger
-------------------------------------------------------------------------------
import           Model.ETL.FieldValues
import           Model.ETL.Key
import qualified Model.ETL.Span         as Span (isExp)
import           Model.ETL.TagRedExp
-------------------------------------------------------------------------------
import           Model.Request          hiding (areSpanValues, minQualityMix)
import qualified Model.Request          as Model (minQualityMix)
-------------------------------------------------------------------------------
import           Model.ETL.Fragment     hiding (intersection)
import qualified Model.ETL.Fragment     as F (intersection)
import           Model.SearchFragment
-------------------------------------------------------------------------------


-- |
-- Highest level type synonyms for the request
--
type MeaETLSubset     = SearchFragment ReqComponents 'ETLSubset
-- type SubjectETLSubset = SearchFragment ReqQualities  'ETLSubset


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
request :: (MonadThrow m, MonadLogger m)
        => SearchFragment FieldValues 'Req -> SearchFragment FieldValues 'ETL
        -> m (SearchFragment FieldValues 'ETLSubset)

request req etl
  | isSubsetOf req etl = do
    let heading = " Search is a subset of Etl"
    let result = coerce req
    logRequest heading req etl result
    pure result

  | otherwise = do
    let heading = " Search is NOT a subset of Etl"
    let result = intersection req etl
    logRequest heading req etl result
    pure result

isSubsetOf :: SearchFragment FieldValues 'Req
           -> SearchFragment FieldValues 'ETL -> Bool
isSubsetOf (SearchFragment search) (SearchFragment etl) = search `subset` etl

intersection :: SearchFragment FieldValues 'Req
             -> SearchFragment FieldValues 'ETL
             -> SearchFragment FieldValues 'ETLSubset

intersection (SearchFragment s) (SearchFragment vs) =
  SearchFragment $ F.intersection s vs


-- |
-- Used to instantiate the fullfilled result of a search (data)
--
fromFieldCompReqValues :: Reduced
                       -> SearchFragment CompValues    'ETLSubset
                       -> SearchFragment CompReqValues 'ETLSubset
fromFieldCompReqValues red vs
  -- span values = Exp when there is more than one value || Range reduced = false
  | areSpanValues (coerce vs) =
       let spans = coerce vs
           redExp = if len spans == 1 && ( not
                                       . Span.isExp -- Bool
                                       . fromJust   -- Span
                                       . head       -- Maybe Span
                                       . fromJust   -- [span]
                                       $ getSpanValues spans )
                       then Red else Exp
             in SearchFragment $ CompReqValues { values = redExp (coerce vs) }
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
logRequest :: (MonadLogger m, Show a, Show b, Show c)
           => Text -> a -> b -> c -> m ()
logRequest heading search values result = do
  logDebugN logDivide
  logDebugN $ "Search.hs - " <> heading
  logDebugN $ "search:\n" <> show search
  logDebugN $ "values:\n" <> show values
  logDebugN $ "result:\n" <> show result
  logDebugN logDivide

logDivide :: Text
logDivide = "-------------------------------------------------------------------"
---------------------------------------------------------------------------------
  --
