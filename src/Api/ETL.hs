{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module     : Api.ETL
-- Description: Search the 'Model.ETL.ObsETL' data
--
-- == Overview
--
-- This module uses "internal" types to process a request.
--
--     @Model Request -> Model -> Model Request@
--     @Model Request -> SearchFragment 'ETL -> SearchFragment 'ETLSubset@
--
-- Much of the computations required to access the 'Model.ETL.ObsETL' data
-- is defined in the 'Model.ETL.Fragment' typeclass module.
--
-- == Description of the data
--
-- > There are five collections in the Obs data structure:
-- > 1. Qualities     parent :: Subject       Key :: Key (QualKey)
-- > 2. QualValues    parent :: Quality       NA
-- > 3. Measurements  parent :: Obs           Key :: Key (MeaKey)
-- > 4. Components    parent :: Measurement   Key :: Key (CompKey)
-- > 5. CompValues    parent :: Component     NA
--
-- QualValues :: FieldValues (wrap Text | Int)
-- CompValues :: FieldValues (wrap Text | Int | Span)
--
-- Note: The range of QualValue Types is not enforced by the Haskell Type system.
--
-- Retrieve a reference to the one Subject associated with the Obs collection.
-- Not strictly a collection but is one of two branches in the Obs object.
-- The Subject node is ~ Measurements node
--
--
module Api.ETL where
---------------------------------------------------------------------------------
import           Protolude               hiding ( Type
                                                , null
                                                , isPrefixOf
                                                )
---------------------------------------------------------------------------------
import           Data.Coerce
import           Data.Text                      ( count
                                                , isPrefixOf
                                                , isSuffixOf
                                                )
import qualified Data.Set                      as Set
                                                ( filter )
---------------------------------------------------------------------------------
import           Model.ETL.Fragment
import           Model.Search            hiding ( logDivide
                                                , logRequest
                                                , isSubstring
                                                )
---------------------------------------------------------------------------------
import           Model.ETL.ObsETL        hiding ( null )
import qualified Model.ETL.Components          as Components
                                                ( lookup )
import qualified Model.ETL.Qualities           as Qualities
                                                ( lookup )
import           Model.Request                  ( CompReqValues(..)
                                                , toTupleCompReqValues
                                                , fromCompValues
                                                )
---------------------------------------------------------------------------------
import           WithAppContext
---------------------------------------------------------------------------------
-- |
--
getSubjectType :: ObsETL -> SubKey
getSubjectType = subType . obsSubject

-- |
-- Retrieve a reference to the Measurements collection.
-- The collection is one of two branches in the Obs object.
--
lookupMeasurements :: MonadLogger m => ObsETL -> m Measurements
lookupMeasurements o = do
  let result = obsMeasurements o
  logDebugN "ETL data"
  logDebugN $ "lookupMeasurements found measurements: " <> show
    (meaTypes result)
  pure result

-- ** Request
-- |
-- 🧮 Unifying filter for CompReqValues
--
-- In the event no values are provided, the filter delivers all values
-- with the Exp tag.
--
--
requestCompReqValues
  :: (MonadLogger m, MonadThrow m)
  => CompReqValues
  -> SearchFragment CompValues 'ETL
  -> m (SearchFragment CompReqValues 'ETLSubset)

requestCompReqValues search values
  | -- empty request is always Include vs Exclude
    null search = do
      logDebugN "ETL data"
      logWarnN "Ran a search with a null value search 👉 Reduce with all values"
      logWarnN "⚠️  This request state should be avoided (deselect all instead)."
      requestCompReqValues
        (fromCompValues True False (coerce values))
        values
  |
      -- expressed = False -- this is confusing; False encodes Expressed
    otherwise = do
       let (search', reduced) = toTupleCompReqValues search
       let (search'', antiRequest) = unwrapReqEnum search'
       let exclude = case antiRequest of Exclude -> True; _ -> False
       result <- request (toFragmentReq search'') values
       logDebugN "ETL data"
       logRequest "requestCompReqValues" search values result
       pure . coerce $ fromCompValues reduced exclude (coerce result)

-- ** requestValues
-- |
-- Unifying filter for FieldValues. The null search returns a null set.
-- 🦀 The null [] request signals we want the field in the select statement
--    ... the where clause is null (no filter, no limit filter => max rows)
--
requestQualReqValues, requestValues
  :: (MonadLogger m, MonadThrow m)
  => FieldValues
  -> SearchFragment FieldValues 'ETL
  -> m (SearchFragment FieldValues 'ETLSubset)

requestQualReqValues search values = do
  logDebugN "ETL data"
  result <- request (toFragmentReq search) values
  logRequest "requestQualReqValues" search values result
  pure result

-- |
-- Unlike 'requestCompReqValues' we do not augment a null search request.
--
requestValues = requestQualReqValues

-- ** searchValues
-- |
-- Return values that contain a search term. Return all values when search
-- term is an empty string.
--
searchValues
  :: (MonadLogger m, MonadThrow m)
  => Text
  -> SearchFragment FieldValues 'ETL
  -> m (SearchFragment FieldValues 'ETLSubset)

searchValues search values = do
  logDebugN "ETL data"
  result <- selectWithTerm search values
  logRequest "searchValues" search values result
  return result

-- ** getQualityValues
-- |
--
getQualityValues :: Text -> ObsETL -> Maybe FieldValues
getQualityValues qualityName etl = do
  let subject :: Subject = obsSubject etl
  fieldValues :: QualValues <- Qualities.lookup (subQualities subject)
                                                (mkQualKey qualityName)
  return fieldValues

-- ** getComponentValues
-- |
--
getComponentValues :: Text -> Text -> ObsETL -> Maybe FieldValues
getComponentValues measurementType componentName etl = do
  let measurements :: Measurements = obsMeasurements etl
  (_, components) :: (MeaKey, Components) <- getValues measurements
                                                       measurementType
  fieldValues :: CompValues <- Components.lookup components
                                                 (mkCompKey componentName)
  return fieldValues

filterValues :: (Text -> Text -> Bool) -> Text -> FieldValues -> FieldValues
filterValues p searchTerm (TxtSet xs) = TxtSet $ Set.filter (p searchTerm) xs
filterValues _ _ _ = panic "Filter values does not work with non-text values"

isSubstringP :: Text -> Text -> Bool
isSubstringP ""        _       = True
isSubstringP filterTxt tryThis = case count filterTxt tryThis of
  0 -> False
  _ -> True

startsWithP :: Text -> Text -> Bool
startsWithP ""         _       = True
startsWithP startsWith tryThis = isPrefixOf startsWith tryThis

endsWithP :: Text -> Text -> Bool
endsWithP ""       _       = True
endsWithP endsWith tryThis = isSuffixOf endsWith tryThis


logRequest
  :: (MonadLogger m, ToJSON a, ToJSON b, ToJSON c)
  => Text
  -> a
  -> b
  -> c
  -> m ()
logRequest heading search values result = do
  logDebugN logDivide
  logDebugN $ ("ETL - " :: Text) <> heading
  logDebugN ("Search: " :: Text)
  logDebugF search
  logDebugN ("Values: " :: Text)
  logDebugF values
  logDebugN ("Result: " :: Text)
  logDebugF result
  logDebugN logDivide
  logDebugN logDivide

logDivide :: Text
logDivide =
  "-------------------------------------------------------------------"


---------------------------------------------------------------------------------
