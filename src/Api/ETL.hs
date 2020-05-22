{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module     : Api.ETL
-- Description: Search the 'Model.ETL.ObsETL' data
--
-- == Overview
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
module Api.ETL
  where
---------------------------------------------------------------------------------
import           Protolude              hiding (Type, null)
---------------------------------------------------------------------------------
import           Data.Coerce
---------------------------------------------------------------------------------
import           Control.Exception.Safe
import           Control.Monad.Logger
---------------------------------------------------------------------------------
import           Model.ETL.Fragment
import           Model.Search
---------------------------------------------------------------------------------
import           Model.ETL.ObsETL       hiding (request)
import           Model.Request          (CompReqValues (..),
                                         toTupleCompReqValues)
---------------------------------------------------------------------------------
-- |
--
getSubjectType :: ObsETL -> SubKey
getSubjectType = subType . obsSubject

-- |
-- Retrieve a reference to the Measurements collection.
-- The collection is one of two branches in the Obs object.
--
lookupMeasurements :: MonadLogger m
                   => ObsETL -> m Measurements
lookupMeasurements o = do
  let result = obsMeasurements o
  logDebugN $ "lookupMeasurements: "
            <> " count: " <> show (len result)
  pure result

-- ** Request
-- |
-- Unifying filter for CompReqValues
-- In the event no values are provided, the filter delivers all values
-- with the Exp tag.
--
requestCompReqValues :: (MonadLogger m, MonadThrow m)
                    => CompReqValues -> SearchFragment CompValues 'ETL
                    -> m (SearchFragment CompReqValues 'ETLSubset)
requestCompReqValues search values

  | null search = do
      logWarnN "Ran a search with a null value search => series with all values"
      requestCompReqValues (coerce (fromFieldCompReqValues False (coerce values))) values
      -- expressed = False -- this is confusing; False encodes Expressed

  | otherwise   = do
      let (search', reduced) = toTupleCompReqValues search
      result <- request (toFragmentReq search') values
      logRequest "requestCompReqValues" search values result
      pure $ fromFieldCompReqValues reduced result

-- ** requestValues
-- |
-- Unifying filter for FieldValues. The null search returns a null set.
--
requestValues, requestQualReqValues :: (MonadLogger m, MonadThrow m)
  => FieldValues -> SearchFragment FieldValues 'ETL
  -> m (SearchFragment FieldValues 'ETLSubset)

requestValues search values = do
  result <- request (toFragmentReq search) values
  logRequest "requestValues" search values result
  pure result

-- |
-- Unlike 'requestCompReqValues' we do not augment a null search request.
--
requestQualReqValues = requestValues

logRequest :: (MonadLogger m, Show a, Show b, Show c)
           => Text -> a -> b -> c -> m ()
logRequest heading search values result = do
  logDebugN logDivide
  logDebugN $ "ETL - " <> heading
  logDebugN $ "search: " <> show search
  logDebugN $ "values: " <> show values
  logDebugN $ "result: " <> show result
  logDebugN logDivide

logDivide :: Text
logDivide = "-------------------------------------------------------------------"


---------------------------------------------------------------------------------
