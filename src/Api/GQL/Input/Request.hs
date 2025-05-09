{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Api.GQL.RequestInput
-- Description : Hosts the user input for Request
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- == Introduction
--
-- This module implements the resolvers for a 'Request'. The GQL inputs and
-- views are specified in @schemas.request.graphql@ and what is shared
-- with the 'Api.GQL.ObsETL' module.
--
--
-- == Design goals
--
-- * Minimize how often field values are required to specify a request.
-- Consequence, avoid displaying field values (levels) when they are not
-- impacting the request (e.g., a request for all levels of a subject quality
-- means the same as not specifying any levels; so do not display the levels
-- in each instance accordingly).
--
-- * Reduce the syntax required to make a request. For instance, infer
-- meaning from requests that may or may not specify the field values.
--
-- == Keep in mind
--
-- The 'Model.Request' is a host for requested data; data fetched from 'Model.ETL'
-- stored in the @Database@ 'AppTypes.Env'.
--
-- === Line between a Request and ETL data
--
-- The design tries to leverage the 'Model.ObsETL' data types where
-- possible.  However, in order to maintain a clear distinction between request
-- and the ETL source of truth, the design likely would have been better
-- had all request inputs be tagged (one way or another) with a 'Request' moniker.
--
-- Notwithstanding, a request is a series of search terms. The
-- @schemas.request.graphql@ define the __input__ object specifications
-- accordingly. However, a 'Request' is not data unto itself. This line
-- is blurred by the fact that 'Model.Request' is instantiated using
-- 'Model.ObsETL' data and viewed using the data __types__ described
-- in @schemas.request.graphql@.
--
-- === Reduced versus Expressed data
--
-- Reduced is a summary version whilst Expressed is a expanded version of
-- a computation.  The use of 'Model.ETL.TagRedExp.TagRedExp' is used in two ways.
--
-- * First for Span both in the ETL and Request phases.
-- * Second, to qualify the requests for Components
-- ('Model.Request.ReqComponents').
--
-- There is likely redundant use of the 'Model.ETL.TagRedExp.TagRedExp' for a
-- 'Model.ETL.Span.Span' included in a 'Request'.
--
-- === A request plays out differently on each arm
--
-- Finally, a request is a subset of the ETL data. However, the subsetting
-- information is used differently for each arm of the request:
--
-- * Subject arm: specifies a filter to reduce the number of records in the
-- matrix
--
--     * none specified     -> include the field but no filter on records
--     * all specified      -> /same as none specified/
--     * a subset specified -> select records with matching values
--
-- * Measurement arm: increases the number of fields to include in a series
--
--     * nothing specified -> a single summary field
--     * all levels specified -> a series of fields
--
module Api.GQL.Input.Request
  where
---------------------------------------------------------------------------------
import           Protolude                 hiding (null)
---------------------------------------------------------------------------------
import           Data.Maybe                (fromJust)
---------------------------------------------------------------------------------
import           Api.ETL
import           Model.ETL.Fragment
import           Model.Search
---------------------------------------------------------------------------------
import qualified Model.ETL.ObsETL          as Model (ObsETL (..))
import qualified Model.Request             as Model (ComponentMixes,
                                                     Request (..))
import           Model.Status
---------------------------------------------------------------------------------
import qualified Api.GQL.Schemas.Request   as GqlInput
import           WithAppContext
---------------------------------------------------------------------------------
import           Api.GQL.Input.MeaRequests (fetchSubsetComponentMix)
import           Api.GQL.Input.SubRequest  (fetchQualityMix)
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
reqIsSubset :: WithAppContext m => GqlInput.RequestInput -> Model.ObsETL -> m Bool
reqIsSubset GqlInput.RequestInput {subReq, meaReqs} etl
  = pure $ isSub && areMeas

    where
      isSub = case subReq of
                   Nothing -> True
                   Just x  -> isSubset x (Model.obsSubject etl)

      areMeas = case meaReqs of
                   Nothing -> True
                   Just xs -> all (== True)
                              $ flip isSubset (Model.obsMeasurements etl) <$> xs

---------------------------------------------------------------------------------
  --
-- * Request
-- |
--
-- > Request
-- >   { subReq  :: !QualityMix
-- >   , meaReqs :: !ComponentMixes
-- >   } deriving (Show, Eq)
--
--
fetchRequest :: WithAppContext m
             => GqlInput.RequestInput
             -> Model.ObsETL
             -> m (Maybe (Model.Request 'Inprocess))

fetchRequest req etl = do

  logInfoN  "Processing Input Request"

  case req of

    GqlInput.RequestInput maybeSubReq (Just meaReqs) -> do

      -- Subject arm
      -- Utilizes mkQualityMix from the Search module to "unlock" request
      ---------------------------------------------------------------------------
      -- augment the subReq result if required
      subResult <- fetchQualityMix maybeSubReq (Model.obsSubject etl)

      let key     = fst <$> subResult
      let mValues = snd =<< subResult
      let subResult' =   fromJust
                     $   mkQualityMix <$> key <*> mValues
                    <|> (Just . minSubResult $ getSubjectType etl)


      -- Measurements arm
      -- Utilizes fromListComponentMixes from the Search module to "unlock"
      ---------------------------------------------------------------------------
      meas <- Api.ETL.lookupMeasurements etl  -- (~ EtlFragment)

      let subsetReqs  = concat $ GqlInput.subsets  <$> meaReqs
      let fullsetReqs = concat $ GqlInput.fullsets <$> meaReqs

      logDebugN $ "Measurements subset requests:  " <> show (length subsetReqs)
      logDebugN $ "Measurements fullset requests: " <> show (length fullsetReqs)

      -- logDebugN ("--------------\n"::Text)
      -- logDebugF maybeSubReq
      -- logDebugN ("--------------\n"::Text)
      -- logDebugF meaReqs
      -- logDebugN ("--------------\n"::Text)


      -- subset
      ---------------------------------------------------------------------------
      subsetResults <- catMaybes
                       <$> traverse (`fetchSubsetComponentMix` meas) subsetReqs

      -- logDebugN ("--- Subset requests   -----------\n"::Text)
      -- logDebugF subsetReqs
      -- logDebugN ("--- Search result     -----------\n"::Text)
      -- logDebugF subsetResults
      -- logDebugN ("---------------------------------\n"::Text)


      -- fullset
      ---------------------------------------------------------------------------
      let results     = catMaybes $ getEtlFragment meas
                      <$> fullsetReqs  -- lift over a list of requests

      let fullsetResults = (\(meaKey, _) -> (meaKey, Nothing)) <$> results


      -- mea combined
      ---------------------------------------------------------------------------
      let meaResults = fromListComponentMixes
                     $ subsetResults <> fullsetResults

      ---------------------------------------------------------------------------
        -- Request instantiation
        --
      if null meaResults

         then do
                logErrorN $ "The measurements requested returned null."
                          <> "\n" <> show meaReqs
                pure Nothing
         else

            (\m -> Just $ Model.Request
                   { subReq  = subResult'
                   , meaReqs = meaResults :: Model.ComponentMixes
                   , meta    = m
                   }) <$> meta' req etl


    -- I can't think of a min return value when nothing is specified
    -- in the measurements arm of the request.
    GqlInput.RequestInput _ Nothing -> do
      logWarnN "No measurements specified in the request; request cancelled."
      pure Nothing

-- |
-- Partial implementation of reporting on whether the request
-- was a subset of the ETL data.
--
meta' :: WithAppContext m
      => GqlInput.RequestInput -> Model.ObsETL
      -> m [(Text, Text)]

meta' req etl = do
          subset <- reqIsSubset req etl
          logDebugN $ "Request input subset of etl? " <> show subset
          pure [("subset"::Text, show subset)]


---------------------------------------------------------------------------------
