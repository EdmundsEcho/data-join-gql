{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module     : Api.GQL.Input.SubRequest
-- Description: UI access point
--
-- This module implements the resolvers for a 'Request'. The GQL inputs and
-- views are specified in @schemas.request.graphql@ and what is shared
-- with the 'Api.GQL.ObsETL' module; see @schemas.shared.graphql@.
--
-- The 'Model.Reqest' is a host for requested data; data fetched from 'Model.ETL'
-- stored in the @Database@ 'AppTypes.Env'.
--
-- The design tries to leverage the 'Model.ObsETL' data types where
-- possible.  However, in order to maintain a clear distinction between request
-- and the ETL source of truth, the design likely would have been better
-- had all request inputs be tagged (one way or another) with a 'Request' moniker.
--
-- Goal: Minimize how often field values are required to specify a request.
--
-- Keep in mind,
-- a request is a series of search terms. The @schemas.request.graphql@ define
-- the input object specifications accordingly. However, a 'Request' is not
-- data unto themselves. This line is blurred by the fact that 'Model.Request'
-- is instantiated using 'Model.ObsETL' data. However, is viewed using the
-- data types described in @schemas.request.graphql@.
--
-- Also note, the use of 'Model.ETL.TagRedExp.TagRedExp' is used in two ways.
-- First for Span both in the ETL and Request phases.  Second, to qualify the
-- requests for Components ('Model.Request.ReqComponents'). There is likely
-- redundant use of the 'Model.ETL.TagRedExp.TagRedExp' for a
-- 'Model.ETL.Span.Span' included in a 'Request'.
--
module Api.GQL.Input.MeaRequests where
---------------------------------------------------------------------------------
import           Protolude               hiding (null)
---------------------------------------------------------------------------------
import           Data.Maybe              (fromJust)
---------------------------------------------------------------------------------
import           Control.Exception.Safe
import           Control.Monad.Logger
---------------------------------------------------------------------------------
import           Api.ETL
import qualified Api.GQL.ObsETL          as Shared
---------------------------------------------------------------------------------
import           Model.ETL.Fragment
import           Model.ETL.ObsETL        (mkCompKey, mkMeaKey)
import qualified Model.ETL.ObsETL        as Model (Components, MeaKey,
                                                   Measurements, TagRedExp (..))
import qualified Model.Request           as Model (CompReqValues (..),
                                                   ReqComponents (..),
                                                   fromListReqComponents)
---------------------------------------------------------------------------------
import qualified Api.GQL.Schemas.Request as GqlInput
import qualified Api.GQL.Schemas.Shared  as GqlInput
---------------------------------------------------------------------------------
  -- Measurements arm
---------------------------------------------------------------------------------
-- |
-- Only subset requests
--
-- measurementType + componentMix
--
-- > input SubsetCompMixReq {
-- >   meaKey: Text
-- >   compReqInput: [ComponentReqInput]
-- > }
--
-- > data ComponentMix {
-- >   meaType: Key
-- >   componentMix: ReqComponents
-- > }
--
-- Maybe encoding
-- Overall success: determined by the measurement key
-- Subset success:  determined by the components requested
--
-- Issue?: If we want a series, we have to create aliases for the request
-- because we are using the same meaType and compName over and over for
-- each output.  Recall, an output is a field in the matrix data table.
--
fetchSubsetComponentMix :: (MonadLogger m, MonadThrow m)
                        => GqlInput.SubsetCompMixReq -> Model.Measurements
                        -> m (Maybe (Model.MeaKey, Maybe Model.ReqComponents))

fetchSubsetComponentMix request etl = do

  let result = getEtlFragment etl mkMeaKey request

  case result of
    -- 1. did the key return a collection?
    -- No, return Nothing.
    Nothing -> do
      logWarnN $ ("Measurement key does not exist: " :: Text)
               <> show (fromJust $ GqlInput.requestKey request)
      pure Nothing

    -- Yes,
    -- run the next keyValues search
    Just keyValues -> do
    -- (MeaKey, Components)

       logInfoN "COMPMIX"

       let nextRequest = GqlInput.compReqInput request
       logInfoN $ "nextRequest" <> show nextRequest
       -- ComponentReqInput {componentNames = Nothing, componentName = Just "Payer" , componentValues = Nothing}
       -- ComponentReqInput {componentNames = Nothing, componentName = Just "RxType" , componentValues =
       -- Just (CompValuesReqInput {txtValues = Just ["Refills"], intValues = Nothing})}

       -- Subset requests
       let subsetReqs    = concat $ traverse GqlInput.subsets nextRequest
       logDebugN $ "mea subset requests: " <> show (length subsetReqs)
       logInfoN  $ "subsets...are they being captured?\n" <> show subsetReqs

       subsetResults     <- fetchSubsetComponents subsetReqs (snd keyValues)

       -- Fullset requests
       let fullsetReqs   = concat $ traverse GqlInput.fullsets nextRequest
       logDebugN $ "mea fullset requests: "  <> show (length fullsetReqs)

       let results       = catMaybes $ getEtlFragment (snd keyValues) mkCompKey
                         <$> fullsetReqs  -- lift over a list of requests
       logDebugN $ "mea results: "  <> show results

       let normalizedRes = Model.fromListReqComponents
                         $ fmap (Just . Model.CompReqValues . Model.Exp) <$> results
       logDebugN $ "mea normalized results: "  <> show normalizedRes

       fullsetResults <-
             if null normalizedRes
                then do
                  logWarnN "None of the fullset component requests were valid."
                  pure Nothing
                else pure $ Just normalizedRes

       logDebugN $ "subsets: "  <> show subsetResults
       logDebugN $ "fullSets: " <> show fullsetResults

       -- return a tuple for the single version of ComponentMix (not Mixes)
       -- m (Maybe (Model.MeaKey, Maybe Model.ReqComponents))
       pure . Just $ (fst keyValues, subsetResults <> fullsetResults)


---------------------------------------------------------------------------------
-- |
--
-- > ComponentReqInput {
-- >   componentNames:  [String!] <<< transform*ed* to multiple single inputs
-- >   componentName:   String    <<< on its own, means `Exp all values`
-- >   comopnentValues: FieldValuesCompReqInput
-- > }
--
-- > Components { components ::  Map Key CompValues }
--
-- > ReqComponents { reqComponents ::  Map Key (Maybe CompReqValues) }
--
-- Three inputs
-- - CompKey, TagRedExp and FieldValues
--
-- /Note/ Subsets will over-write fullset selections (left-bias).
-- The subset request may be problematic.  How consider two mixes that differ
-- only in the RedExp tag? Answer: For auto-generation, we use Exp, for custom
-- the user will provide Gql Aliases to disambiguate the request.
--
-- /Note/: A fullset request is a request for a series of data using all of
-- the values in that component... Powerful long -> wide view request. This
-- actuality is reflected in ReqCompValue Exp CompValues with every value
-- included if CompReqValues are not specified (augmented request). Reduced
-- is not included unless a subset request is made.
--
fetchSubsetComponents :: (MonadLogger m, MonadThrow m)
                      => [GqlInput.SubsetCompReq] -> Model.Components
                      -> m (Maybe Model.ReqComponents)
fetchSubsetComponents [] _ = pure Nothing
fetchSubsetComponents requests etl = do

  result <- Model.fromListReqComponents . catMaybes
            <$> traverse fetchComponent requests

  if null result
     then do
        logWarnN $ ("Invalid key(s) in the component subset request(s)."::Text)
                 <> show (catMaybes $ GqlInput.requestKey <$> requests)
        pure Nothing
     else pure $ Just result

  where
    -- :: -> m Maybe (key, Maybe values)
    fetchComponent req = do
       let fullsetResult = getEtlFragment etl mkCompKey req
       logDebugN $ ("getEltFragment: "::Text) <> show fullsetResult
                 <> ("\netl: "::Text) <> show etl
                 <> ("\nreq: "::Text) <> show req

       case fullsetResult of
          Nothing -> pure Nothing  -- invalid key
          Just (key, etlValues) -> do
            values <- filterCompReqValues
                      (fromInputReqCompValues (GqlInput.compValuesInput req))
                      etlValues

            -- Warn about valid key, but invalid subset
            if null values
               then do
                  logWarnN $ "Component filter failed; no filter applied"
                           <> "\n key: " <> show key
                           <> "\n failed filter: " <> show values
                  pure $ Just (key, Nothing)    -- valid key, no values
               else do
                  logDebugN $ "Component subset request for key: " <> show key
                  pure $ Just (key, Just values)


---------------------------------------------------------------------------------
-- |
--
fromInputReqCompValues :: GqlInput.CompValuesReqInput -> Model.CompReqValues
fromInputReqCompValues (GqlInput.CompValuesReqInput txt int span red)
    | red       = Model.CompReqValues . Model.Red $ go (GqlInput.CompValuesInput txt int span)
    | otherwise = Model.CompReqValues . Model.Exp $ go (GqlInput.CompValuesInput txt int span)
    where go = Shared.fromInputCompValues
