{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module     : Api.GQL.Input.MeaRequests
-- Description: Measurements branch of the 'Api.GQL.Input.Request'
--
--
module Api.GQL.Input.MeaRequests where
---------------------------------------------------------------------------------
import           Protolude               hiding ( null )
---------------------------------------------------------------------------------
import           Data.Maybe                     ( fromJust )
---------------------------------------------------------------------------------
import           Api.ETL                        ( requestCompReqValues )
import qualified Api.GQL.ObsETL                as Shared
---------------------------------------------------------------------------------
import qualified Model.ETL.ObsETL              as Model
                                                ( CompValues
                                                , Components
                                                , MeaKey
                                                , Measurements
                                                , Span
                                                , toIncludeRequest
                                                , toExcludeRequest
                                                , ValuesReqEnum
                                                )
import qualified Model.ETL.TagRedExp           as Model
import qualified Model.Request                 as Model
                                                ( CompReqValues(..)
                                                , ReqComponents(..)
                                                , mkSpan
                                                )
---------------------------------------------------------------------------------
import           Model.ETL.Fragment
import           Model.Search
---------------------------------------------------------------------------------
import qualified Api.GQL.Schemas.Request       as GqlInput
import qualified Api.GQL.Schemas.Shared        as GqlInput
---------------------------------------------------------------------------------
import           WithAppContext
---------------------------------------------------------------------------------
  -- Measurements arm
---------------------------------------------------------------------------------
-- |
--
-- === Only subset requests
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
-- === Maybe encoding
--
-- * Overall success: determined by the measurement key
--
-- * Subset success: determined by the components requested
--
-- === No Alias Required
-- The struct returns Arrays avoiding the need to name repeated requests.
--
fetchSubsetComponentMix
  :: (MonadLogger m, MonadThrow m)
  => GqlInput.SubsetCompMixReq
  -> Model.Measurements
  -> m (Maybe (Model.MeaKey, Maybe MeaETLSubset))

fetchSubsetComponentMix req etl = do

  let result = getEtlFragment etl req
  logDebugN $ ("fetch MEAKEY: " :: Text) <> show
    (fromJust $ GqlInput.requestKey req)

  case result of
  -- Maybe (MeaKey, Components)

    -- 1. did the key return a collection?
    -- No, return Nothing.
    Nothing -> do
      logWarnN $ ("Measurement key does not exist: " :: Text) <> show
        (fromJust $ GqlInput.requestKey req)
      pure Nothing

    -- Yes,
    -- run the next keyValues search
    Just keyValues -> do
    -- (MeaKey, Components)

      logDebugN "fetch COMPMIX"

      let nextRequest = GqlInput.compReqInput req
      logDebugN $ "children: " <> show (len nextRequest)
      logDebugN $ ("component searches: " :: Text) <> show
        (   sum
        $   (\GqlInput.ComponentReqInput {..} ->
              maybe 0 length componentNames + maybe 0 (const 1) componentName
            )
        <$> nextRequest
        )

      -- Subset requests
      -- utilizes MixedRequest type class version of request
      --------------------------------------------------------------------------
      let subsetReqs = concat $ GqlInput.subsets <$> nextRequest
      subsetResults <- fetchSubsetComponents subsetReqs (snd keyValues)


      -- Fullset requests
      -- utilizes MixedRequest type class version of request
      --------------------------------------------------------------------------
      let fullsetReqs = concat $ GqlInput.fullsets <$> nextRequest

      let results =
            fromListExpComponents
              .   catMaybes
              $   getEtlFragment (snd keyValues)
              <$> fullsetReqs  -- lift over a list of requests

      fullsetResults <- if null fullsetReqs
        then pure Nothing
        else if null results
          then do
            logWarnN "=> None of the fullset component requests were valid."
            logWarnN $ show fullsetReqs
            pure Nothing
          else do
            logDebugN "=> fullset success"
            pure $ Just results

      logDebugN "Measurement request"
      logDebugN $ "...subset requests: " <> show (len subsetReqs)
      logDebugN . show $ GqlInput.compKey <$> subsetReqs
      logDebugN $ ("=> subsets: " :: Text) <> show (len subsetResults)

      logDebugN $ "...fullset requests: " <> show (len fullsetReqs)
      logDebugN $ "" <> show fullsetReqs
      logDebugN $ "=> fullSets: " <> show (len fullsetResults)

      -- combined result
      --------------------------------------------------------------------------
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
-- === /Notes/
--
-- Subsets will over-write fullset selections (left-bias).
-- The subset request may be problematic.  How consider two mixes that differ
-- only in the RedExp tag? Answer: For auto-generation, we use Exp, for custom
-- the user will provide Gql Aliases to disambiguate the request.
--
-- A fullset request is a request for a series of data using all of
-- the values in that component... Powerful long -> wide view request. This
-- actuality is reflected in ReqCompValue Exp CompValues with every value
-- included if CompReqValues are not specified (augmented request). Reduced
-- is not included unless a subset request is made.
--
fetchSubsetComponents
  :: (MonadLogger m, MonadThrow m)
  => [GqlInput.SubsetCompReq]
  -> Model.Components
  -> m (Maybe (SearchFragment Model.ReqComponents 'ETLSubset))
fetchSubsetComponents []       _   = pure Nothing
fetchSubsetComponents requests etl = do

  logDebugN "Running components subset requests"
  logDebugN $ show (GqlInput.compKey <$> requests)

  result <-
    fromListReqComponents . catMaybes <$> traverse fetchComponent requests

  if null result
    then do
      logWarnN
        $  ("Invalid key(s) in the component subset request(s)." :: Text)
        <> show (catMaybes $ GqlInput.requestKey <$> requests)
      pure Nothing
    else pure $ Just result

 where

    -- :: -> m Maybe (key, Maybe values)
    -- :: -> m Maybe (key, Maybe Model.CompReqValues 'ETLSubset)
    -- req hosts antiRequest
  fetchComponent req = do
    let exclude         = getAntiRequest req
    let fullsetFragment = getEtlFragment etl req
    logDebugN "ETL data"
    logDebugN $ ("fetchComponent key: " :: Text) <> show
      (GqlInput.requestKey req)
    logDebugN $ show (fmap showLen <$> fullsetFragment)
    logDebugF req

    case fullsetFragment of
      Nothing -> do
        logWarnN $ "fullsetFragment invalid key: " <> show
          (GqlInput.requestKey req)
        pure Nothing  -- invalid key

      -- request values
      Just (key, etlValues) -> do
        logDebugN $ "...making the request from ETL: " <> show key
        values <- requestCompReqValues
          (fromInputReqCompValues exclude (GqlInput.compValuesInput req))
          etlValues

        -- Warn about valid key, but invalid subset
        -- 🚧 ...less of an issue when the antiRequest = Exclude
        if null values
          then do
            logWarnN "MeaRequests line 208 *** null values ***"
            logWarnN
              $ "Component filter failed despite a valid key; no filter applied"
              <> "\n key: "
              <> show key
              <> "\n failed filter: "
              <> show values
            pure $ Just (key, Nothing)    -- valid key, no values
          else do
            logDebugN $ "Component subset request for key: " <> show key
            pure $ Just (key, Just values)

getAntiRequest :: GqlInput.SubsetCompReq -> Bool
getAntiRequest GqlInput.SubsetCompReq { antiRequest } = antiRequest

---------------------------------------------------------------------------------
-- |
-- This could be refactored to share more from ObsETL.  The only difference
-- is the use of 'Model.ETL.Span.mkSpan' instead of the monadic version.
--
-- 1️⃣  Input
-- This request version does not fail when provided negative span lengths.
-- > ComponentReqInput {
-- >   componentNames:  [String!]
-- >   componentName:   String
-- >   componentValues: CompValuesReqInput
-- >   antiRequest: Bool!
-- > }
--
-- > ComponentValuesReqInput {
-- >   txtValues: [String!]
-- >   intValues: [Int!]
-- >   spanValues: [Span!]
-- >   reduced: Boolean!
-- > }
--
-- 2️⃣  Ouput Model
-- newtype ReqComponents = ReqComponents
--         { reqComponents :: Map CompKey (Maybe CompReqValues)
--         } deriving (Show, Eq, Ord, Generic)

--
-- newtype CompReqValues = CompReqValues { values :: TagRedExp ValuesReqEnum }
--   deriving (Show, Eq, Ord, Generic)

--Task: the antiRequest information include in the function param
--
fromInputReqCompValues
  :: Exclude -> GqlInput.CompValuesReqInput -> Model.CompReqValues
fromInputReqCompValues exclude (GqlInput.CompValuesReqInput txt int span red)
  | red = Model.CompReqValues . Model.Red $ fromRequest
    (GqlInput.CompValuesInput txt int span)
  | otherwise = Model.CompReqValues . Model.Exp $ fromRequest
    (GqlInput.CompValuesInput txt int span)

 where
  -- uses exclude from the local/global scope
  fromRequest :: Shared.CompValuesInput -> Model.ValuesReqEnum
  fromRequest Shared.CompValuesInput { txtValues = Just vs } = if exclude
    then Model.toExcludeRequest $ (fromList @Model.CompValues) vs
    else Model.toIncludeRequest $ (fromList @Model.CompValues) vs
  fromRequest Shared.CompValuesInput { intValues = Just vs } = if exclude
    then Model.toExcludeRequest $ (fromList @Model.CompValues) vs
    else Model.toIncludeRequest $ (fromList @Model.CompValues) vs
  fromRequest Shared.CompValuesInput { spanValues = Just vs } = if exclude
    then
      Model.toExcludeRequest
      .   (fromList @Model.CompValues)
      $   spanFromInput
      <$> vs
    else
      Model.toIncludeRequest
      .   (fromList @Model.CompValues)
      $   spanFromInput
      <$> vs

   where
    -- GraphQL -> Model
    spanFromInput :: Shared.SpanInput -> Model.Span
    spanFromInput Shared.SpanInput {..}
      | reduced   = Model.mkSpan Model.Red rangeStart rangeLength
      | otherwise = Model.mkSpan Model.Exp rangeStart rangeLength

  fromRequest Shared.CompValuesInput{} =
    panic "The values type does not match that for CompValues"



---------------------------------------------------------------------------------
