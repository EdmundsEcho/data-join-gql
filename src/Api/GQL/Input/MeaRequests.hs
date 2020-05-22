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
import           Protolude               hiding (null)
---------------------------------------------------------------------------------
import           Data.Maybe              (fromJust)
---------------------------------------------------------------------------------
import           Control.Exception.Safe
import           Control.Monad.Logger
---------------------------------------------------------------------------------
import           Api.ETL                 (requestCompReqValues)
import qualified Api.GQL.ObsETL          as Shared
---------------------------------------------------------------------------------
import           Model.ETL.ObsETL        (mkCompKey, mkMeaKey)
import qualified Model.ETL.ObsETL        as Model (CompValues, Components,
                                                   MeaKey, Measurements, Span,
                                                   TagRedExp (..))
import qualified Model.Request           as Model (CompReqValues (..),
                                                   ReqComponents (..), mkSpan)
---------------------------------------------------------------------------------
import           Model.ETL.Fragment
import           Model.Search
---------------------------------------------------------------------------------
import qualified Api.GQL.Schemas.Request as GqlInput
import qualified Api.GQL.Schemas.Shared  as GqlInput
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
-- * Subset success: determined by the components requested
--
-- === Issue?
-- If we want a series, we have to create aliases for the request
-- because we are using the same meaType and compName over and over for
-- each output.  Recall, an output is a field in the matrix data table.
--
fetchSubsetComponentMix :: (MonadLogger m, MonadThrow m)
                        => GqlInput.SubsetCompMixReq -> Model.Measurements
                        -> m (Maybe (Model.MeaKey, Maybe MeaETLSubset))

fetchSubsetComponentMix req etl = do

  let result = getEtlFragment etl mkMeaKey req

  case result of
    -- 1. did the key return a collection?
    -- No, return Nothing.
    Nothing -> do
      logWarnN $ ("Measurement key does not exist: " :: Text)
               <> show (fromJust $ GqlInput.requestKey req)
      pure Nothing

    -- Yes,
    -- run the next keyValues search
    Just keyValues -> do
    -- (MeaKey, Components)

       logDebugN "COMPMIX"

       let nextRequest = GqlInput.compReqInput req
       logDebugN $ "nextRequest from parent: " <> show (len nextRequest)


       -- Subset requests
       --------------------------------------------------------------------------
       let subsetReqs = concat $ GqlInput.subsets <$> nextRequest
       subsetResults <- fetchSubsetComponents subsetReqs (snd keyValues)


       -- Fullset requests
       --------------------------------------------------------------------------
       let fullsetReqs = concat $ GqlInput.fullsets <$> nextRequest

       let results = fromListExpComponents
                   . catMaybes
                   $ getEtlFragment (snd keyValues) mkCompKey
                   <$> fullsetReqs  -- lift over a list of requests

       fullsetResults <-
          if null fullsetReqs
             then pure Nothing

          else if null results
             then do
                logWarnN "=> None of the fullset component requests were valid."
                logWarnN $ show fullsetReqs
                pure Nothing

             else pure $ Just results

       logDebugN "Measurement request"
       logDebugN $ "...subset requests: " <> show (len subsetReqs)
       logDebugN . show $ GqlInput.compKey <$> subsetReqs
       logDebugN $ ("=> subsets: "::Text) <> show (len subsetResults)

       logDebugN $ "...fullset requests: "  <> show (len fullsetReqs)
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
fetchSubsetComponents :: (MonadLogger m, MonadThrow m)
                      => [GqlInput.SubsetCompReq] -> Model.Components
                      -> m (Maybe (SearchFragment Model.ReqComponents 'ETLSubset))
fetchSubsetComponents [] _ = pure Nothing
fetchSubsetComponents requests etl = do

  logDebugN ("Running components subset request"::Text)

  result <- fromListReqComponents . catMaybes
            <$> traverse fetchComponent requests

  if null result
     then do
        logWarnN $ ("Invalid key(s) in the component subset request(s)."::Text)
                 <> show (catMaybes $ GqlInput.requestKey <$> requests)
        pure Nothing
     else pure $ Just result

  where
    -- debugging SpanType
    -- isSpanType :: Maybe (Model.CompKey, Model.CompValues) -> Bool
    isSpanType Nothing        = False
    isSpanType (Just (key,_)) = key == mkCompKey "SpanKey"

    -- :: -> m Maybe (key, Maybe values)
    fetchComponent req = do
       let fullsetFragment = getEtlFragment etl mkCompKey req
       logDebugN ("fetchComponent etl"::Text)

       -- debugging SpanType
       if isSpanType fullsetFragment
          then
             logDebugN $ ("*** SpanType *** fragment\n"::Text)
                       <> show (fmap len <$> fullsetFragment)
                       <> ("\netl: "::Text) <> show fullsetFragment
                       <> ("\nreq: "::Text) <> show req
          else
             logDebugN $ ("*** other *** fragment\n"::Text)
                       <> show (fmap len <$> fullsetFragment)

       case fullsetFragment of
          Nothing -> do
            logWarnN $ "fullsetFragment invalid key: "
                     <> show (GqlInput.requestKey req)
            pure Nothing  -- invalid key

          -- request values
          Just (key, etlValues) -> do
            logDebugN $ "...making the request from ETL: " <> show key
            values <- requestCompReqValues
                      (fromInputReqCompValues (GqlInput.compValuesInput req))
                      etlValues

            -- debugging SpanType
            if key == mkCompKey "SpanKey"
               then logWarnN $ "*** Spantype search result ***\n"
                             <> show values
               else pure ()

            -- Warn about valid key, but invalid subset
            if null values
               then do
                  logWarnN "MeaRequests line 207 *** null values ***"
                  logWarnN $ "Component filter failed despite a valid key; no filter applied"
                           <> "\n key: " <> show key
                           <> "\n failed filter: " <> show values
                  pure $ Just (key, Nothing)    -- valid key, no values
               else do
                  logDebugN $ "Component subset request for key: " <> show key
                  pure $ Just (key, Just values)


---------------------------------------------------------------------------------
-- |
-- This could be refactored to share more from ObsETL.  The only difference
-- is the use of 'Model.ETL.Span.mkSpan' instead of the monadic version.
--
-- This request version does not fail when provided negative span lengths.
--
fromInputReqCompValues :: GqlInput.CompValuesReqInput -> Model.CompReqValues
fromInputReqCompValues (GqlInput.CompValuesReqInput txt int span red)
    | red       = Model.CompReqValues . Model.Red $ go (GqlInput.CompValuesInput txt int span)
    | otherwise = Model.CompReqValues . Model.Exp $ go (GqlInput.CompValuesInput txt int span)
    where go = fromRequest

fromRequest :: Shared.CompValuesInput -> Model.CompValues
fromRequest Shared.CompValuesInput { txtValues  = Just vs } = (fromList @Model.CompValues) vs
fromRequest Shared.CompValuesInput { intValues  = Just vs } = (fromList @Model.CompValues) vs
fromRequest Shared.CompValuesInput { spanValues = Just vs } =
  (fromList @Model.CompValues) $ spanFromInput <$> vs
      where
        -- | GraphQL -> Model
        spanFromInput :: Shared.SpanInput -> Model.Span
        spanFromInput Shared.SpanInput{..}
          | reduced   = Model.mkSpan Model.Red rangeStart rangeLength
          | otherwise = Model.mkSpan Model.Exp rangeStart rangeLength

fromRequest Shared.CompValuesInput {}
  = panic "The values type does not match that for CompValues"






---------------------------------------------------------------------------------
