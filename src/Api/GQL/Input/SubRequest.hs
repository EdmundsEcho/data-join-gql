{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module     : Api.GQL.Input.SubRequest
-- Description: Subject branch of the 'Api.GQL.Input.Request'
--
module Api.GQL.Input.SubRequest where
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
import           Model.ETL.ObsETL        (mkQualKey, mkSubKey, unKey)
import qualified Model.ETL.ObsETL        as Model (QualValues, Qualities (..),
                                                   SubKey, Subject, subType)
import qualified Model.Request           as Model (ReqQualities)
---------------------------------------------------------------------------------
import           Model.ETL.Fragment
import           Model.Search
---------------------------------------------------------------------------------
import qualified Api.GQL.Schemas.Request as GqlInput
import qualified Api.GQL.Schemas.Shared  as GqlInput
---------------------------------------------------------------------------------
  -- SubRequest - Qualities arm
---------------------------------------------------------------------------------
ppRequest :: Maybe GqlInput.QualityMixInput -> Model.Subject
          -> GqlInput.QualityMixInput
ppRequest req etl =
  fromJust $ req <|> (Just . GqlInput.minQualityMixInput
                           . unKey
                           . Model.subType
                           $ etl)

-- |
--
-- >  QualityMixInput {
-- >    subjectType: String
-- >    qualityMix: [QualityReqInput!]
-- >  }
--
-- > data QualityMix = QualityMix
-- >   { subType    :: !Key
-- >   , qualityMix :: !Qualities
-- >   } deriving (Show, Eq)
--
-- /Note/: fullset and subset are @Request@ specific concepts. ETL is always
-- fullset (by defintion).
--
fetchQualityMix :: (MonadLogger m, MonadThrow m)
                => Maybe GqlInput.QualityMixInput -> Model.Subject
                -> m (Maybe (Model.SubKey, Maybe(SearchFragment Model.ReqQualities 'ETLSubset)))

fetchQualityMix maybeRequest etl = do

  let request' :: GqlInput.QualityMixInput = ppRequest maybeRequest etl
      reqKey   = (\GqlInput.QualityMixInput {subjectType}  -- avoid namespace collision
                 -> mkSubKey $ fromJust subjectType) request'

  let result = getEtlFragment etl mkSubKey request'

  case result of
    -- 1. did the key return qualities?
    -- No, return Nothing.
    Nothing -> do
      logWarnN $ "Subject key does not exist: " <> show reqKey
      pure Nothing

    -- Yes,
    -- run the maybe QualMixReq search
    Just etlQuals ->

      case (\GqlInput.QualityMixInput {qualityMix}
            -> qualityMix) request' of

        -- 2. does a qualityMix request exist?
        -- No, return minimum viable response.
        Nothing -> do
          logDebugN $ "Subject type only request: " <> show (fst etlQuals)
          pure . Just $ (fst etlQuals, Nothing)

        -- Yes,
        -- run the maybe [QualityReqInput] on Qualities
        Just qualMixReq -> do

          -- propogate the request to child requests

          logDebugN "SUBJECT"

          -- Subset requests
          let subsetReqs = concat $ GqlInput.subsets <$> qualMixReq
          logDebugN $ "subject subset requests: " <> show (length subsetReqs)
          -- logDebugN $ "subsets...are they being captured?\n" <> show subsetReqs
          subsetResults     <- fetchSubsetQualities subsetReqs (snd etlQuals)

          -- Fullset requests
          let fullsetReqs = concat $ GqlInput.fullsets <$> qualMixReq
          logDebugN $ "subject fullset requests: " <> show (length fullsetReqs)
          let results       = catMaybes $ getEtlFragment (snd etlQuals) mkQualKey
                            <$> fullsetReqs  -- lift over a list of requests
          let normalizedRes = fromListReqQualities
                            $ (\(key, _) -> (key, Nothing)) <$> results
                            -- do not display the fullset of values
          logWarnN "Not displaying the fullset of quality values."

          fullsetResults <-
                if null normalizedRes
                   then do
                     logWarnN "None of the fullset quality requests were valid."
                     logWarnN $ show fullsetReqs
                     pure Nothing

                   else pure . Just $ normalizedRes

          logDebugN $ "Subject Results"
                    <> "\nsubsets: " <> show (length subsetResults)
                    <> "\nfullSets: " <> show (length fullsetResults)

          -- instantiate the Model.Request
          pure . Just $ ((subsetResults <> fullsetResults) <$ etlQuals)


-- |
-- Processes subset requests
--
-- > { qualNames  = Nothing
-- >   qualName   = Just n
-- >   qualValues = Just vs
-- > }
--
-- Processes fullset requests
--
-- > { qualNames  = Nothing
-- >   qualName   = Just n
-- >   qualValues = Nothing
-- > }
--
fetchSubsetQualities :: (MonadLogger m, MonadThrow m)
                     => [GqlInput.SubsetQualReq] -> Model.Qualities
                     -> m (Maybe (SearchFragment Model.ReqQualities 'ETLSubset))
fetchSubsetQualities [] _ = pure Nothing
fetchSubsetQualities requests etl = do

  result <- fromListReqQualities
            . catMaybes
            <$> traverse fetchQuality requests

  if null result
     then do
        logWarnN "Invalid key(s) in the quality subset request(s)."
        pure Nothing
     else pure $ Just result

  where
    -- :: -> m Maybe (key, Maybe SearchFragment QualValues 'ETLSubset)
    fetchQuality req = do
       let fullsetResult = getEtlFragment etl mkQualKey req
       case fullsetResult of
          Nothing -> pure Nothing               -- invalid key
          Just (key, etlValues) -> do
            values <- requestQualReqValues      -- data pull
                      (fromInputReqQualValues (GqlInput.qualValuesInput req))
                      etlValues

            -- Warn about valid key, but invalid subset
            if null values
               then do
                  logWarnN $ "Quality filter failed despite a valid key; no filter applied"
                           <> "\n key: " <> show key
                           <> "\n failed filter: " <> show values
                  pure $ Just (key, Nothing)    -- valid key, no values
               else do
                  logDebugN $ "Quality subset request for key: " <> show key
                  logDebugN $ "Values selected: " <> show values
                  pure $ Just (key, Just values)

---------------------------------------------------------------------------------
-- |
-- > QualityReqInput {
-- >   qualityNames:  [String!]
-- >   qualityName:   String
-- >   qualityValues: QualValuesInput
-- > }
--
-- > QualValuesInput {
-- >   txtValues: [String!]
-- >   intValues: [Int!]
-- > }
--
fromInputReqQualValues :: GqlInput.QualValuesInput -> Model.QualValues
fromInputReqQualValues = Shared.fromInputQualValues



---------------------------------------------------------------------------------
