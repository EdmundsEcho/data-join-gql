{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module     : Api.GQL.Request
-- Description: UI access point
--
-- /Note/: The design mistangly tries to leverage the ObsETL data types where
-- possible.  However, in order to maintain a clear distinction between request
-- and the ETL source of truth, all inputs should be tagged (one way or another)
-- with Request moniker.
--
-- Also note, the use of Red/Exp is used in two ways.  First fro Span both
-- in the ETL and Request phases.  Second, to qualify the requests for
-- Components (ReqComponents). Is there a way to unify?
--
module Api.GQL.Request where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Morpheus.Document (importGQLDocument)
import           Data.Morpheus.Types
-------------------------------------------------------------------------------
import           Control.Concurrent.STM
import qualified Data.Set               as Set
-------------------------------------------------------------------------------
import           Api.ETL
import           Api.GQL.ObsETL         hiding (Query)
-- (FieldValuesInput (..), Quality,
                                         -- QualityInput (..), Span, SpanInput)
import           Api.GqlHttp
-------------------------------------------------------------------------------
import qualified Model.ETL.FieldValues  as Model (fromList)
import qualified Model.ETL.ObsETL       as Model (CompKey, Components,
                                                  FieldValues (..),
                                                  Measurements, ObsETL (..),
                                                  QualKey, QualValues,
                                                  Qualities (..), Range (..),
                                                  Span (..), Subject,
                                                  TagRedExp (..),
                                                  componentNames, mkCompKey,
                                                  mkMeaKey, mkQualKey, mkSubKey)
import qualified Model.Request          as Model (CompReqValues (..),
                                                  ComponentMix (..),
                                                  QualityMix (..),
                                                  ReqComponents (..),
                                                  Request (..), mkQualityMix)
-- AppObs types
import           AppTypes               (AppObs)
import qualified AppTypes               as App
-------------------------------------------------------------------------------
-- importGQLDocument "src/Api/GQL/schema.shared.graphql"
importGQLDocument "src/Api/GQL/schema.request.graphql"
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- |
-- == Root Resolver
-- Maries Queries and Mutations
gqlRoot :: GQLRootResolver AppObs () Query Undefined Undefined
gqlRoot = rootResolver

rootResolver :: GQLRootResolver AppObs () Query Undefined Undefined
rootResolver =
   GQLRootResolver
       { queryResolver = Query { validate = resolverValidate }
       , mutationResolver = Undefined
       , subscriptionResolver = Undefined
       }

---------------------------------------------------------------------------------
-- |
-- == GQL Query Resolvers
-- Validation = Instantiate Model.Request with the user input GQL.Request.
--
resolverValidate :: ValidateArgs -> OptionalObject QUERY Request
resolverValidate ValidateArgs { request } = fetchRequest request

getDb :: GraphQL o => Value o App.Database
getDb = do
  dbTVar <- lift $ asks App.database
  liftIO . atomically $ readTVar dbTVar

-- |
-- Model -> GraphQL View
-- Request data types -> Types specified in the schema
resolverRequest :: GraphQL o => Model.Request -> OptionalObject o Request
resolverRequest = undefined

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-- |
-- == Overall
-- > RequestInput -fetch-> etl -> Model.Request -resolver-> Object o Request
-- >
-- > ChildReqInput -> (parent etl collection -fetch-> child etl collection)
-- >               -> Model.Request -> Object o Request
--
-- > input RequestInput {
-- >   subReq: QualityMixInput
-- >   meaReqs: [ComponentMixInput!]
-- > }
--
-- > data Request = Request
-- >   { subReq  :: !QualityMix
-- >   , meaReqs :: ![ComponentMix]
-- >   } deriving (Show, Eq)
--
--
fetchRequest :: GraphQL o => RequestInput -> OptionalObject o Request
-- fetchRequest (RequestInput Nothing Nothing) = pure Nothing
-- fetchRequest (RequestInput (Just subReq) (Just meaReqs)) = do
fetchRequest req = do
  -- get the data, return nothing if not initiated with the right data
  obsEtl <- fmap App.db getDb
  case obsEtl of
    App.DataObsETL obsEtl' ->   -- have the correct data
      case fetchRequest' req obsEtl' of  -- :: Input -> Model
        Just requestM -> resolverRequest requestM  -- :: Model -> Object o Request
        _             -> pure Nothing
    _ -> pure Nothing          -- have the wrong data

    where
      -- exploits global access to db's etl :: ObsETL
      fetchRequest' :: RequestInput -> Model.ObsETL -> Maybe Model.Request
      fetchRequest' request etl =
        case request of
          RequestInput (Just subReq) (Just meaReqs) -> do
            -- Subject arm
            sub <- Api.ETL.lookupSubject etl
            subReq' <- fetchQualityMix subReq sub

            -- Measurements arm
            meas <- Api.ETL.lookupMeasurements etl
            -- look ahead at ComponentMixInput,
            -- convert measurementTypes [String] to multiple single requests
            -- ComponentMixInput { measurementType: Just Text }
            -- cons the individual requests to [ComponentMixInput]
            let meaReqs' = catMaybes
                          $ flip fetchComponentMix meas
                          <$> concat (traverse preProcess meaReqs)
                          -- meaReqs [ComponentMixInput]

            pure $ Model.Request {
              subReq  = subReq',  -- :: QualityMix
              meaReqs = meaReqs'  -- :: [ComponentMix]
            }

          -- for now, default everything else to Nothing
          RequestInput _ _ -> Nothing

        where
          preProcess :: ComponentMixInput -> [ComponentMixInput]
          preProcess (ComponentMixInput (Just meaTypes) _ _) =
            fmap (\meaType -> ComponentMixInput Nothing (Just meaType) Nothing) meaTypes
          preProcess meaReq = [meaReq]


---------------------------------------------------------------------------------
-- |
-- GQLInput -> Model.Request
-- GQLInput specified by the schema.request.graphql
-- Model.Request 'Model.Request'
--
-- /Recall/: The input is a search term.  It is not valid etl data.
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
--
-- subject (type: String!) Subject
fetchQualityMix :: QualityMixInput -> Model.Subject -> Maybe Model.QualityMix
fetchQualityMix request etl =
  case request of
    QualityMixInput subjectTypeReq qualityMixReq -> do
      let subKey = Model.mkSubKey subjectTypeReq
      qualities <- Api.ETL.lookupQualities subKey etl
      result <- fetchQualities qualityMixReq qualities
      pure $ Model.mkQualityMix subKey result

-- |
-- QualityReqInput {
--   qualityNames: [String!]
--   qualityName: String
--   qualityValues: FieldValuesInput
-- }
-- Pattern:
-- Extract the request fragment
-- Extract the etl fragment
-- Run the search Model -fetch-> some temporary List -from-> Model
--
-- fromInputQualities :: [QualityInput] -> Model.Qualities
-- /Note/ Subsets will over-write fullset selections (left-bias).
--
fetchQualities :: [QualityReqInput] -> Model.Qualities -> Maybe Model.Qualities
fetchQualities requests etl =
  let subsets = selectQualities (catMaybes (f1 <$> requests)) etl
      fullsets = Just $ filterQualities (concat (catMaybes (f2 <$> requests))) etl
   in subsets <> fullsets

  -- tag the requests with the type that implements the search
  where
    f1 :: QualityReqInput -> Maybe (Text, Model.FieldValues)
    f1 (QualityReqInput _ (Just k) (Just vs)) = case vs of

      FieldValuesInput (Just values) Nothing Nothing ->
        Just (k, Model.TxtSet $ Model.fromList values)

      FieldValuesInput Nothing (Just values) Nothing ->
        Just (k, Model.IntSet $ Model.fromList values)

      FieldValuesInput {} -> Nothing

    -- No fields to select, no subsetting to do
    f1 (QualityReqInput _ _ Nothing) = Nothing
    f1 _ = Nothing

    f2 :: QualityReqInput -> Maybe [Model.QualKey]
    f2 (QualityReqInput maybeKeys _ _) = do
      keys <- maybeKeys
      pure $ Model.mkQualKey <$> keys

-- |
-- ReqInput {
--   names: [String!]
--   name: String
--   values: FieldValuesCompReqInput
-- }
-- /Note/ Subsets will over-write fullset selections (left-bias).
-- The subset request may be problematic.  How consider two mixes that differ
-- only in the RedExp tag?
-- TODO
-- /Note/: A fullset request is a request for a series of data using all of
-- the values in that component... Powerful long -> wide view request.
fetchComponents :: [ComponentReqInput] -> Model.Components -> Maybe Model.ReqComponents
fetchComponents requests etl =
  let subsets = selectReqComponents (catMaybes (f1 <$> requests)) etl
      fullsets = Just $ filterReqComponents (concat (catMaybes (f2 <$> requests))) etl
   in subsets <> fullsets

  -- tag the requests with the type that implements the search
  where
    f1 :: ComponentReqInput -> Maybe (Text, Model.CompReqValues)
    f1 (ComponentReqInput _ (Just k) (Just vs)) = case vs of

      --   values: FieldValuesCompReqInput {
      --     txtValues
      --     intValues
      --     spanValues
      --     reduced
      --   }
      --
      FieldValuesCompReqInput (Just values) Nothing Nothing red ->
        if red then Just (k, (Model.CompReqValues . Model.Red) . Model.TxtSet $ Model.fromList values)
        else Just        (k, (Model.CompReqValues . Model.Exp) . Model.TxtSet $ Model.fromList values)

      FieldValuesCompReqInput Nothing (Just values) Nothing red ->
        if red then Just (k, (Model.CompReqValues . Model.Red) . Model.IntSet $ Model.fromList values)
        else Just        (k, (Model.CompReqValues . Model.Exp) . Model.IntSet $ Model.fromList values)

      FieldValuesCompReqInput Nothing Nothing (Just values) red ->
        if red then Just (k, (Model.CompReqValues . Model.Red) . Model.SpanSet $ Model.fromList (spanFromInput <$>values))
        else Just        (k, (Model.CompReqValues . Model.Exp) . Model.SpanSet $ Model.fromList (spanFromInput <$> values))

      FieldValuesCompReqInput {} -> Nothing

    -- No fields, no subsetting
    f1 (ComponentReqInput _ _ Nothing) = Nothing
    f1 _ = Nothing

    f2 :: ComponentReqInput -> Maybe [Model.QualKey]
    f2 (ComponentReqInput maybeKeys _ _) = do
      keys <- maybeKeys
      pure $ Model.mkQualKey <$> keys

    -- | GraphQL -> Model
    spanFromInput :: SpanInput -> Model.Span
    spanFromInput SpanInput{..}
      | reduced   = Model.Span $ Model.Red (Model.Range rangeStart rangeLength)
      | otherwise = Model.Span $ Model.Exp (Model.Range rangeStart rangeLength)

-- |
-- There are two types of requests baked into this field.
--
-- 1. measurementTypes
--    * Measurent(s) -> No filtration, just tag with Expressed to create a series
--    * This type of request must be pre-processed and combined with the list
--      of single MeaType requests.
--
-- 2. measurementType + componentMix
--    * Includes componentMix which encodes subsetting the components
--
-- /Note/: @Red@ is the same as not having this request.
-- @Exp@ turns the component long -> wide series
--
-- > input ComponentMixInput {
-- >   measurementTypes: [String!]
-- >   measurementType: String
-- >   componentMix: [ComponentReqInput!]
-- > }
--
-- > data ComponentMix {
-- >   meaType: Key
-- >   componentMix: ReqComponents
-- > }
--
-- Issue?: If we want a series, we have to create aliases for the request
-- because we are using the same meaType and compName over and over for
-- each output.  Recall, an output is a field in the matrix data table.
--
fetchComponentMix :: ComponentMixInput -> Model.Measurements -> Maybe Model.ComponentMix

fetchComponentMix (ComponentMixInput (Just _) _ _) _ =
  panic $ "Cannot process multiple meaKeys simultaneously here; " <>
          "the request was not properly pre-processed"
-- |
-- This is a single, Measurement only request.
-- The computational task:
-- Augment the request with an instance of 'ComponentReqInput'.
-- Downstream, the 'CompReqValues' instance will include the Exp TagRedExp
--
-- /Note/: This will produce as many fields as there are
-- component 1 set length x component 2 x component 3 in the matrix
-- data request.
--
-- /Note/: The list of MeaTypes that is also possible to request are funneled
-- upstream by pre-processing the request.
--
fetchComponentMix (ComponentMixInput Nothing (Just meaKey) Nothing) etl = do
  let meaType' = Model.mkMeaKey meaKey
  etl' <- Api.ETL.lookupComponents meaType' etl
  let names' = Model.componentNames etl'
      augmentedRequest = ComponentReqInput (Just names') Nothing Nothing
  componentMix' <- fetchComponents [augmentedRequest] etl'
  pure Model.ComponentMix {
    meaType = meaType',
    componentMix = componentMix'
  }
-- |
-- Second Request type.
-- Subset of values for each of the components specified.
fetchComponentMix (ComponentMixInput Nothing (Just meaKey) (Just compReqs)) etl = do
  let meaType' = Model.mkMeaKey meaKey
  etl' <- Api.ETL.lookupComponents meaType' etl
  componentMix' <- fetchComponents compReqs etl'
  pure Model.ComponentMix {
    meaType = meaType',
    componentMix = componentMix'
  }

fetchComponentMix (ComponentMixInput _ _ _) _ = panic "malformed query"

-- |
-- Utility function
dedup :: Ord a => [a] -> [a]
dedup = Set.toList . Set.fromList
