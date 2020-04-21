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
-- Keep in mind,
-- a request is a series of search terms. The @schemas.request.graphql@ define
-- the input object specifications accordingly. However, a 'Request' is not
-- data unto themselves. This line is blurred by the fact that 'Model.Request'
-- is instantiated using 'Model.ObsETL' data. However, is viewed using the
-- data types described in @schemas.request.graphql@.
--
-- Also note, the use of 'Model.ETL.TagRedExp' is used in two ways.  First
-- for Span both in the ETL and Request phases.  Second, to qualify the
-- requests for Components ('Model.Request.ReqComponents'). There is likely redundant use of
-- the 'Model.ETL.TagRedExp' for a 'Model.ETL.Span' included in a 'Request'.
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
import           Api.GQL.ObsETL         (ComponentValues, FieldValuesInput (..),
                                         Quality, SpanInput (..))
import qualified Api.GQL.ObsETL         as Shared
import           Api.GqlHttp
-------------------------------------------------------------------------------
import qualified Model.ETL.FieldValues  as Model (fromList)
import qualified Model.ETL.ObsETL       as Model (CompKey, Components,
                                                  FieldValues (..),
                                                  Measurements, ObsETL (..),
                                                  QualKey, Qualities (..),
                                                  Range (..), Span (..),
                                                  Subject, TagRedExp (..),
                                                  componentNames, isRed,
                                                  mkMeaKey, mkQualKey, mkSubKey,
                                                  unKey, unTag)
import qualified Model.ETL.Transformers as Trans
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
-- Validation = Instantiate 'Model.Request' with the user input objects
-- specified in @schemas.request.graphql@.
--
resolverValidate :: ValidateArgs -> OptionalObject QUERY Request
resolverValidate ValidateArgs { request } = fetchRequest request

getDb :: GraphQL o => Value o App.Database
getDb = do
  dbTVar <- lift $ asks App.database
  liftIO . atomically $ readTVar dbTVar

---------------------------------------------------------------------------------
-- |
-- == Overall
--
-- > RequestInput -fetch-> etl -> Model.Request -resolver-> Object o Request
--
-- /Note/: Once a Model.Request is instantiated, we guarantee a return value.
--
-- > ChildReqInput -> (parent etl collection -fetch-> child etl collection)
-- >               -> Model.Request -> Object o Request
--
-- fetch :: 'RequestInput' -> 'Model.Request'
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
fetchRequest req = do
  -- get the data, return nothing if not initiated with the right data
  obsEtl <- fmap App.db getDb
  case obsEtl of
    App.DataObsETL obsEtl' ->   -- have the correct data
      case fetchRequest' req obsEtl' of -- :: Input -> Maybe Model
        Nothing      -> pure Nothing
        Just request -> Just <$> resolverRequest request -- (resolverRequest request)

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

---------------------------------------------------------------------------------
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

---------------------------------------------------------------------------------
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

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- Request data types -> Types specified in the schema
-- Request (Resolver o () AppObs)
resolverRequest :: GraphQL o => Model.Request -> Object o Request
resolverRequest Model.Request {..} =
  pure $
    Request
      (Just <$> resolverQualityMix subReq)
       -- :: Model.QualityMix -> QualityMix
      (Just <$> traverse resolverComponentMix meaReqs)
       -- :: [Model.ComponentMix] -> [ComponentMix!]

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
resolverQualityMix :: GraphQL o => Model.QualityMix -> Object o QualityMix
resolverQualityMix Model.QualityMix {..} =
  pure $
    QualityMix
      (Shared.resolverSubType subType)      -- :: Model.Key       -> String!
      (Shared.resolverQualities qualityMix) -- :: Model.Qualities -> [Quality!]!

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
resolverComponentMix :: GraphQL o => Model.ComponentMix -> Object o ComponentMix
resolverComponentMix Model.ComponentMix {..} =
  pure $
    ComponentMix
      (pure $ Model.unKey meaType)         -- :: Model.Key           -> String!
      (resolverReqComponents componentMix) -- :: Model.ReqComponents -> [ReqComponent!]!

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- /Note/: The use of @traverse identity :: [Object o a] -> ArrayObject o a
-- to coerce the change in type.
--
-- > ReqComponents
-- >   { reqComponents :: Map Key CompReqValues
-- >   } deriving (Show, Eq, Ord, Generic)
--
-- > type ReqComponent {
-- >   componentName: String!
-- >   values: CompReqValues!
-- > }
--
resolverReqComponents :: GraphQL o => Model.ReqComponents -> ArrayObject o ReqComponent
resolverReqComponents reqComps =
  traverse identity $ Trans.fromReqComponents resolverReqComponent reqComps

resolverReqComponent :: GraphQL o => Model.CompKey -> Model.CompReqValues -> Object o ReqComponent
resolverReqComponent compKey o' =
  pure $
    ReqComponent
      (pure $ Model.unKey compKey) -- :: Model.CompKey -> String!
      (resolverCompReqValues o') -- :: Model.CompReqValues ->  CompReqValues!

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
--
-- > CompReqValues { values :: TagRedExp CompValues }
--
-- > type CompReqValues {
-- >   values: ComponentValues
-- >   reduced: Boolean!
-- > }
--
resolverCompReqValues :: GraphQL o => Model.CompReqValues -> Object o CompReqValues
resolverCompReqValues (Model.CompReqValues taggedValues) =
    pure $
      CompReqValues {
        values  = Just <$> (Shared.resolverCompValues $ Model.unTag taggedValues)
      , reduced = pure $ Model.isRed taggedValues
      }

---------------------------------------------------------------------------------
-- |
-- Utility function
dedup :: Ord a => [a] -> [a]
dedup = Set.toList . Set.fromList
