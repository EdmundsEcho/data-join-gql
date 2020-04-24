{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

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
-- Also note, the use of 'Model.ETL.TagRedExp.TagRedExp' is used in two ways.
-- First for Span both in the ETL and Request phases.  Second, to qualify the
-- requests for Components ('Model.Request.ReqComponents'). There is likely
-- redundant use of the 'Model.ETL.TagRedExp.TagRedExp' for a
-- 'Model.ETL.Span.Span' included in a 'Request'.
--
module Api.GQL.Request where
---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import qualified Data.Set               as Set
---------------------------------------------------------------------------------
import           Api.ETL
import qualified Api.GQL.ObsETL         as Shared
import           Api.GqlHttp
---------------------------------------------------------------------------------
import qualified Model.ETL.FieldValues  as Model (fromList)
import qualified Model.ETL.ObsETL       as Model (CompKey, CompValues,
                                                  Components, FieldValues (..),
                                                  Measurements, ObsETL (..),
                                                  QualKey, Qualities (..),
                                                  Range (..), Span (..),
                                                  Subject, TagRedExp (..),
                                                  componentNames,
                                                  fromListIntValues,
                                                  fromListSpanValues,
                                                  fromListTxtValues, isRed,
                                                  mkCompKey, mkMeaKey,
                                                  mkQualKey, mkSubKey,
                                                  qualityNames, unKey, unTag)
import qualified Model.ETL.Transformers as Trans
import qualified Model.Request          as Model (CompReqValues (..),
                                                  ComponentMix (..),
                                                  QualityMix (..),
                                                  ReqComponents (..),
                                                  Request (..),
                                                  fromListReqComponents,
                                                  mkQualityMix)
---------------------------------------------------------------------------------
import           Api.GQL.Schemas        hiding (CompReqValues,
                                         ComponentReqInput,
                                         FieldValuesCompReqInput, ReqComponent)
import qualified Api.GQL.Schemas        as GqlType (CompReqValues (..),
                                                    ReqComponent (..))
import qualified Api.GQL.Schemas        as GqlInput (ComponentReqInput (..), FieldValuesCompReqInput (..))
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- * Fetch request
-- |
-- GQLInput -> Model.Request
-- /Note/: Will not display anything unless both fields are present
-- in the
-- > input Request { subReq, meaReqs }
--
fetchRequest :: RequestInput -> Model.ObsETL -> Maybe Model.Request
fetchRequest request etl =
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
      let subsets  = mconcat $ traverse ppSubset  meaReqs
      let fullsets = mconcat $ traverse ppFullSet meaReqs

      let subsets'  = catMaybes $ flip fetchComponentMix meas <$> subsets
      let fullsets' = catMaybes $ flip fetchComponentMix meas <$> fullsets

      let meaReqs' = subsets' <> fullsets'


      pure $ Model.Request {
        subReq  = subReq',  -- :: QualityMix
        meaReqs = meaReqs'  -- :: [ComponentMix]
      }

    -- for now, default everything else to Nothing
    RequestInput _ _ -> Nothing

  where
    -- full set requests
    ppFullSet :: ComponentMixInput -> [ComponentMixInput]
    ppFullSet (ComponentMixInput (Just keys) maybeKey Nothing) =
      let keys' = case maybeKey of
                      Just k  -> k:keys
                      Nothing -> keys
      in
        fmap (\key ->
          ComponentMixInput { measurementTypes = Nothing
                            , measurementType  = Just key
                            , componentMix = Nothing
                            }
         ) keys'
    ppFullSet req = [req] -- return untouched wrapped in list

    -- subset requests
    ppSubset :: ComponentMixInput -> [ComponentMixInput]
    ppSubset (ComponentMixInput Nothing (Just key) (Just vs)) =
             [ComponentMixInput Nothing (Just key) (Just vs)]
    ppSubset _ = []


---------------------------------------------------------------------------------
-- |
-- GQLInput -> Model.Request
-- GQLInput specified by the schema.request.graphql
-- Model.Request 'Model.Request'
--
-- The function will call itself in the event the `qualityMix` field is empty.
-- It will do so with an augmented request that includse all of the
-- 'Model.ETL.Qualities.Qualities' in the `qualityMix` field.
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
fetchQualityMix (QualityMixInput subTypeReq mixReq) etl = do

    -- retrieve etl subject
    let subKey = Model.mkSubKey subTypeReq
    etl' <- Api.ETL.lookupQualities subKey etl

    -- cascade the request
    case mixReq of

      -- the consumer/fetcher
      Just mix -> do
        let subsets  = traverse ppSubset  mix  -- :: [Model.Qualities]
        let fullsets = traverse ppFullSet mix  -- :: [Model.Qualities]

        let subsets'  = mconcat . catMaybes $ flip fetchQualities etl' <$> subsets
        let fullsets' = mconcat . catMaybes $ flip fetchQualities etl' <$> fullsets

        -- left bias
        let result = subsets' <> fullsets'

        pure $ Model.mkQualityMix subKey result

      -- augments before delegating fetch to the next call
      Nothing -> do
        -- augment this request
        let qualNames = Model.qualityNames etl'
        let qualReq = QualityReqInput { qualityNames  = Just qualNames
                                      , qualityName   = Nothing
                                      , qualityValues = Nothing
                                      }
        let qualityMixInput =
                      QualityMixInput { subjectType = subTypeReq
                                      , qualityMix  = Just [qualReq]
                                      }
        -- resend the request to this function
        fetchQualityMix qualityMixInput etl


    -- for valus in QualityReqInput { qualityNames }, we need to preProcess to
    -- eliminate them.
    where
      -- full set requests
      ppFullSet :: QualityReqInput -> [QualityReqInput]
      ppFullSet (QualityReqInput (Just qualNames) maybeName Nothing) =
        let names = case maybeName of
                       Just n  -> n:qualNames
                       Nothing -> qualNames
        in
          fmap (\qualName ->
            QualityReqInput { qualityNames  = Nothing
                            , qualityName   = Just qualName
                            , qualityValues = Nothing
                            }
           ) names
      ppFullSet qualReq = [qualReq] -- return untouched wrapped in list

      -- subset requests
      ppSubset :: QualityReqInput -> [QualityReqInput]
      ppSubset (QualityReqInput Nothing (Just name) (Just vs)) =
               [QualityReqInput Nothing (Just name) (Just vs)]
      ppSubset _ = []

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

    f1 (QualityReqInput (Just _) _ _) =
        panic $ "Cannot process multiple qualNames simultaneously here; " <>
                "the request was not properly pre-processed"

    f1 (QualityReqInput Nothing (Just k) (Just vs)) = case vs of

      FieldValuesInput (Just values) Nothing Nothing ->
        Just (k, Model.TxtSet $ Model.fromList values)

      FieldValuesInput Nothing (Just values) Nothing ->
        Just (k, Model.IntSet $ Model.fromList values)

      FieldValuesInput {} -> Nothing

    -- No fields to select, no subsetting to do
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
-- > input ComponentMixInput {
-- >   measurementTypes: [String!]  <<< should have been pre-processed
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
fetchComponentMix
  :: ComponentMixInput -> Model.Measurements -> Maybe Model.ComponentMix

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
  -- delegate to the next call with augmented request
  -- get the measurement from etl data
  let meaType' = Model.mkMeaKey meaKey
  etl' <- Api.ETL.lookupComponents meaType' etl
  -- get the names of each component for that measurement
  let compNames = Model.componentNames etl'
  -- augment this request with the component names
  let augmentedRequest
        = ComponentMixInput { measurementTypes = Nothing
                            , measurementType  = Just meaKey
                            , componentMix     = Just [preProcess compNames]
                            }

  -- recall this function with the augmented request
  fetchComponentMix augmentedRequest etl

  where
    preProcess :: [Name] -> GqlInput.ComponentReqInput
    preProcess compNames' =
         GqlInput.ComponentReqInput { componentNames  = Just compNames'
                           , componentName   = Nothing
                           , componentValues = Nothing
                           }

-- |
-- Second Request type.
-- Subset of values for each of the components specified.
fetchComponentMix (ComponentMixInput Nothing (Just meaKey) (Just compReqs)) etl = do
  -- consume the request
  -- get the measurement from etl data
  let meaType' = Model.mkMeaKey meaKey
  etl' <- Api.ETL.lookupComponents meaType' etl

  -- make several
  -- fullsets :: [ComponentReqInput]
  let subsets  = traverse ppSubset  compReqs
  let fullsets = traverse ppFullSet compReqs

  -- fullsets' :: [ReqComponents]
  let subsets'  = mconcat . catMaybes $ flip fetchComponents etl' <$> subsets
  let fullsets' = mconcat . catMaybes $ flip fetchComponents etl' <$> fullsets

  -- left bias
  let compMix' = subsets' <> fullsets'

  pure $ Model.ComponentMix {
    measurementType = meaType', -- :: Key
    componentMix    = compMix'  -- :: ReqComponents
  }

  where
    -- full set requests
    ppFullSet :: GqlInput.ComponentReqInput -> [GqlInput.ComponentReqInput]
    ppFullSet (GqlInput.ComponentReqInput (Just keys) maybeKey Nothing) =
      let keys' = case maybeKey of
                      Just k  -> k:keys
                      Nothing -> keys
      in
        fmap (\key ->
          GqlInput.ComponentReqInput { componentNames  = Nothing
                            , componentName   = Just key
                            , componentValues = Nothing
                            }
         ) keys'
    ppFullSet req = [req] -- return untouched wrapped in list

    -- subset requests
    ppSubset :: GqlInput.ComponentReqInput -> [GqlInput.ComponentReqInput]
    ppSubset (GqlInput.ComponentReqInput Nothing (Just key) (Just vs)) =
             [GqlInput.ComponentReqInput Nothing (Just key) (Just vs)]
    ppSubset _ = []

fetchComponentMix ComponentMixInput {} _ = panic "malformed query"

type Name = Text

---------------------------------------------------------------------------------
-- |
--
-- > ComponentReqInput {
-- >   componentNames: [String!]
-- >   componentName: String
-- >   comopnentValues: FieldValuesCompReqInput
-- > }
--
-- > ReqComponents { reqComponents ::  May Key CompReqValues }
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
-- the values in that component... Powerful long -> wide view request.
fetchComponents :: [GqlInput.ComponentReqInput]
                -> Model.Components -> Maybe Model.ReqComponents
fetchComponents requests etl =
  let
      requests' = preProcess <$> requests
      fullSetReqs = filter isFullSet requests'
      subSetReqs  = filter (not . isFullSet) requests'

      fullSetReqs' :: [(Model.CompKey, Model.CompReqValues)]
      fullSetReqs' = mapMaybe (`fetchFullSet` etl) fullSetReqs

      subSetReqs' :: [(Model.CompKey, Model.CompReqValues)]
      subSetReqs' = mapMaybe (`fetchSubset` etl) subSetReqs

   in
      Just $
        Model.fromListReqComponents subSetReqs' <>
        Model.fromListReqComponents fullSetReqs'

  -- Model.fromListReqComponents [(CompKey, CompReqValues)] -> ReqComponents
  where
    preProcess :: GqlInput.ComponentReqInput -> GqlInput.ComponentReqInput
    preProcess (GqlInput.ComponentReqInput (Just _) _ _) =
        panic $ "Cannot process multiple component names simultaneously here; " <>
                "the request was not properly pre-processed"
    preProcess req = req

    -- filterCompReqValues :: CompReqValues -> FieldValues -> Maybe CompReqValues
    fetchSubset :: GqlInput.ComponentReqInput
                -> Model.Components
                -> Maybe (Model.CompKey, Model.CompReqValues)
    fetchSubset req etl' = do
      (compKey, compValues) <- getEtlFragment req etl'
      -- extract and prep the req field values
      compReq <- GqlInput.componentValues req
      let compReq' = toCompReqValues  compReq
      -- fetch field values
      (compKey,) <$> filterCompReqValues compReq' compValues

    -- at this point, fullset has to imply Exp (create a series)
    -- otherwise, there is no values in subsetting, i.e., making this request
    -- We are displaying what the request would look like.
    -- What does it mean if did not specify details? => Expressed
    -- /Note/: Even when the user specifies every value, the computation
    -- will get to this place with a specified TagRedExp. So, that will not
    -- be confused with what we infer.
    fetchFullSet :: GqlInput.ComponentReqInput
                 -> Model.Components
                 -> Maybe (Model.CompKey, Model.CompReqValues)
    fetchFullSet req etl' =
      fmap (Model.CompReqValues . Model.Exp) <$> getEtlFragment req etl'

    -----------------------------------------------------------------------------
    getEtlFragment :: GqlInput.ComponentReqInput
                  -> Model.Components
                  -> Maybe (Model.CompKey, Model.CompValues)
    getEtlFragment req etl'' = do
      compName <- GqlInput.componentName req
      let compKey = Model.mkCompKey compName
      (compKey,) <$> lookupCompValues compKey etl''

    -----------------------------------------------------------------------------
    isFullSet :: GqlInput.ComponentReqInput -> Bool
    isFullSet (GqlInput.ComponentReqInput Nothing (Just _) Nothing) = True
    isFullSet _                                                     = False
    -----------------------------------------------------------------------------

toCompReqValues :: GqlInput.FieldValuesCompReqInput -> Model.CompReqValues
toCompReqValues input@(GqlInput.FieldValuesCompReqInput _ _ _ red)
    | red       = go Model.Red input
    | otherwise = go Model.Exp input
    where
      go redExp (GqlInput.FieldValuesCompReqInput (Just vs) _ _ _)
        = Model.CompReqValues . redExp $ Model.fromListTxtValues vs

      go redExp (GqlInput.FieldValuesCompReqInput _ (Just vs) _ _)
        = Model.CompReqValues . redExp $ Model.fromListIntValues vs

      go redExp (GqlInput.FieldValuesCompReqInput _ _ (Just vs) _)
        = Model.CompReqValues . redExp $ Model.fromListSpanValues (spanFromInput <$> vs)

      go _ GqlInput.FieldValuesCompReqInput {} =
        panic "The values type does not match FieldValues"

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
      { subReq = Just <$> resolverQualityMix subReq
       -- :: Model.QualityMix -> QualityMix
      , meaReqs = Just <$> traverse resolverComponentMix meaReqs
       -- :: [Model.ComponentMix] -> [ComponentMix!]
      }

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
resolverQualityMix :: GraphQL o => Model.QualityMix -> Object o QualityMix
resolverQualityMix Model.QualityMix {..} =
  pure $
    QualityMix
      { subjectType = Shared.resolverSubType subjectType  -- :: Model.Key -> String!
      , qualityMix  = Shared.resolverQualities qualityMix -- :: Model.Qualities -> [Quality!]!
      }

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
resolverComponentMix :: GraphQL o => Model.ComponentMix -> Object o ComponentMix
resolverComponentMix Model.ComponentMix {..} =
  pure $
    ComponentMix
      { measurementType = pure $ Model.unKey measurementType -- :: Model.Key -> String!
      , componentMix    = resolverReqComponents componentMix -- :: Model.ReqComponents -> [ReqComponent!]!
      }

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
resolverReqComponents :: GraphQL o => Model.ReqComponents -> ArrayObject o GqlType.ReqComponent
resolverReqComponents reqComps =
  traverse identity $ Trans.fromReqComponents resolverReqComponent reqComps

resolverReqComponent :: GraphQL o => Model.CompKey -> Model.CompReqValues -> Object o GqlType.ReqComponent
resolverReqComponent compKey o' =
  pure $
    GqlType.ReqComponent
      { componentName = pure $ Model.unKey compKey -- :: Model.CompKey -> String!
      , values = resolverCompReqValues o' -- :: Model.CompReqValues ->  CompReqValues!
      }

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
resolverCompReqValues :: GraphQL o => Model.CompReqValues -> Object o GqlType.CompReqValues
resolverCompReqValues (Model.CompReqValues taggedValues) =
    pure $
      GqlType.CompReqValues
        { values =  Just <$> Shared.resolverCompValues (Model.unTag taggedValues)
        , reduced = pure $ Model.isRed taggedValues
        }

---------------------------------------------------------------------------------
-- |
-- Utility function
dedup :: Ord a => [a] -> [a]
dedup = Set.toList . Set.fromList
