{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module     : Api.GQL.RequestInput
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
module Api.GQL.RequestInput where
---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import           Api.ETL
import qualified Api.GQL.ObsETL          as Shared
---------------------------------------------------------------------------------
-- import qualified Model.ETL.FieldValues   as Model (FieldValues (..))
import qualified Model.ETL.ObsETL        as Model (CompKey, CompValues,
                                                   Components, MeaKey,
                                                   Measurements, ObsETL (..),
                                                   QualName, QualValues,
                                                   Qualities (..), Subject,
                                                   TagRedExp (..), mkCompKey,
                                                   mkMeaKey, mkQualKey,
                                                   mkSubKey)
import qualified Model.Request           as Model (CompReqValues (..),
                                                   ComponentMixes,
                                                   QualityMix (..),
                                                   ReqComponents (..),
                                                   ReqQualities, Request (..),
                                                   fromListComponentMixes,
                                                   fromListReqComponents,
                                                   fromListReqQualities,
                                                   mkQualityMix)
---------------------------------------------------------------------------------
import qualified Api.GQL.Schemas.Request as GqlInput
import qualified Api.GQL.Schemas.Shared  as GqlInput
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- * Fetch request
-- |
-- GQLInput -> Model.Request
--
-- > input RequestInput
-- >  { subReq: QualityMixInput
-- >  , meaReqs: [ComponentMixInput!]
-- >  }
--
-- fetch from 'Model.ETL.ObsETL' to instantiate 'Model.Request'
--
-- > Request
-- >   { subReq  :: !QualityMix
-- >   , meaReqs :: !ComponentMixes
-- >   } deriving (Show, Eq)
--
fetchRequest :: GqlInput.RequestInput -> Model.ObsETL -> Maybe Model.Request
fetchRequest request etl =
  case request of
    GqlInput.RequestInput maybeSubReq (Just meaReqs) -> do

      -- Subject arm
      subEtl  <- Api.ETL.lookupSubject etl :: Maybe Model.Subject
      subReq  <- maybeSubReq
      subReq' <- fetchQualityMix subReq subEtl

      -- Measurements arm
      meas <- Api.ETL.lookupMeasurements etl
      -- look ahead at ComponentMixInput,
      -- convert measurementTypes [String] to multiple single requests
      -- ComponentMixInput { measurementType: Just Text }
      -- cons the individual requests to [ComponentMixInput]
      let
        -- subset
        subsets :: [GqlInput.ComponentMixInput]
        subsets = filter isSubset  meaReqs

        subsets' :: [(Model.MeaKey, Maybe Model.ReqComponents)]
        subsets'  = catMaybes $ flip fetchComponentMix meas <$> subsets

        -- fullset
        fullsets :: [GqlInput.ComponentMixInput]
        fullsets = mconcat $ traverse ppFullSet meaReqs

        fullsets' :: [(Model.MeaKey, Maybe Model.ReqComponents)]
        fullsets' = catMaybes $ flip fetchComponentMix meas <$> fullsets

        -- reqComponents :: Map CompKey (Maybe CompReqValues)
        meaReqs' = Model.fromListComponentMixes subsets' <>
                   Model.fromListComponentMixes fullsets'

      pure $ Model.Request { subReq  = subReq' :: Model.QualityMix
                           , meaReqs = meaReqs':: Model.ComponentMixes
                           }

    -- for now, default everything else to Nothing
    GqlInput.RequestInput _ _ -> Nothing

  where
    -- full set requests
    ppFullSet :: GqlInput.ComponentMixInput -> [GqlInput.ComponentMixInput]
    ppFullSet (GqlInput.ComponentMixInput (Just keys) Nothing (Just _)) =
      ppFullSet (GqlInput.ComponentMixInput (Just keys) Nothing Nothing)

    ppFullSet (GqlInput.ComponentMixInput (Just keys) maybeKey Nothing) =
      let keys' = case maybeKey of
                      Just k  -> k:keys
                      Nothing -> keys
      in
        fmap (\key ->
          GqlInput.ComponentMixInput
                       { measurementTypes = Nothing
                       , measurementType  = Just key
                       , componentMix = Nothing
                       }
         ) keys'
    ppFullSet _ = [] -- not a fullset request

    -- subset requests
    isSubset :: GqlInput.ComponentMixInput -> Bool
    isSubset (GqlInput.ComponentMixInput Nothing (Just _) (Just _)) = True
    isSubset _                                                      = False

---------------------------------------------------------------------------------
  -- Measurements arm
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
fetchComponentMix :: GqlInput.ComponentMixInput -> Model.Measurements
                  -> Maybe (Model.MeaKey, Maybe Model.ReqComponents)

fetchComponentMix (GqlInput.ComponentMixInput (Just v) _ _) _ =
  panic $ "Cannot process multiple meaKeys simultaneously here; " <>
          "the request was not properly pre-processed" <> show v
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
fetchComponentMix (GqlInput.ComponentMixInput Nothing (Just meaKey) Nothing) etl = do
  meaKey' <- lookupMeasurementType (Model.mkMeaKey meaKey) etl
  pure (meaKey', Nothing)

-- |
-- Second Request type.
-- Subset of values for each of the components specified.
fetchComponentMix (GqlInput.ComponentMixInput Nothing (Just meaKey) (Just compReqs)) etl = do
  -- consume the request
  -- get the measurement from etl data
  let meaKey' = Model.mkMeaKey meaKey
  etl' <- Api.ETL.lookupComponents meaKey' etl

  -- make several
  -- fullsets :: [ComponentReqInput]
  let subsets  = traverse ppSubset  compReqs
  let fullsets = traverse ppFullSet compReqs

  -- fullsets' :: [ReqComponents]
  let subsets'  = mconcat . catMaybes $ flip fetchComponents etl' <$> subsets
  let fullsets' = mconcat . catMaybes $ flip fetchComponents etl' <$> fullsets

  -- left bias
  let compMix' = subsets' <> fullsets' :: Model.ReqComponents

  pure (meaKey', Just compMix' :: Maybe Model.ReqComponents)

  where
    -- full set requests
    ppFullSet :: GqlInput.ComponentReqInput -> [GqlInput.ComponentReqInput]
    ppFullSet (GqlInput.ComponentReqInput (Just keys) _ (Just _)) =
      ppFullSet (GqlInput.ComponentReqInput (Just keys) Nothing Nothing)

    ppFullSet (GqlInput.ComponentReqInput (Just keys) maybeKey Nothing) =
      let keys' = case maybeKey of
                      Just k  -> k:keys
                      Nothing -> keys
      in
        fmap (\key -> GqlInput.ComponentReqInput
            { componentNames   = Nothing  :: Maybe [Text]
            , componentName    = Just key :: Maybe Text
            , componentValues  = Nothing  :: Maybe GqlInput.FieldValuesCompReqInput
            } :: GqlInput.ComponentReqInput
         ) keys'
    ppFullSet req = [req] -- return untouched wrapped in list

    -- subset requests
    ppSubset :: GqlInput.ComponentReqInput -> [GqlInput.ComponentReqInput]
    ppSubset (GqlInput.ComponentReqInput Nothing (Just key) (Just vs)) =
             [GqlInput.ComponentReqInput Nothing (Just key) (Just vs)]
    ppSubset _ = []

fetchComponentMix GqlInput.ComponentMixInput {} _ = panic "malformed query"

type Name = Text

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
fetchComponents :: [GqlInput.ComponentReqInput]
                -> Model.Components -> Maybe Model.ReqComponents
fetchComponents requests etl =
   -- ppSubsetting :: [SetReqInput] -> ([SubsetReq], [FullSetReq])
  let
      (subSetReqs, fullSetReqs) = mapTuple (catMaybes . (fmap unWrapComp))
                                $ ppSubsetting (Comp <$> requests)

      fullSetReqs' :: [(Model.CompKey, Maybe Model.CompReqValues)]
      fullSetReqs' = mapMaybe (`fetchFullSet` etl) fullSetReqs

      subSetReqs' :: [(Model.CompKey, Maybe Model.CompReqValues)]
      subSetReqs' = mapMaybe (`fetchSubset` etl) subSetReqs

   in
      Just $
        Model.fromListReqComponents subSetReqs' <>
        Model.fromListReqComponents fullSetReqs'

  -- Model.fromListReqComponents [(CompKey, CompReqValues)] -> ReqComponents
  where
    -- filterCompReqValues :: CompReqValues -> FieldValues -> Maybe CompReqValues
    fetchSubset :: GqlInput.ComponentReqInput
                -> Model.Components
                -> Maybe (Model.CompKey, Maybe Model.CompReqValues)
    fetchSubset req etl' = do
      (compKey, compValues) <- getEtlFragment req etl'
      -- request: extract and prep the requested field values
      compReq <- GqlInput.componentValues req :: Maybe GqlInput.FieldValuesCompReqInput
      let compReq' = fromInputCompReqValues  compReq

      -- fetch field values
      (compKey,) . Just <$> filterCompReqValues compReq' compValues

    -- at this point, fullset implies Exp (create a series)
    -- otherwise, there is no values in subsetting, i.e., making this request
    -- We are displaying what the request would look like.
    -- What does it mean if did not specify details? => Expressed
    -- /Note/: Even when the user specifies every value, the computation
    -- will get to this place with a specified TagRedExp. So, that will not
    -- be confused with what we infer.
    fetchFullSet :: GqlInput.ComponentReqInput
                 -> Model.Components
                 -> Maybe (Model.CompKey, Maybe Model.CompReqValues)
    fetchFullSet req etl' =
      fmap (Just . Model.CompReqValues . Model.Exp) <$> getEtlFragment req etl'

    -----------------------------------------------------------------------------
    getEtlFragment :: GqlInput.ComponentReqInput
                  -> Model.Components
                  -> Maybe (Model.CompKey, Model.CompValues)
    getEtlFragment req etl'' = do
      compName <- getCompName req :: Maybe Text
      let compKey = Model.mkCompKey compName
      (compKey,) <$> lookupCompValues compKey etl''

    -----------------------------------------------------------------------------
    getCompName :: GqlInput.ComponentReqInput -> Maybe Text
    getCompName GqlInput.ComponentReqInput {..} = componentName


---------------------------------------------------------------------------------
  -- Qualities arm
---------------------------------------------------------------------------------
-- |
-- GQLInput -> Model.Request
-- GQLInput specified by the schema.request.graphql
-- Model.Request 'Model.Request'
--
-- In the event the `qualityMix` field is empty, we are done.
--
-- /Recall/: The input is a search term that filters out ETL data.
-- It is not valid etl data unto itself.
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
fetchQualityMix :: GqlInput.QualityMixInput -> Model.Subject -> Maybe Model.QualityMix
fetchQualityMix (GqlInput.QualityMixInput subTypeReq mixReq) etl = do

    -- retrieve etl subject
    let subKey = Model.mkSubKey subTypeReq
    etl' <- Api.ETL.lookupQualities subKey etl
    mixReq' <- mixReq

    -- propogate the request to child requests
    let fullSetReqs = mconcat $ traverse ppFullSets mixReq' :: [GqlInput.QualityReqInput]
    let subSetReqs  = filter isSubset mixReq'               :: [GqlInput.QualityReqInput]

    fullSets' <- fetchQualities fullSetReqs etl' :: Maybe Model.ReqQualities
    subsets'  <- fetchQualities subSetReqs etl'  :: Maybe Model.ReqQualities

    -- instantiate the Model.Request
    pure $ Model.mkQualityMix subKey (subsets' <> fullSets')

    -- for values in `QualityReqInput { qualityNames }` preProcess to create
    -- a list of `QualityReqInput { qualityName }`.
    where
      -- full set request has zero filtering capacity BUT, does mean the
      -- quality field will be included in the Matrix request.
      -- ... so include it in what we echo, augmented.
      ppFullSets :: GqlInput.QualityReqInput -> [GqlInput.QualityReqInput]
      ppFullSets (GqlInput.QualityReqInput (Just keys) Nothing (Just _)) =
        ppFullSets (GqlInput.QualityReqInput (Just keys) Nothing Nothing)

      ppFullSets (GqlInput.QualityReqInput (Just keys) maybeKey Nothing) =
        let keys' = case maybeKey of
                       Just n  -> n:keys
                       Nothing -> keys
        in
          fmap (\key ->
            GqlInput.QualityReqInput
                            { qualityNames  = Nothing
                            , qualityName   = Just key
                            , qualityValues = Nothing
                            }
           ) keys'
      ppFullSets _ = [] -- not a fullset request

      -- subset requests
      isSubset :: GqlInput.QualityReqInput -> Bool
      isSubset (GqlInput.QualityReqInput Nothing (Just _) (Just _)) = True
      isSubset _                                                    = False


fetchQualities :: [GqlInput.QualityReqInput]
                -> Model.Qualities -> Maybe Model.ReqQualities
fetchQualities requests etl =
  let
      -- wrap, process using shared logic, unwrap
      (subSetReqs, fullSetReqs) = mapTuple (catMaybes . (fmap unWrapQual))
                                $ ppSubsetting (Qual <$> requests)

      fullSetReqs' :: [(Model.QualName, Maybe Model.QualValues)]
      fullSetReqs' = mapMaybe (`fetchFullSet` etl) fullSetReqs

      subSetReqs' :: [(Model.QualName, Maybe Model.QualValues)]
      subSetReqs' = mapMaybe (`fetchSubset` etl) subSetReqs

   in
      Just $
        Model.fromListReqQualities subSetReqs' <>
        Model.fromListReqQualities fullSetReqs'

  -- Model.fromListReqQualities [(QualName, Maybe QualValues)] -> ReqQualities
  where
    -- filterCompReqValues :: CompReqValues -> FieldValues -> Maybe CompReqValues
    fetchSubset :: GqlInput.QualityReqInput
                -> Model.Qualities
                -> Maybe (Model.QualName, Maybe Model.QualValues)
    fetchSubset req etl' = do
      (qualKey, qualValues) <- getEtlFragment req etl'  :: Maybe (Model.QualName, Model.QualValues)
      -- extract and prep the req field values
      qualReq <- GqlInput.qualityValues req             :: Maybe GqlInput.QualValuesInput
      let qualReq' = Shared.fromInputQualValues qualReq :: Model.QualValues
      -- fetch field values
      values <- filterQualReqValues qualReq' qualValues :: Maybe Model.QualValues
      pure (qualKey, Just values)
      -- TODO: Log message when Nothing came back when something was requested.
      --       No records should be returned if asked for quality values that
      --       all do not exist.

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
    fetchFullSet :: GqlInput.QualityReqInput
                 -> Model.Qualities
                 -> Maybe (Model.QualName, Maybe Model.QualValues)
    fetchFullSet req etl' = do
      qualName <- getQualName req
      let qualKey = Model.mkQualKey qualName
      etlKey <- lookupQualityKey qualKey etl'
      pure ( etlKey, Nothing )

    -----------------------------------------------------------------------------
    getEtlFragment :: GqlInput.QualityReqInput
                  -> Model.Qualities
                  -> Maybe (Model.QualName, Model.QualValues)
    getEtlFragment req etl'' = do
      qualName <- getQualName req :: Maybe Text  -- hack to avoid namespace collision
      let qualKey = Model.mkQualKey qualName
      (qualKey,) <$> lookupQualValues qualKey etl''
    -----------------------------------------------------------------------------
    getQualName :: GqlInput.QualityReqInput -> Maybe Text
    getQualName GqlInput.QualityReqInput {..} = qualityName

---------------------------------------------------------------------------------
-- |
--
fromInputCompReqValues :: GqlInput.FieldValuesCompReqInput -> Model.CompReqValues
fromInputCompReqValues (GqlInput.FieldValuesCompReqInput txt int span red)
    | red       = Model.CompReqValues . Model.Red $ go (GqlInput.CompValuesInput txt int span)
    | otherwise = Model.CompReqValues . Model.Exp $ go (GqlInput.CompValuesInput txt int span)
    where go = Shared.fromInputCompValues


---------------------------------------------------------------------------------
-- |
-- Unifying function to reduce boilerplate
-- Utilities for 'fetchQualities' and 'fetchReqComponents'
--
data SetReqInput
  = Comp GqlInput.ComponentReqInput
  | Qual GqlInput.QualityReqInput

type FullSetReq = SetReqInput
type SubsetReq  = SetReqInput

ppSubsetting :: [SetReqInput] -> ([SubsetReq], [FullSetReq])
ppSubsetting requests =
  if any raiseError requests
     then panic $ "Cannot process multiple component names simultaneously here; " <>
                  "the request was not properly pre-processed"
     else
       let fullSetReqs = filter isFullSet requests
           subSetReqs  = filter (not . isFullSet) requests
        in (subSetReqs, fullSetReqs)

  where
    raiseError :: SetReqInput -> Bool
    raiseError (Comp GqlInput.ComponentReqInput { componentNames = Just _ }) = True
    raiseError (Qual GqlInput.QualityReqInput { qualityNames = Just _ }) = True
    raiseError _ = False

    isFullSet :: SetReqInput -> Bool
    isFullSet (Comp GqlInput.ComponentReqInput
      { componentName = Just _, componentValues = Nothing }) = True
    isFullSet (Qual GqlInput.QualityReqInput
      { qualityNames  = Just _, qualityValues   = Nothing }) = True
    isFullSet _ = False

unWrapComp :: SetReqInput -> Maybe GqlInput.ComponentReqInput
unWrapComp (Comp v) = Just v
unWrapComp (Qual _) = Nothing

unWrapQual :: SetReqInput -> Maybe GqlInput.QualityReqInput
unWrapQual (Qual v) = Just v
unWrapQual (Comp _) = Nothing

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)
