{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Api.GQL.Schemas.Request
  ( module Api.GQL.Schemas.Request
  , QualityValues -- type annotation
  ) where

---------------------------------------------------------------------------------
import           Protolude              hiding (Meta)
---------------------------------------------------------------------------------
import           Data.Aeson             (ToJSON)
---------------------------------------------------------------------------------
import           Data.Morpheus.Document (importGQLDocument)
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.Shared
importGQLDocument "src/Api/GQL/Schemas/schema.request.graphql"
---------------------------------------------------------------------------------

-- |
-- Utilized to augment a request when required
-- Only Subject can have some sort of default because there is only
-- one instance of Subject. Furthermore, without a Subject no records
-- would be included in the request (strictly speaking).
--
minQualityMixInput :: Text -> QualityMixInput
minQualityMixInput subType =
  QualityMixInput { subjectType = Just subType
                  , qualityMix  = Nothing
                  }

class RequestKey request where
  requestKey :: request -> Maybe Text

class ToFullsetReq request fullReq | request -> fullReq where
  toFullsetReq :: request -> fullReq

class ToSubsetReq request subReq | request -> subReq where
  toSubsetReq :: request -> subReq

-- |
-- Mixed Request is a request that encodes both a subset and
-- fullset request.  A subset request encodes a filter that
-- describes a subset of the ETL data. The typeclass defines
-- two functions that parse the requests accordingly.
--
class ( ToFullsetReq request fullReq
      , ToSubsetReq  request subReq
      , RequestKey   request )
      => MixedRequest request fullReq subReq where
  requestKeys :: request -> Maybe [Text]
  areValues   :: request -> Bool
  setKey      :: request -> Maybe Text -> request

  -- |
  -- Processes fullset requests
  --
  -- full set request has zero filtering capacity BUT, does mean the
  -- key will be included as follows in the Matrix request.
  -- * Quality -> field
  -- * Measurement -> field
  -- * Component -> series of fields
  --
  -- > { keys   = Just keys
  -- >   key    = Just k
  -- >   values = Nothing
  -- > }
  --
  -- converts to a collection k:keys of
  --
  -- > { keys   = Nothing
  -- >   key    = Just k
  -- >   values = Nothing
  -- > }
  --
  -- /Note/: subsets are processed separately.
  --
  fullsets  :: request -> [fullReq]
  fullsets req =
    let keys' =
          case (requestKeys req, requestKey req, areValues req) of
            (Just ks, Just k, False) -> k:ks
            (Just ks, Just _, True)  -> ks
            (Just ks, Nothing, _)    -> ks
            (Nothing, Just k, False) -> [k]
            (Nothing, Just _, True)  -> []   -- subset only
            (Nothing, Nothing, _)    -> []   -- malformed
    in
       toFullsetReq . setKey req . Just <$>  keys'

  subsets   :: request -> [subReq]
  subsets req =
    case (requestKey req, areValues req) of
      (Just _, True) -> [toSubsetReq req]
      (_,_)          -> []

---------------------------------------------------------------------------------
instance RequestKey QualityMixInput where
  requestKey = subjectType

---------------------------------------------------------------------------------
instance RequestKey QualityReqInput where
  requestKey = qualityName

instance MixedRequest QualityReqInput
         FullsetRequest  SubsetQualReq where
  requestKeys = qualityNames
  areValues = isJust . qualityValues
  setKey req k = req { qualityName = k }

instance ToFullsetReq QualityReqInput FullsetRequest where
  toFullsetReq QualityReqInput { qualityName } = case qualityName of
    Just k  -> FullsetRequest k
    Nothing -> panic "Tried to constuct a fullset request without a key"

instance ToSubsetReq QualityReqInput SubsetQualReq where
  toSubsetReq QualityReqInput { qualityName, qualityValues } =
    case (qualityName, qualityValues) of
      (Just k, Just vs) -> SubsetQualReq k vs
      (_,_) -> panic "Tried to make a subset request without both key and values"

---------------------------------------------------------------------------------
instance RequestKey ComponentMixInput where
  requestKey = measurementType

instance MixedRequest ComponentMixInput
         FullsetRequest SubsetCompMixReq where
  requestKeys  = measurementTypes
  setKey req k = req { measurementType  = k }
  areValues    = isJust . componentMix'
      where componentMix' ComponentMixInput {componentMix} = componentMix

instance ToFullsetReq ComponentMixInput FullsetRequest where
  toFullsetReq ComponentMixInput { measurementType } = case measurementType of
    Just k  -> FullsetRequest k
    Nothing -> panic "Tried to constuct a fullset request without a key"

instance ToSubsetReq ComponentMixInput SubsetCompMixReq where
  toSubsetReq ComponentMixInput { measurementType, componentMix } =
    case (measurementType, componentMix) of
      (Just k, Just vs) -> SubsetCompMixReq k vs
      (_,_) -> panic "Tried to make a subset request without both key and values"

---------------------------------------------------------------------------------
instance RequestKey ComponentReqInput where
  requestKey = componentName

instance MixedRequest ComponentReqInput
         FullsetRequest SubsetCompReq where
  requestKeys  = componentNames
  areValues    = isJust . componentValues
  setKey req k = req { componentName = k }

instance ToFullsetReq ComponentReqInput FullsetRequest where
  toFullsetReq ComponentReqInput { componentName } = case componentName of
    Just k  -> FullsetRequest k
    Nothing -> panic "Tried to constuct a fullset request without a key"

instance ToSubsetReq ComponentReqInput SubsetCompReq where
  toSubsetReq ComponentReqInput { componentName, componentValues } =
    case (componentName, componentValues) of
      (Just k, Just vs) -> SubsetCompReq k vs
      (_,_) -> panic "Tried to make a subset request without both key and values"

---------------------------------------------------------------------------------
-- # Intermediary objects
-- Output of MixedRequest typeclass
--
-- Subset for subset requests
-- Fullset for fullset requests
--
-- input -> subset  -> fetch -> view
-- input -> fullset -> fetch -> view
--
-- |
-- Subset for Subject
-- There is only ever at most one for each request.
-- The source = Request { subReq }
--
data SubsetQualReq = SubsetQualReq {
      subKey          :: !Text
    , qualValuesInput :: !QualValuesInput
} deriving (Show, Generic)

-- |
-- Subset for a Measurement
-- There can be several for each request.
-- The source = Request { meaReqs }
--
data SubsetCompMixReq = SubsetCompMixReq {
      meaKey       :: !Text
    , compReqInput :: ![ComponentReqInput]
} deriving (Show, Generic)

instance RequestKey SubsetCompMixReq where
  requestKey SubsetCompMixReq { meaKey } = Just meaKey

instance RequestKey SubsetQualReq where
  requestKey SubsetQualReq { subKey } = Just subKey

data SubsetCompReq = SubsetCompReq {
      compKey         :: !Text
    , compValuesInput :: !CompValuesReqInput
} deriving (Show, Generic)

instance RequestKey SubsetCompReq where
  requestKey SubsetCompReq { compKey } = Just compKey


--
-- """
-- Fullset action
--
-- Subject     -> min viable request; no quality fields (key without mix)
--             -> Etl action: nothing (nothing to filter)
--
-- Quality     -> display the field; no impact on records requested
--             -> Etl action: nothing (nothing to filter)
--
-- Measurement -> display the field; Red for all components (default)
--             -> Etl action: nothing (Reduction is the default)
--
-- Component   -> display a series of fields (so Exp); Red is encoded by absence
--             -> Etl action: pull the EtlFragment required to generate the
--                Exp series
--
-- """
newtype FullsetRequest = FullsetRequest {
  key :: Text
} deriving (Show, Generic)

instance RequestKey FullsetRequest where
  requestKey FullsetRequest { key } = Just key


---------------------------------------------------------------------------------
-- debugging support
-- types specified in the schema
--
instance ToJSON SubsetQualReq
instance ToJSON SubsetCompReq
instance ToJSON SubsetCompMixReq
instance ToJSON FullsetRequest

instance ToJSON QualityMixInput
instance ToJSON QualityReqInput
instance ToJSON ComponentMixInput
instance ToJSON ComponentReqInput
instance ToJSON CompValuesReqInput

---------------------------------------------------------------------------------
  --
