{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Api.GQL.Schemas.Shared where

---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import           Data.Aeson             (FromJSON, ToJSON)
---------------------------------------------------------------------------------
import           Data.Morpheus.Document (importGQLDocument)
-- import Api.GQL.ObsETL (QualValuesInput)
---------------------------------------------------------------------------------
importGQLDocument "src/Api/GQL/Schemas/schema.shared.graphql"
---------------------------------------------------------------------------------
  --

---------------------------------------------------------------------------------
-- debugging support
-- types specified in the schema
--
instance ToJSON QualValuesInput
instance ToJSON CompValuesInput
instance ToJSON SpanInput

instance FromJSON QualValuesInput
instance FromJSON CompValuesInput
instance FromJSON SpanInput

---------------------------------------------------------------------------------
