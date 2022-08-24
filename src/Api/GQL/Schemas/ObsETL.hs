{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Api.GQL.Schemas.ObsETL where

---------------------------------------------------------------------------------
import           Protolude
import           Data.Aeson             (FromJSON)
---------------------------------------------------------------------------------
import           Data.Morpheus.Document (importGQLDocument)
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.Shared
importGQLDocument "src/Api/GQL/Schemas/schema.obsetl.graphql"
---------------------------------------------------------------------------------
--
instance FromJSON ObsEtlInput
instance FromJSON SubjectInput
instance FromJSON QualityInput
instance FromJSON MeasurementInput
instance FromJSON ComponentInput
