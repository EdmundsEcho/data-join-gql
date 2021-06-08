{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Api.GQL.Schemas.Root where

---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import           Data.Morpheus.Document (importGQLDocument)
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.ObsETL
import           Api.GQL.Schemas.MatrixSpec
import           Api.GQL.Schemas.Request
import           Api.GQL.Schemas.Levels
---------------------------------------------------------------------------------
importGQLDocument "src/Api/GQL/Schemas/schema.root.graphql"
---------------------------------------------------------------------------------
