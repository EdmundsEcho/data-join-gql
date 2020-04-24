
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Api.GQL.Schemas where

---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
-- import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Document (importGQLDocument)
---------------------------------------------------------------------------------
importGQLDocument "src/Api/GQL/schema.shared.graphql"
importGQLDocument "src/Api/GQL/schema.obsetl.graphql"
importGQLDocument "src/Api/GQL/schema.request.graphql"
---------------------------------------------------------------------------------
  --
