{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Api.GQL.Schemas.MatrixSpec
  ( module Api.GQL.Schemas.MatrixSpec
  ) where
---------------------------------------------------------------------------------
import           Protolude              hiding (filter)
---------------------------------------------------------------------------------
import           Data.Morpheus.Document (importGQLDocument)
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.Shared
importGQLDocument "src/Api/GQL/Schemas/schema.matrix.graphql"
---------------------------------------------------------------------------------
