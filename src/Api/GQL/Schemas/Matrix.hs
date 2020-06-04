{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Api.GQL.Schemas.Matrix
  ( module Api.GQL.Schemas.Matrix
  ) where
---------------------------------------------------------------------------------
import           Protolude              hiding (filter)
---------------------------------------------------------------------------------
import           Data.Aeson             (ToJSON)
---------------------------------------------------------------------------------
import           Data.Morpheus.Document (importGQLDocument)
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.Shared
importGQLDocument "src/Api/GQL/Schemas/schema.matrix.graphql"
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- debugging support
-- types specified in the schema
--
-- instance ToJSON Expression
-- instance ToJSON FieldName
-- instance ToJSON Filter
-- instance ToJSON FilterRange
-- instance ToJSON FilterValue
-- instance ToJSON Matrix
-- instance ToJSON QualMxValue
-- instance ToJSON Reducer
-- instance ToJSON Relation
-- instance ToJSON RelSymbol
-- instance ToJSON RelSymbolEnum
---------------------------------------------------------------------------------
  --
