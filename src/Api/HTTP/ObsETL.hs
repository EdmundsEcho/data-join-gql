-- |
-- Module      : Api.HTTP.ObsEtl
-- Description : The endpoint for the UI
--
module Api.HTTP.ObsETL (ObsEtl , serveObsEtl)
    where

--------------------------------------------------------------------------------
import           Data.Morpheus       (Interpreter (..))
import           Data.Morpheus.Types
import           Servant
--------------------------------------------------------------------------------
import           Api.GQL.Root        (gqlRoot)
import           Api.GqlHttp
import           AppTypes
--------------------------------------------------------------------------------
-- |
-- == Endpoint type
-- Servant Has Server types
type ObsEtl  = GQLAPI "obsetl" "v1"

api :: GQLRequest -> AppObs GQLResponse
api = interpreter gqlRoot

-- |
-- == Handlers
-- Servant Has Handler
serveObsEtl :: ServerT ObsEtl AppObs
serveObsEtl = serveGQL api
