{-# LANGUAGE PolyKinds #-}
-- |
-- Module      : Api.GqlHttp
-- Description : Bridge between Servant, GraphQL and the WebApp types
--
module Api.GqlHttp
  where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           AppTypes            (AppObs)
import           Servant
-------------------------------------------------------------------------------
import           Data.Morpheus.Types (GQLRequest, GQLResponse)
-------------------------------------------------------------------------------
  --
-- == > Http -> Gql helper functions
-- |
-- Morpheus helpers
--
type GQLAPI (version :: Symbol) (name :: Symbol) (projectId :: Symbol)
  = version     -- endpoint
  :> name       -- endpoint
  :> projectId  -- endpoint
  :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse -- Servant Has Handler

type GQLTest (version :: Symbol) (name :: Symbol)
  = version     -- endpoint
  :> name       -- endpoint
  :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse -- Servant Has Handler

-- == Gql endpoint type
-- |
-- Usage
--
--   > serveGQL ( interpreter rootResolver )
--   later,
--   serveObsEtlApi :: ServerT ObsEtlApi AppObs
--   serveObsEtlApi = serveGQL api
--
--   api :: GQLRequest -> AppObs GQLResponse
--   api = interpreter gqlRoot
--
serveGQL :: (GQLRequest -> AppObs GQLResponse)
         -> ServerT (GQLAPI version name projectId) AppObs
serveGQL = identity


---------------------------------------------------------------------------------
  --
