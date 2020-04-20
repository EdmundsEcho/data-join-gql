{-# LANGUAGE PartialTypeSignatures #-}

module Api.HTTP.GraphiQL (GraphiQL , serveGraphiQL)
    where

import           Data.Morpheus (Interpreter (..))
import           Servant

import           Api.GQL.Root  (gqlRoot)
import           Api.GqlHttp
import           AppTypes


-- Servant Has Server types
type GraphiQL
  =     ObsAPI
  :<|>  Graphi

-- Types that map to specific handlers
type ObsAPI = GQLAPI "graphiql" "v1"
type Graphi =  "graphiql" :> "v1" :> ServeGraphi

-- Servant Has Handler types
-- type ServeGql = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
type ServeGraphi = Raw

-- Export Handler
serveGraphiQL :: ServerT GraphiQL AppObs
serveGraphiQL = serveObsAPI :<|> serveGraphi
  where
    serveObsAPI :: ServerT ObsAPI AppObs
    serveObsAPI = serveGQL (interpreter gqlRoot)

    serveGraphi :: ServerT Graphi AppObs
    serveGraphi = serveDirectoryFileServer "assets/"
