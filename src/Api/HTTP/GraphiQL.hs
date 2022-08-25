module Api.HTTP.GraphiQL (GraphiQL , serveGraphiQL)
    where

--------------------------------------------------------------------------------
import           Servant
import           AppTypes
--------------------------------------------------------------------------------

-- Servant Has Server types
-- type GraphiQL = ObsAPI :<|>  Graphi
type GraphiQL = Graphi

-- Types that map to specific handlers
-- The endpoints are one in the same.
-- type ObsAPI = GQLAPI "v1" "warehouse" "projectId"
type Graphi =  "v1" :> "graphiql" :> ServeGraphi

-- Servant Has Handler types
-- type ServeGql = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
type ServeGraphi = Raw


-- Export Handler
serveGraphiQL :: ServerT GraphiQL AppObs
serveGraphiQL = serveGraphi
-- serveGraphiQL = serveObsAPI :<|> serveGraphi
  where
    -- gql server (to which graphi posts requests)
    -- serveObsAPI :: ServerT ObsAPI AppObs
    -- serveObsAPI = serveGQL (interpreter gqlRoot)

    -- static file server
    serveGraphi :: ServerT Graphi AppObs
    serveGraphi = serveDirectoryFileServer "assets/"
