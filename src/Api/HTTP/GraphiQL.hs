module Api.HTTP.GraphiQL (GraphiQL , serveGraphiQL)
    where

--------------------------------------------------------------------------------
import           Data.Morpheus (Interpreter (..))
--------------------------------------------------------------------------------
import           Servant
--------------------------------------------------------------------------------
import           Api.GQL.Root  (gqlRoot)
import           Api.GqlHttp
import           AppTypes
--------------------------------------------------------------------------------

-- Servant Has Server types
type GraphiQL
  =     ObsAPI
  :<|>  Graphi

-- Types that map to specific handlers
-- The endpoints are one in the same.
type ObsAPI = GQLAPI "graphiql" "v1"
type Graphi =  "graphiql" :> "v1" :> ServeGraphi

-- Servant Has Handler types
-- type ServeGql = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
type ServeGraphi = Raw


-- Export Handler
serveGraphiQL :: ServerT GraphiQL AppObs
serveGraphiQL = serveObsAPI :<|> serveGraphi
  where
    -- gql server (to which graphi posts requests)
    serveObsAPI :: ServerT ObsAPI AppObs
    serveObsAPI = serveGQL (interpreter gqlRoot)

    -- static file server
    serveGraphi :: ServerT Graphi AppObs
    serveGraphi = serveDirectoryFileServer "assets/"
