-- | Http -> Gql helper functions
module Api.GqlHttp where

import           Control.Monad.IO.Class
import           Protolude              (IO, Symbol, identity, lift, (.))

import           Data.Morpheus.Types    (GQLRequest, GQLResponse)
import           Servant
import           Types

-- | Morpheus helpers
type GQLAPI (name :: Symbol) (version :: Symbol)
  = name     -- ^ endpoint
  :> version -- ^ endpoint
  :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse -- ^ Servant Has Handler

-- | usage: serveGQL ( interpreter rootResolver )
serveGQL :: (GQLRequest -> AppObs GQLResponse)
         -> ServerT (GQLAPI name version) AppObs
serveGQL = identity
