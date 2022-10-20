{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Api.HTTP.HealthCheck
-- Description : Useful in distributed setting
--
module Api.HTTP.HealthCheck (HealthCheckApi , serveHealthCheckApi)
    where
--

--------------------------------------------------------------------------------
import           Protolude           hiding (null)
--------------------------------------------------------------------------------
import           Servant
import           Data.Aeson          (ToJSON)
--------------------------------------------------------------------------------
import           AppTypes            (AppObs)
--------------------------------------------------------------------------------
--
-- Endpoint type constructor
--
type TypeBuilder (version :: Symbol) (name :: Symbol)
  = version     -- endpoint
  :> name       -- endpoint
  :> Post '[JSON] HealthCheckResponse -- Servant Has Handler

newtype HealthCheckResponse = HealthCheckResponse Text deriving (Generic)

instance ToJSON HealthCheckResponse

-- |
-- == Endpoint type
-- Servant Has Server types
--
-- Applicatoin of the type constructor. Is the first parameter in the
-- ServerT construction (concrete application).
--
type HealthCheckApi  = TypeBuilder "v1" "livez"

api :: AppObs HealthCheckResponse
api = pure $ HealthCheckResponse "ok"

serveApi :: AppObs HealthCheckResponse -> ServerT (TypeBuilder version name) AppObs
serveApi = identity

-- |
-- == Handlers
-- Servant Has Handler
-- > type ServerT api (m :: * -> *) :: *
--
serveHealthCheckApi :: ServerT HealthCheckApi AppObs
serveHealthCheckApi = serveApi api

-- END
