-- |
-- Module      : App
-- Description : The WebApp that is hoisted into Servant
-- Copyright   : (c) Lucivia, LLC 2020
-- License     :
-- Maintainer  : edmund@lucivia.com
-- Stability   : experimental
-- Portability : POSIX

-- The App uses a custom monad to enable use of state. The app is hoisted
-- into Servant.
--
module App
  ( -- * single export
    --
    -- > :: AppConfig -> IO ()
    -- [@AppConfig@]: Mostly a placeholder.  Currently holds port number.
    --
    exec
  ) where

--------------------------------------------------------------------------------
import           Control.Concurrent.STM.TVar          (newTVarIO)
import           Protolude                            hiding (State)
--------------------------------------------------------------------------------
import           Network.Wai
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Servant
--------------------------------------------------------------------------------
import           Api.HTTP.GraphiQL
import           Api.HTTP.ObsETL
import           Api.HTTP.ObsTest
import           AppTypes                             (AppConfig (..), AppObs,
                                                       Env (..), dbInit, nat)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | Servant min cors policy
corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
         { corsRequestHeaders = [ "content-type" ] }
--------------------------------------------------------------------------------
-- | Servant has ServerT instance
type Api = ObsTest :<|> ObsEtlApi :<|> GraphiQL

-- | Proxy @Api
apiType :: Proxy Api
apiType = Proxy

-- | Custom monad for serving ObsETL.  Provides handler access to @Env@.
-- > type ServerT api (m :: * -> *) :: *
-- nat :: AppObs -> Handler
--
appM :: ServerT Api AppObs
appM  = serveObsTest :<|> serveObsEtlApi :<|> serveGraphiQL

--------------------------------------------------------------------------------
-- import Network.Wai.Middleware.RequestLogger
--
-- | Application from wai
-- > Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
--
-- serve from Servant.Server
-- > serve :: HasServer api '[] => Proxy api -> Server api -> Application
-- > Server api :: ServerT api Handler
-- hoistServer :: HasServer api '[]
--             => Proxy api -> (forall x. m x -> n x)
--             -> ServerT api m -> ServerT api n
--
app :: Env -> Application
app env = logStdout . cors ( const $ Just corsPolicy )
        . serve apiType $ hoistServer apiType (nat env) appM

-- | Single point of access to the module
exec :: AppConfig -> IO ()
exec config = do
  let p = port config
  db <- newTVarIO dbInit
  Warp.run p $ app (Env db Nothing config)

-- nat env :: Env -> AppObs a -> Handler a
-- appM :: ServerT Api AppObs
-- hoistServer :: Proxy api
--             -> (forall x. m x -> n x) -> ServerT api m -> ServerT api n
-- serve :: Server api -> Application


  --
--------------------------------------------------------------------------------
