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
    -- > :: Config -> IO ()
    -- [@Config@]: Mostly a placeholder.  Currently holds port number.
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
-- import           Servant.Client.Streaming
-- import qualified Servant.Types.SourceT                as S
--------------------------------------------------------------------------------
import           Api.HTTP.ObsETL                      (ObsEtlApi, serveObsEtlApi)
import           Api.HTTP.GraphiQL                    (GraphiQL, serveGraphiQL)
--------------------------------------------------------------------------------
import           AppTypes                             (Config(..), AppObs,
                                                       Env, mkAppEnv, dbInit, nat)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Cors
-- |
-- Servant min cors policy
corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
         { corsRequestHeaders = [ "content-type" ] }

--------------------------------------------------------------------------------
-- ** Servant API
-- |
-- Servant "has ServerT instance"
--
type Api = ObsEtlApi :<|> GraphiQL

-- |
-- Proxy @Api
apiType :: Proxy Api
apiType = Proxy

-- | Custom monad for serving ObsETL.  Provides handler access to @Env@.
-- > type ServerT api (m :: * -> *) :: *
-- nat :: AppObs -> Handler
--
appM :: ServerT Api AppObs
appM  = serveObsEtlApi :<|> serveGraphiQL

-- import Network.Wai.Middleware.RequestLogger
--------------------------------------------------------------------------------
-- ** Application defined in wai
-- |
--
-- > Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
--
-- serve from Servant.Server
--
-- > serve :: HasServer api '[] => Proxy api -> Server api -> Application
-- > Server api :: ServerT api Handler
--
-- hoistServer :: HasServer api '[]
--             => Proxy api -> (forall x. m x -> n x)
--             -> ServerT api m -> ServerT api n
--
app :: Env -> Application
app env = logStdoutDev . cors ( const $ Just corsPolicy )
        . serve apiType $ hoistServer apiType (nat env) appM
          -- cors ( const $ Just corsPolicy )

-- |
-- Single point of access to the module
exec :: Config -> IO ()
exec cfg = do
  let p = port cfg
  db <- newTVarIO dbInit
  env <- mkAppEnv db cfg
  Warp.run p $ app env

  --
--------------------------------------------------------------------------------
