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

import           Control.Concurrent.STM.TVar          (newTVarIO)
import           Protolude                            hiding (State)
--------------------------------------------------------------------------------
import           Network.Wai
import qualified Network.Wai.Handler.Warp             as W
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Servant
--------------------------------------------------------------------------------
import           Api.HTTP.GraphiQL
import           Api.HTTP.ObsETL
import           Api.HTTP.ObsTest
import           AppTypes

--------------------------------------------------------------------------------
-- | Servant min cors policy
corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
         { corsRequestHeaders = [ "content-type" ] }
--------------------------------------------------------------------------------
-- | Servant has ServerT instance
type Api = ObsTest :<|> ObsEtl :<|> GraphiQL

-- | Proxy @Api
apiType :: Proxy Api
apiType = Proxy

-- | Custom monad for serving ObsETL.  Provides handler access to @Env@.
appM :: ServerT Api AppObs
appM  = serveObsTest :<|> serveObsEtl :<|> serveGraphiQL

--------------------------------------------------------------------------------
-- import Network.Wai.Middleware.RequestLogger
--
-- | Application from wai
-- > Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
--
-- serve from Servant.Server
-- > serve :: HasServer api '[] => Proxy api -> Server api -> Application
-- > Server api :: ServerT api Handler
--
app :: Env -> Application
app env = logStdout . cors ( const $ Just corsPolicy )
        . serve apiType $ hoistServer apiType (nat env) appM

-- | Single point of access to the module
exec :: AppConfig -> IO ()
exec config = do
  let p = port config
  db <- newTVarIO dbInit
  W.run p $ app (Env db Nothing config)
