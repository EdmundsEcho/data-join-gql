-- {-# LANGUAGE AllowAmbiguousTypes   #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

module Api.HTTP.ObsAPI (ObsAPI , serveObsAPI)
    where

import           Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy.Char8 as B
import           Protolude

import           Data.Morpheus              (Interpreter (..))
import           Data.Morpheus.Types
import           Servant

import           Api.GQL.ObsTest            (gqlRoot)
import           Api.GqlHttp
import           Types


-- Has Server types
type ObsAPI  = GQLAPI "obsetl" "v1"

api :: GQLRequest -> AppObs GQLResponse
api = interpreter gqlRoot
-- AppObs _what

-- Has Handler types
serveObsAPI :: ServerT ObsAPI AppObs
serveObsAPI = serveGQL api

--
-- interpreter :: (Monad m, RootResCon m e query mut sub)
--             => GQLRootResolver m e query mut sub
--             -> GQLRequest -> m GQLResponse
--
--
-- (Interpreter (GQLRequest ->  GQLResponse) AppObs ())
-- class Interpreter k m e
-- where k :: a -> IO a
-- where k :: GQLRequest -> IO GQLResponse
--
--
-- interpreter gqlRoot :: Interpreter k AppObs () => k
