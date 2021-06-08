{-|
Module      : Api.HTTP.ObsAPI
Description : The endpoint for the UI
-}
module Api.HTTP.ObsTest (ObsTest , serveObsTest)
    where

--------------------------------------------------------------------------------
import           Data.Morpheus       (interpreter)
import           Data.Morpheus.Types
import           Servant

import           Api.GQL.ObsTest     (gqlRoot)
import           Api.GqlHttp
import           AppTypes

--------------------------------------------------------------------------------
-- |
-- == Endpoint type
-- Servant Has Server types
type ObsTest  = GQLAPI "obstest" "v1"

api :: GQLRequest -> AppObs GQLResponse
api = interpreter gqlRoot

-- |
-- == Handlers
-- Servant Has Handler
serveObsTest :: ServerT ObsTest AppObs
serveObsTest = serveGQL api

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
