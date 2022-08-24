-- |
-- Module      : Api.HTTP.ObsEtl
-- Description : The endpoint for the UI
--
module Api.HTTP.ObsETL (ObsEtlApi , serveObsEtlApi)
    where

--------------------------------------------------------------------------------
import           Protolude
--------------------------------------------------------------------------------
import           Data.Morpheus       (interpreter)
import           Data.Morpheus.Types
--------------------------------------------------------------------------------
import           Servant
--------------------------------------------------------------------------------
import           Api.GQL.Root        (gqlRoot)
import           AppTypes
import qualified WithAppContext      as App hiding (WithAppContext)
--------------------------------------------------------------------------------
--New IO to read file
import qualified Data.ByteString.Lazy as B
import           Data.Aeson
import           Api.GQL.ObsETL      (fromInputObsEtl, ObsEtlInput)
--------------------------------------------------------------------------------
--
type ProjectId = Text

jsonFile :: FilePath
jsonFile = "obsTestCase.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

decodeObsInput :: IO (Maybe ObsEtlInput)
decodeObsInput = decode <$> getJSON
--
-- Endpoint type constructor
--
type GQLApi (version :: Symbol) (name :: Symbol) (projectId :: Symbol)
  = version     -- endpoint
  :> name       -- endpoint
  :> Capture projectId ProjectId  -- endpoint
  :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse -- Servant Has Handler

-- |
-- == Endpoint type
-- Servant Has Server types
--
-- Applicatoin of the type constructor. Is the first parameter in the
-- ServerT construction (concrete application).
--
type ObsEtlApi  = GQLApi "v1" "warehouse" "projectId"


-- |
-- Ingredient for the Handler
--
-- interpreter :: Monad m
--             => RootResCon m e query mut sub
--             => GQLRootResolver m e query mut sub -> a -> b
--
-- gqlRoot :: GQLRootResolver AppObs () Query Mutation Undefined
--
-- a :: GQLRequest
-- b :: m GQLResponse
-- m :: AppObs
--
api :: ProjectId -> GQLRequest -> AppObs GQLResponse
api pid req = do
   maybeObs :: Maybe ObsEtlInput <- liftIO decodeObsInput
   obsInput :: ObsEtlInput <- case maybeObs of
     Just obs -> pure obs
     Nothing  -> do
        App.logErrorN "Failed to decode obsetl"
        App.throw
           $ App.ValueException
                (Just "\nThe obsetl data decoding failed")

   newObs <- fromInputObsEtl obsInput
   dbTVar <- asks App.database
   _      <- liftIO . atomically $ App.readTVar dbTVar
   interpreter gqlRoot req


serveGQL :: (ProjectId -> GQLRequest -> AppObs GQLResponse)
         -> ServerT (GQLApi version name projectId) AppObs
serveGQL = identity
-- |
-- == Handlers
-- Servant Has Handler
-- > type ServerT api (m :: * -> *) :: *
-- ~ Text -> Handler GQLResponse
-- AppObs is (m:: * -> *)
--
serveObsEtlApi :: ServerT ObsEtlApi AppObs
serveObsEtlApi = serveGQL api
