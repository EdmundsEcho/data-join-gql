-- |
-- Module      : Api.HTTP.ObsEtl
-- Description : The endpoint for the UI
--
module Api.HTTP.ObsETL (ObsEtlApi , serveObsEtlApi)
    where
--
--------------------------------------------------------------------------------
-- import           Network.URI             (parseURI)
import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as H
--------------------------------------------------------------------------------
import           Protolude           hiding (null)
import           Data.Text
--------------------------------------------------------------------------------
import           Data.Morpheus       (interpreter)
import           Data.Morpheus.Types
--------------------------------------------------------------------------------
import           Servant
--------------------------------------------------------------------------------
import           Api.GQL.Root        (gqlRoot)
import           AppTypes            as App
--------------------------------------------------------------------------------
--New IO to read file
import qualified Data.ByteString.Lazy as B
import           Data.Aeson
import           Api.GQL.ObsETL      (fromInputObsEtl, ObsEtlInput)
--------------------------------------------------------------------------------
--
type ProjectId = Text
type WithFile = Bool

warehouseFileName :: [Char]
warehouseFileName = "warehouse.json"


-- |
-- Where to retrieve project-specific data
--
mkFilename :: WithFile -> Config -> ProjectId -> FilePath
mkFilename wf cfg projectId = do
    let path = unpack (mkMountPoint (mountPoint cfg))
             <> unpack (mkDataDir (dataDir cfg))
             <> "/" <> unpack projectId

    if wf then path <> "/" <> warehouseFileName else path

    where
        mkDataDir :: Text -> Text
        mkDataDir d = if null d then "" else "/" <> d

        mkMountPoint :: Text -> Text
        mkMountPoint m = if null m then "" else "/" <> m

-- |
-- Where to retrieve project-specific data
--
mkRequest :: Config -> ProjectId -> H.Request
mkRequest cfg projectId = undefined

-- Facilitates type inference
getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile


decodeFromPathObsInput :: FilePath -> IO (Maybe ObsEtlInput)
decodeFromPathObsInput path = decode <$> getJSON path

decodeFromUrlObsInput :: FilePath -> IO (Maybe ObsEtlInput)
decodeFromUrlObsInput path = decode <$> getJSON path
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
-- Consumed by the Handler
--
-- ⬜ review the error handling
--
-- MOUNT_POINT + '/diamonds/{project_id}/warehouse.json'
-- projectId is extracted from the endpoint by servant using Capture
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
   -- derive where to find the project-specific data
   env :: Env <- ask
   let path = mkFilename True (App.config env) pid

   maybeObs :: Maybe ObsEtlInput <- liftIO $ decodeFromPathObsInput path

   obsInput :: ObsEtlInput <- case maybeObs of
     Just obs -> pure obs
     Nothing  -> do
        App.logErrorN $ "Failed to read: " <> pack path
        App.throw
           $ App.ValueException
                (Just "\nThe obsetl data decoding failed")

   tryObsETL <- fromInputObsEtl obsInput  -- :: Either Exception ObsETL

   -- retrieve the store ref
   dbTVar <- asks App.database  -- :: TVar Database

   App.logInfoN ("💫 Loading project: " <> show pid)

   -- build a new store from tryObsETL if possible, otherwise return empty db
   newStore  <- case tryObsETL of
        Left  e      -> do App.logErrorN $ show e; pure dbInit
        Right obsETL -> pure $ dbNew obsETL

   liftIO . atomically $ App.writeTVar dbTVar newStore

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
