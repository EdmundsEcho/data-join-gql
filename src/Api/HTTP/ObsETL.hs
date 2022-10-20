-- |
-- Module      : Api.HTTP.ObsEtl
-- Description : The endpoint for the UI
--
module Api.HTTP.ObsETL (ObsEtlApi , serveObsEtlApi)
    where
--
--------------------------------------------------------------------------------
import           Protolude           hiding (null)
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
import qualified Data.ByteString.Lazy as LB
import           Data.Aeson
--------------------------------------------------------------------------------
import           Api.GQL.ObsETL                 (fromInputObsEtl, ObsEtlInput)
import           HttpClient                     (request)
import           Model.ETL.ObsETL               (ObsETL)
--------------------------------------------------------------------------------
import qualified Amazonka                as S3
import qualified Amazonka.S3.GetObject   as S3
import qualified Data.Conduit.Binary     as CB  (sinkLbs)
import           Data.Conduit (ConduitT)
import           Control.Monad.Trans.Resource   ( ResourceT
                                                , closeInternalState
                                                , createInternalState
                                                , runInternalState )
--------------------------------------------------------------------------------
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
    _ <- setDbWithS3 pid
    interpreter gqlRoot req

-- |
-- Update database with ObsEtlInput retrieved from S3
--
setDbWithS3 :: (MonadCatch m, MonadLogger m, MonadReader Env m, MonadIO m)
          => ProjectId -> m (ConduitT () S3.ByteString (ResourceT IO) ())
setDbWithS3 pid = do
   env :: Env <- ask
   let s3env' = s3env env
   let cfg    = App.config env
   resourceState <- createInternalState
   obsEtl <- flip runInternalState resourceState $ do   -- :: ResourceT m ObsETL
        awsRes <- request pid cfg s3env'
        rawBytes <- S3.body awsRes `S3.sinkBody` CB.sinkLbs  -- ConduitM ByteString Void (ResourceT IO) a
        tryObsFromResponse rawBytes

   dbTVar <- asks App.database  -- :: TVar Database
   App.logInfoN ("💫 Loading project: " <> show pid)
   -- side-effect: mutate db state
   let newStore = dbNew pid obsEtl  -- App.Database
   liftIO . atomically $ App.writeTVar dbTVar newStore
   pure $ closeInternalState resourceState

     where
        -- |
        -- Json ByteSting Input -> ObsEtl
        tryObsFromResponse :: (MonadLogger m, MonadCatch m) => LB.ByteString -> m ObsETL
        tryObsFromResponse res = do

            -- Json ByteString -> Maybe ObsEtlInput
            obsInput :: ObsEtlInput <- case decode res of
              Just obs -> pure obs
              Nothing  -> do
                 App.logErrorN "Failed to decode the json"
                 App.throw
                    $ App.ValueException
                         (Just "\nThe obsetl data decoding failed")

            tryBuildObs <- fromInputObsEtl obsInput -- :: m (Either ObsException ObsETL)
            case tryBuildObs of -- :: Either ObsException ObsETL
                Left e -> App.throw e
                Right obsEtl -> pure obsEtl



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
