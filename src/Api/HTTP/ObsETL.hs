-- |
-- Module      : Api.HTTP.ObsEtl
-- Description : The endpoint for the UI
--
module Api.HTTP.ObsETL (ObsEtlApi , serveObsEtlApi)
    where
--
--------------------------------------------------------------------------------
import           Protolude           hiding (null)
import           Data.Text
import           Control.Concurrent.STM.TVar          (TVar)
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
--------------------------------------------------------------------------------
import           Api.GQL.ObsETL      (fromInputObsEtl, ObsEtlInput)
import           HttpClient          (bucketName, request, sinkFile)
import           Model.ETL.ObsETL    (ObsETL)
--------------------------------------------------------------------------------
import qualified Amazonka                as S3
import qualified Amazonka.S3             as S3
import qualified Amazonka.S3.GetObject   as S3
import           Control.Monad.Trans.Resource
import           Servant.Conduit
import           Data.Conduit
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

--conduits :: (MonadAWS m) => m (S.Stream (Of BS.ByteString) (ResourceT IO) ())
--conduits = do
--  let bucketName = BucketName "test"
--      key = ObjectKey "foobar"
--  rs <- send (getObject bucketName key)
--  let (RsBody body) = view gorsBody rs
--  return $ hoist lift body $$+- CL.mapM_ S.yield

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
   -- let s3env' = s3env env
   let cfg    = App.config env
   pure undefind
--   resourceState <- createInternalState
--   res <- flip runInternalState resourceState $ do
--       -- response has access to streaming interface,
--       -- a conduit source, a lazy ByteString
--       response :: S3.AWSResponse S3.GetObject <- request pid cfg s3env'
--       -- let body :: S3.ResponseBody = S3.body response
--       -- let (S3.RsBody body) :: S3.view gorsBody response
--       -- body `S3.sinkBody` sinkFile (bucketName pid)
--       let raw = S3._streamBody body
--       let maybeObs = decode raw
--
--       maybeObs :: Maybe ObsEtlInput <- liftIO $ decode res
--
--       -- raw <- body `S3.sinkBody` CB.sinkLazy
--       let path = "it_does_not_matter.json"
--
--
--       obsInput :: ObsEtlInput <- case maybeObs of
--         Just obs -> pure obs
--         Nothing  -> do
--            App.logErrorN $ "Failed to read: " <> pack path
--            App.throw
--               $ App.ValueException
--                    (Just "\nThe obsetl data decoding failed")
--
--       tryObsETL <- fromInputObsEtl obsInput  -- :: Either Exception ObsETL
--
--       -- retrieve the store ref
--       dbTVar <- asks App.database  -- :: TVar Database
--
--       App.logInfoN ("💫 Loading project: " <> show pid)
--
--       -- build a new store from tryObsETL if possible, otherwise return empty db
--       newStore  <- case tryObsETL of
--            Left  e      -> do App.logErrorN $ show e; pure dbInit
--            Right obsETL -> pure $ dbNew obsETL
--
--       liftIO . atomically $ App.writeTVar dbTVar newStore
--
--       interpreter gqlRoot req
--
--   pure $ res *> closeInternalState resourceState

-- |
-- Update database with ObsEtlInput
-- modified forwarding stream to instantiate database
--oneStepHandler :: ProjectId -> Config -> TVar Database -> Servant.Handler (ConduitT () ByteString (ResourceT IO) ())
--oneStepHandler pid cfg dbTVar = do
--  awsEnv <- S3.newEnv S3.Discover
--  resourceState <- createInternalState
--  res :: ConduitM () ByteString (ResourceT a) () <- flip runInternalState resourceState $ do
--      awsRes :: S3.AWSResponse S3.GetObject <- request pid cfg awsEnv
--      -- awsRes <- S3.send awsEnv S3.getObjectRequest
--      let body :: S3.ResponseBody = S3.body awsRes
--      pure . S3._streamBody $ body
--
--  -- build a new store from try, otherwise return empty db
--  -- xxx ConduitM () ByteString (ResourceT a) () -> ByteString
--  tryObs <- tryObsFromResponse res
--  newStore  <- case tryObs of
--      Left  e      -> do App.logErrorN $ show e; pure dbInit
--      Right obsETL -> pure $ dbNew obsETL
--
--  -- side-effect: mutate db
--  liftIO . atomically $ App.writeTVar dbTVar newStore
--
--  -- It took me a long time to understand what this was doing.
--  -- Because ConduitT is a monad transformer and we have
--  -- closeInternalState :: MonadIO m => InternalState -> m ()
--  -- this attaches the `closeInternalState resourceState` action to the
--  -- end of the conduit, just before it returns its final value
--  -- (which was () anyway).
--  pure $ res *> closeInternalState resourceState
--
---- |
---- Json ByteSting Input -> ObsEtl
--tryObsFromResponse :: (MonadLogger m, MonadCatch m) => B.ByteString -> m (Either ObsException ObsETL)
--tryObsFromResponse res = do
--
--    -- Json ByteString -> Maybe ObsEtlInput
--    obsInput :: ObsEtlInput <- case decode res of
--      Just obs -> pure obs
--      Nothing  -> do
--         App.logErrorN "Failed to decode the json"
--         App.throw
--            $ App.ValueException
--                 (Just "\nThe obsetl data decoding failed")
--
--    fromInputObsEtl obsInput  -- :: Either Exception ObsETL



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
