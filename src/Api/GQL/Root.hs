{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module     : Api.GQL.Root
-- Description: ObsEtl UI access point
--
module Api.GQL.Root
  ( gqlRoot )
    where

---------------------------------------------------------------------------------
import           Control.Exception.Safe
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Protolude
---------------------------------------------------------------------------------
import           Data.Morpheus.Document    (importGQLDocument)
import           Data.Morpheus.Types
---------------------------------------------------------------------------------
import           Control.Concurrent.STM
---------------------------------------------------------------------------------
import           Api.GqlHttp
import qualified Model.ETL.ObsETL          as Model
import qualified Model.Request             as Model (Request, validate)
import           Model.Status
---------------------------------------------------------------------------------
import qualified AppTypes                  as App
import qualified ObsExceptions             as App
---------------------------------------------------------------------------------
import           Api.GQL.Input.Request     (fetchRequest)
import           Api.GQL.ObsETL
import           Api.GQL.RequestView       (resolverRequest)
import           AppTypes
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.Request
importGQLDocument "src/Api/GQL/Schemas/schema.root.graphql"
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- * Root resolver
-- |
--
gqlRoot :: GQLRootResolver AppObs () Query Mutation Undefined
gqlRoot = rootResolver
--
-- >  GQLRootResolver
-- >      queryResolver :: query (Resolver QUERY event m)
-- >      mutationResolver :: mut (Resolver MUTATION event m)
-- >      subscriptionResolver :: sub (Resolver SUBSCRIPTION event m)
--
--
rootResolver :: GQLRootResolver AppObs () Query Mutation Undefined
rootResolver =
    GQLRootResolver
        { queryResolver =
              Query
                  { getObsEtl = resolverGetObsEtl
                  , validate  = resolverValidate
                  , getStatus = resolverGetStatus
                  }
        , mutationResolver = Mutation {newObsEtl = resolverNewObsETL}
        , subscriptionResolver = Undefined
        }

---------------------------------------------------------------------------------
-- * Query Resolvers
-- ** Query getObsEtl
-- |
-- View data pulled from the 'Env'.
-- > Input -> Model -> View
--
resolverGetObsEtl :: OptionalObject QUERY ObsETL
resolverGetObsEtl = do
  obsEtl' <- fmap App.db getDb
  case obsEtl' of
    App.DataObsETL o -> Just <$> resolverObsEtl o
    _                -> pure Nothing

---------------------------------------------------------------------------------
-- ** Query validate
-- |
-- Validation = Instantiate 'Model.Request' with the user input objects
-- specified in @schemas.request.graphql@.
--
resolverValidate :: ValidateArgs -> OptionalObject QUERY Request
resolverValidate ValidateArgs { request } = delegateForValidate request

-- ** getStatus
-- |
-- Test/dummy query
resolverGetStatus :: Value QUERY Text
resolverGetStatus = fmap App.status getDb

---------------------------------------------------------------------------------
-- * Mutation NewObsETL
-- |
-- > Input -> Model -> View
--
-- Processes the Maybe value returned by fromInputObsEtl that can fail
-- with improper 'Model.ETL.Span' values.
--
resolverNewObsETL :: NewObsEtlArgs -> Object MUTATION ObsETL
resolverNewObsETL NewObsEtlArgs {value = newObs} = do
  newObs' <- lift $ fromInputObsEtl newObs          -- parse input, returns Either
  db      <- getDb
  newDb   <- case newObs' of
                Left  e        -> do lift . logErrorN $ show e; pure db
                Right newObs'' -> pure $ db { App.db = App.DataObsETL newObs'' }

  let App.DataObsETL newObs''' = App.db db

  dbTVar  <- lift $ asks App.database
  liftIO . atomically $ writeTVar dbTVar newDb
  resolverObsEtl newObs'''                          -- display result

---------------------------------------------------------------------------------
-- * validate
-- |
-- Make a request/pull of the 'Model.ETL.ObsETL' data.
--
-- > RequestInput -fetch-> etl -> Model.Request -resolver-> Object o Request
--
-- /Note/: Once a Model.Request is instantiated, we guarantee a return value.
--
-- > ChildReqInput -> (parent etl collection -fetch-> child etl collection)
-- >               -> Model.Request -> Object o Request
--
-- fetch :: 'RequestInput' -> 'Model.Request'
--
-- > input RequestInput {
-- >   subReq: QualityMixInput
-- >   meaReqs: [ComponentMixInput!]
-- > }
--
-- > data Request = Request
-- >   { subReq  :: !QualityMix
-- >   , meaReqs :: ![ComponentMix]
-- >   } deriving (Show, Eq)
--
--
delegateForValidate :: GraphQL o => RequestInput -> OptionalObject o Request
delegateForValidate req = do
  ---- get the data, return nothing if not initiated with the right data
  obsEtl :: Model.ObsETL                     <- getEtlData
  result :: Maybe (Model.Request 'Inprocess) <- lift $ fetchRequest req obsEtl
  traverse resolverRequest (Model.validate result)

---------------------------------------------------------------------------------
  -- Goal: App that provides MonadLogger and MonadThrow
  -- how marry AppObs with
-- goFetch :: GraphQL o => RequestInput -> Value o a
-- goFetch request = fetchRequest request -- runFetch' request

-- testLookup :: (MonadLogger m, MonadThrow m)
--            => TestInput
--            -> m (Maybe ResultModel)
---------------------------------------------------------------------------------
-- * Utility functions
-- |
-- MonadLogger (Resolver MUTATION () AppObs)
--
getDb :: GraphQL o => Value o App.Database
getDb = do
  dbTVar <- lift $ asks App.database
  liftIO . atomically $ readTVar dbTVar

getEtlData :: GraphQL o => Value o Model.ObsETL
getEtlData = do
  dats <- getDb
  case App.db dats of
    App.DataObsETL obsEtl -> do
      -- subType <- Model.subType $ lookupSubject obsEtl
      let subType = Model.subType $ Model.obsSubject obsEtl
      lift $ logInfoN ("Loading db with Subject type: " <> show subType)
      pure obsEtl  -- have the correct data
    _                     -> do
      lift $ logErrorN "Failed to load the correct data"
      lift . throw $ App.TypeException Nothing -- "Wrong ini data" -- have the wrong data

---------------------------------------------------------------------------------
  --
