{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Api.GQL.RootResolvers
-- Description : Top-level resolvers for generic context
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
module Api.GQL.RootResolvers
    where

---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import           Data.Morpheus.Document     (importGQLDocument)
import           Data.Morpheus.Types
---------------------------------------------------------------------------------
import qualified Model.ETL.ObsETL           as Model
import qualified Model.Request              as Model (Request, validate)
import           Model.Status
---------------------------------------------------------------------------------
import           WithAppContext             (WithAppContext)
import qualified WithAppContext             as App hiding (WithAppContext)
---------------------------------------------------------------------------------
import           Api.GQL.Input.Request      (fetchRequest)
import           Api.GQL.MatrixSpec
import           Api.GQL.ObsETL
import           Api.GQL.RequestView        (resolverRequest)
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.MatrixSpec
import           Api.GQL.Schemas.Request
import           Api.GQL.Types
importGQLDocument "src/Api/GQL/Schemas/schema.root.graphql"
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-- * Query Resolvers
-- ** Query getObsEtl
-- |
-- View data pulled from the 'Env'.
-- > Input -> Model -> View
--
resolverGetObsEtl :: WithAppContext m
                  => OptionalObject QUERY m ObsETL
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
resolverValidate :: WithAppContext m
                 => ValidateArgs -> OptionalObject QUERY m Request
resolverValidate ValidateArgs { request } = delegateForValidate request

---------------------------------------------------------------------------------
-- ** Query reqMatrixSpec
-- |
-- Request -> MatrixSpec
-- input specified in @schemas.request.graphql@.
-- output specified in @schemas.matrix.graphql@.
--
resolverReqMatrixSpec :: WithAppContext m
                      => ReqMatrixSpecArgs -> OptionalObject QUERY m MatrixSpec
resolverReqMatrixSpec ReqMatrixSpecArgs { requestSpec } =
  delegateForMatrix requestSpec

---------------------------------------------------------------------------------
-- ** getStatus
-- |
-- Test/dummy query
resolverGetStatus :: WithAppContext m
                  => Value QUERY m Text
resolverGetStatus = fmap App.status getDb

---------------------------------------------------------------------------------
-- * Mutation NewObsETL
-- |
-- > Input -> Model -> View
--
-- Processes the Maybe value returned by fromInputObsEtl that can fail
-- with improper 'Model.ETL.Span' values.
--
resolverNewObsETL :: WithAppContext m
                  => NewObsEtlArgs -> Object MUTATION m ObsETL

resolverNewObsETL NewObsEtlArgs {value = newObs} = do
  newObs' <- lift $ fromInputObsEtl newObs          -- parse input, returns Either
  store   <- getDb
  -- lift . App.logDebugN $ "newObs: " <> show newObs'
  newDb   <- case newObs' of
                Left  e        -> do lift . App.logErrorN $ show e; pure store
                Right newObs'' -> pure $ store { App.db = App.DataObsETL newObs'' }

  obs     <- case App.db newDb of
                App.DataObsETL vs -> pure vs
                other -> do
                  lift . App.logErrorN $ "ObsETL does not exist"
                  lift . App.throw
                   $ App.ValueException
                       (Just ("\nThe ObsETL data was not recorded: " <> show other))

  dbTVar  <- lift $ asks App.database
  liftIO . atomically $ App.writeTVar dbTVar newDb
  resolverObsEtl obs                          -- display result


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
delegateForValidate :: (GraphQL o m, WithAppContext m)
                    => RequestInput -> OptionalObject o m Request
delegateForValidate req = do
  ---- get the data, return nothing if not initiated with the right data
  obsEtl :: Model.ObsETL                     <- getEtlData
  result :: Maybe (Model.Request 'Inprocess) <- lift $ fetchRequest req obsEtl
  traverse resolverRequest (Model.validate result)

---------------------------------------------------------------------------------
-- |
--
delegateForMatrix :: (GraphQL o m, WithAppContext m)
                  => RequestInput -> OptionalObject o m MatrixSpec
delegateForMatrix req = do
  ---- get the data, return nothing if not initiated with the right data
  obsEtl  :: Model.ObsETL                     <- getEtlData
  request :: Maybe (Model.Request 'Inprocess) <- lift $ fetchRequest req obsEtl
  traverse resolverMatrixSpec (Model.validate request)

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-- * Utility functions
-- |
-- MonadLogger (Resolver MUTATION () AppObs)
--
getDb :: (GraphQL o m, WithAppContext m)
      => Value o m App.Database
getDb = do
  dbTVar <- lift $ asks App.database
  liftIO . atomically $ App.readTVar dbTVar

getEtlData :: (GraphQL o m, WithAppContext m)
           => Value o m Model.ObsETL
getEtlData = do
  dats <- getDb
  case App.db dats of
    App.DataObsETL obsEtl -> do
      -- subType <- Model.subType $ lookupSubject obsEtl
      let subType = Model.subType $ Model.obsSubject obsEtl
      lift $ App.logInfoN ("Loading db with Subject type: " <> show subType)
      pure obsEtl  -- have the correct data
    _                     -> do
      lift $ App.logErrorN "Failed to load the correct data"
      lift . App.throw $ App.TypeException Nothing -- "Wrong ini data" -- have the wrong data

---------------------------------------------------------------------------------
  --
