{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module     : Api.GQL.Root
-- Description: ObsEtl UI access point
--
module Api.GQL.Root
  ( gqlRoot )
    where

---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import           Data.Morpheus.Document (importGQLDocument)
import           Data.Morpheus.Types
---------------------------------------------------------------------------------
import           Control.Concurrent.STM
---------------------------------------------------------------------------------
import           Api.GqlHttp
import qualified AppTypes               as App
---------------------------------------------------------------------------------
import           Api.GQL.ObsETL
import           Api.GQL.Request
import           AppTypes
---------------------------------------------------------------------------------
import           Api.GQL.Schemas
importGQLDocument "src/Api/GQL/schema.root.graphql"
---------------------------------------------------------------------------------


---------------------------------------------------------------------------------
-- * Root resolver
-- |
--
gqlRoot :: GQLRootResolver AppObs () Query Mutation Undefined
gqlRoot = rootResolver
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
    App.DataObsETL o  -> do
      obsEtl <- resolverObsEtl o
      pure $ Just obsEtl
    _ -> pure Nothing

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
resolverNewObsETL :: NewObsEtlArgs -> Object MUTATION ObsETL
resolverNewObsETL NewObsEtlArgs {value = newObs'} = do
  let newObs = fromInputObsEtl newObs'                     -- parse input
  db <- getDb
  let newDb = db { App.db = App.DataObsETL newObs }
  dbTVar <- lift $ asks App.database
  liftIO . atomically $ writeTVar dbTVar newDb
  resolverObsEtl newObs                                   -- display result

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
  -- get the data, return nothing if not initiated with the right data
  obsEtl <- fmap App.db getDb
  case obsEtl of
    App.DataObsETL obsEtl' ->   -- have the correct data
      case fetchRequest req obsEtl' of -- :: Input -> Maybe Model
        Nothing      -> pure Nothing
        Just request -> Just <$> resolverRequest request -- (resolverRequest request)

    _ -> pure Nothing          -- have the wrong data

-------------------------------------------------------------------------------
-- * Utility functions
-- |
getDb :: GraphQL o => Value o App.Database
getDb = do
  dbTVar <- lift $ asks App.database
  liftIO . atomically $ readTVar dbTVar
