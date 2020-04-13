-- | Gql root definition
-- Note: There are many resolvers.
-- * Query :: GqlInput -> Model (GqlInput -> Model -> Model) -> View
-- * Mutation :: GqlInput -> Model (GqlInput -> Model -> Model) -> View
-- * View :: Model -> Gql
--
  {-# LANGUAGE ConstraintKinds            #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Api.GQL.ObsTest
  (
    gqlRoot,
  )
where

import           Protolude

import           Data.Morpheus.Document           (importGQLDocument)
import           Data.Morpheus.Types
import           Data.Morpheus.Types.Internal.AST (OperationType)

import           Control.Concurrent.STM
import           Control.Monad.Logger
import           Control.Monad.Trans              (MonadTrans)
import qualified Model.Types                      as Model
import           Types

importGQLDocument "src/Api/GQL/schema.graphql"


-------------------------------------------------------------------------------
-- gqlRoot
-- serveGQL :: MonadIO m =>  (GQLReqeust -> IO GQLResponse)
--          -> ServerT GQLAPI m
--
-- api :: GQLRequest -> IO GQLResponse
-- api = interpreter gqlRoot
gqlRoot = rootResolver

-- GQLRootResolver
--     queryResolver :: query (Resolver QUERY event m)
--     mutationResolver :: mut (Resolver MUTATION event m)
--     subscriptionResolver :: sub (Resolver SUBSCRIPTION event m)
rootResolver :: GQLRootResolver AppObs () Query Mutation Undefined
rootResolver =
    GQLRootResolver
        { queryResolver =
              Query
                  { getObsEtl = resolverGetObsEtl
                  , getStatus = resolverGetStatus
                  }
        , mutationResolver = Mutation {newObsEtl = resolverNewObsEtl}
        , subscriptionResolver = Undefined
        }
---------------------------------------------------------------------------------
-- | Type level Resolvers
-- data Resolver (o :: OperationType) event (m :: * -> *) value
--
-- Resolve single value
type Value (o :: OperationType) a = Resolver o () AppObs a

-- | Resolve object (which includes other fields that need their own resolvers)
type Object (o :: OperationType) a = Resolver o () AppObs (a (Resolver o () AppObs))

-- | Resolve (Maybe object)
type OptionalObject (o :: OperationType) a
     = Resolver o () AppObs (Maybe (a (Resolver o () AppObs)))

-- | Resolve [object]
-- type ArrayObject (o :: OperationType) a
     -- = Resolver o () AppObs [a (Resolver o () AppObs)]

type GraphQL o
     = ( MonadIO (Resolver o () AppObs)
       , WithOperation o
       , MonadTrans (Resolver o ())
       , MonadLogger (Resolver o () AppObs))

-------------------------------------------------------------------------------
-- GQL Query Resolvers

resolverGetObsEtl :: OptionalObject QUERY ObsEtl
resolverGetObsEtl = do
  obsEtl' <- fmap obsEtl getDb
  case obsEtl' of
    Just o  -> do
      obsEtl <- resolverObsEtl o
      pure $ Just obsEtl
    Nothing -> pure Nothing

resolverGetStatus :: Value QUERY Text
resolverGetStatus = fmap status getDb

getDb :: GraphQL o => Value o Database
getDb = do
  dbTVar <- lift $ asks database
  liftIO . atomically $ readTVar dbTVar

--------------------------------------------------------------------------------
-- GQL Mutation Resolvers
resolverNewObsEtl :: NewObsEtlArgs -> Object MUTATION ObsEtl
resolverNewObsEtl NewObsEtlArgs {value = newObs'} = do
  let newObs = fromInputObsEtl newObs'
  db <- getDb
  let newDb = db { obsEtl = Just newObs }
  dbTVar <- lift $ asks database
  liftIO . atomically $ writeTVar dbTVar newDb
  resolverObsEtl newObs

fromInputObsEtl :: ObsEtlInput -> Model.ObsEtl
fromInputObsEtl ObsEtlInput {..}
  = Model.ObsEtl subject (fromInputQuality quality)

fromInputQuality :: QualityInput -> Model.Quality
fromInputQuality QualityInput {..}
  = Model.Quality name (fromInputValues values)

fromInputValues :: [Text] -> [Text]
fromInputValues = identity

-------------------------------------------------------------------------------
-- Model -> View resolvers
-- fromModelToView
resolverObsEtl :: GraphQL o => Model.ObsEtl -> Object o ObsEtl
resolverObsEtl Model.ObsEtl {subject = thisSubject, quality = thisQuality} =
  do
    _ <- logInfoN $ "Processing obsEtl view: " <> thisSubject
    pure $
      ObsEtl
        { subject = resolverSubject thisSubject
        , quality = resolverQuality thisQuality
        }
    where
      resolverSubject = pure

resolverQuality :: GraphQL o => Model.Quality -> Object o Quality
resolverQuality Model.Quality {name = thisName, values = thisValues} =
  pure $
    Quality
      { name = resolverName thisName
      , values = resolverValues thisValues
      }
  where
    resolverName = pure
    resolverValues = pure
    -- resolverValue = pure

