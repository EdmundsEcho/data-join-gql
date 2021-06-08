{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Module     : Api.GQL.ObsTest
-- Description: Test/Seed to build out GQL capacity
--
-- There are many GQL resolvers.
--   * > Query :: GqlInput -> Model (GqlInput -> Model -> Model) -> View
--   * > Mutation :: GqlInput -> Model (GqlInput -> Model -> Model) -> View
--   * > View :: Model -> Gql
--
module Api.GQL.ObsTest
  (
    gqlRoot,
  )
where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Morpheus.Document           (importGQLDocument)
import           Data.Morpheus.Types
import           Data.Morpheus.Types.Internal.AST (OperationType)
-------------------------------------------------------------------------------
import           AppTypes
import           Control.Monad.Trans              (MonadTrans)
import qualified Model.ObsTest                    as Model
-------------------------------------------------------------------------------
importGQLDocument "src/Api/GQL/Schemas/schema.test.graphql"
-------------------------------------------------------------------------------
-- |
-- == Root Resolver
-- Maries Queries and Mutations
gqlRoot :: RootResolver AppObs () Query Mutation Undefined
gqlRoot = rootResolver

-- RootResolver
--     queryResolver :: query (Resolver QUERY event m)
--     mutationResolver :: mut (Resolver MUTATION event m)
--     subscriptionResolver :: sub (Resolver SUBSCRIPTION event m)
rootResolver :: RootResolver AppObs () Query Mutation Undefined
rootResolver =
    RootResolver
        { queryResolver =
              Query
                  { getObsTest = resolverGetObsTest
                  , getStatus = resolverGetStatus
                  }
        , mutationResolver = Mutation {newObsTest = resolverNewObsTest}
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
       )

-------------------------------------------------------------------------------
-- |
-- == GQL Query Resolvers

resolverGetObsTest :: OptionalObject QUERY ObsTest
resolverGetObsTest = do
  obsEtl' <- fmap db getDb
  case obsEtl' of
    DataObsTest o -> Just <$> resolverObsTest o
    _             -> pure Nothing

resolverGetStatus :: Value QUERY Text
resolverGetStatus = fmap status getDb

getDb :: GraphQL o => Value o Database
getDb = do
  dbTVar <- lift $ asks database
  liftIO . atomically $ readTVar dbTVar

--------------------------------------------------------------------------------
-- |
-- == GQL Mutation Resolvers
resolverNewObsTest :: NewObsTestArgs -> Object MUTATION ObsTest
resolverNewObsTest NewObsTestArgs {value = newObs'} = do
  let newObs = fromInputObsTest newObs'
  db <- getDb
  let newDb = db { db = DataObsTest newObs }
  dbTVar <- lift $ asks database
  liftIO . atomically $ writeTVar dbTVar newDb
  resolverObsTest newObs

fromInputObsTest :: ObsTestInput -> Model.ObsTest
fromInputObsTest ObsTestInput {..}
  = Model.ObsTest subject (fromInputQuality quality)

fromInputQuality :: QualityInput -> Model.Quality
fromInputQuality QualityInput {..}
  = Model.Quality name (fromInputValues values)

fromInputValues :: [Text] -> [Text]
fromInputValues = identity

-------------------------------------------------------------------------------
-- |
-- == Model -> View resolvers
-- fromModelToView
resolverObsTest :: GraphQL o => Model.ObsTest -> Object o ObsTest
resolverObsTest Model.ObsTest {subject = thisSubject, quality = thisQuality} =
  -- do
    -- _ <- logInfoN $ "Processing obsEtl view: " <> thisSubject
    pure $
      ObsTest
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
