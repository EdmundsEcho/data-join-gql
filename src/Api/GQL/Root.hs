{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module     : Api.GQL.Root
-- Description: ObsEtl UI access point
--
module Api.GQL.Root
  ( gqlRoot )
    where
-------------------------------------------------------------------------------
import           Api.GQL.ObsETL
import           AppTypes
import           Data.Morpheus.Types
-------------------------------------------------------------------------------

-- |
-- == Root Resolver
-- |
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
                  , getStatus = resolverGetStatus
                  }
        , mutationResolver = Mutation {newObsEtl = resolverNewObsETL}
        , subscriptionResolver = Undefined
        }
