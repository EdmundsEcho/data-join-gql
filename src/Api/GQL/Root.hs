{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module     : Api.GQL.Root
-- Description: ObsEtl UI access point
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
module Api.GQL.Root ( gqlRoot )
    where

---------------------------------------------------------------------------------
import           Data.Morpheus.Types
---------------------------------------------------------------------------------
import           AppTypes
---------------------------------------------------------------------------------
import           Api.GQL.RootResolvers
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.Root
---------------------------------------------------------------------------------
-- * Root resolver
-- |
--
gqlRoot :: RootResolver AppObs () Query Mutation Undefined
gqlRoot = rootResolver

---------------------------------------------------------------------------------
-- |
-- Endpoints specified in @schemas.root.graphql@
--
-- Resolvers specified here.
--
rootResolver :: RootResolver AppObs () Query Mutation Undefined
rootResolver =
    RootResolver
        { queryResolver =
              Query
                  { getObsEtl     = resolverGetObsEtl
                  , validate      = resolverValidate
                  , reqMatrixSpec = resolverReqMatrixSpec
                  , getStatus     = resolverGetStatus
                  , selectValues  = resolverSelectValues
                  , levels        = resolverLevels
                  }
        , mutationResolver = Mutation {newObsEtl = resolverNewObsETL}
        , subscriptionResolver = Undefined
        }

---------------------------------------------------------------------------------
