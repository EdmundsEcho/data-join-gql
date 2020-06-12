{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}
-- {-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module     : Matrix.Root
-- Description: ObsEtl UI access point
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
module TestRoot ( api )
    where

---------------------------------------------------------------------------------
import           Data.Morpheus         (Interpreter (..))
import           Data.Morpheus.Types
---------------------------------------------------------------------------------
import           TestTypes             (TestObs)
import           WithAppContext
---------------------------------------------------------------------------------
import           Api.GQL.RootResolvers
---------------------------------------------------------------------------------

api :: GQLRequest -> TestObs GQLResponse
api = interpreter gqlRoot

---------------------------------------------------------------------------------
-- * Root resolver
-- |
--
gqlRoot :: GQLRootResolver TestObs () Query Mutation Undefined
gqlRoot = rootResolver

-- |
-- Endpoints specified in @schemas.root.graphql@
--
rootResolver :: WithAppContext m
             => GQLRootResolver m () Query Mutation Undefined
rootResolver =
    GQLRootResolver
        { queryResolver =
              Query
                  { getObsEtl     = resolverGetObsEtl
                  , validate      = resolverValidate
                  , reqMatrixSpec = resolverReqMatrixSpec
                  , getStatus     = resolverGetStatus
                  }
        , mutationResolver = Mutation {newObsEtl = resolverNewObsETL}
        , subscriptionResolver = Undefined
        }

---------------------------------------------------------------------------------
