{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api.GQL.Test
  (
    gqlRoot,
  )
where

import           Api.GQL.GqlPlace    (City (..))
import           Data.Morpheus.Types (GQLRootResolver (..), GQLType, IORes,
                                      Undefined (..))
import           Protolude

--------------------------------------------------------------------------------
-- GQL
newtype Query m = Query {place :: m Place} deriving (Generic, GQLType)

newtype Place = Place {place :: City} deriving (Generic, GQLType)

thisPlace :: Place
thisPlace = Place {place = Ithaca}

resolvePlace :: IORes e Place
resolvePlace = return thisPlace

gqlRoot :: GQLRootResolver IO () Query Undefined Undefined
gqlRoot =
  GQLRootResolver
    { queryResolver = Query {place = resolvePlace},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

-- api :: B.ByteString -> IO B.ByteString
-- api = interpreter gqlRoot
