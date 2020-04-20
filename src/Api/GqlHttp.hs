{-# LANGUAGE ConstraintKinds #-}
{-|
Module      : Api.GqlHttp
Description : Bridge between Servant, GraphQL and the WebApp types
-}
module Api.GqlHttp where

-------------------------------------------------------------------------------
import           Protolude                        (Maybe, MonadIO, Symbol,
                                                   identity)
-------------------------------------------------------------------------------
import           Control.Monad.Trans              (MonadTrans)
-------------------------------------------------------------------------------
import           AppTypes
import           Data.Morpheus.Types              (GQLRequest, GQLResponse,
                                                   Resolver, WithOperation)
import           Data.Morpheus.Types.Internal.AST (OperationType)
import           Servant
-------------------------------------------------------------------------------
-- |
-- == > Http -> Gql helper functions
-- | Morpheus helpers
type GQLAPI (name :: Symbol) (version :: Symbol)
  = name     -- endpoint
  :> version -- endpoint
  :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse -- Servant Has Handler

-- |
-- == xx
-- Usage
--   > serveGQL ( interpreter rootResolver )
serveGQL :: (GQLRequest -> AppObs GQLResponse)
         -> ServerT (GQLAPI name version) AppObs
serveGQL = identity


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
type ArrayObject (o :: OperationType) a
     = Resolver o () AppObs [a (Resolver o () AppObs)]

type GraphQL o
     = ( MonadIO (Resolver o () AppObs)
       , WithOperation o
       , MonadTrans (Resolver o ())
       )
