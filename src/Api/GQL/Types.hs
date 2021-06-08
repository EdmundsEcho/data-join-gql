{-# LANGUAGE ConstraintKinds #-}

module Api.GQL.Types where

---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import           Control.Monad.Trans.Class
---------------------------------------------------------------------------------
import           Data.Morpheus.Types
import           Data.Morpheus.Types.Internal.AST (OperationType)
---------------------------------------------------------------------------------
-- | Type level Resolvers
-- data Resolver (o :: OperationType) event (m :: * -> *) value
--
-- Resolve single value
type Value (o :: OperationType) (m :: * -> *) a
     = Resolver o () m a

-- | Resolve object (which includes other fields that need their own resolvers)
type Object (o :: OperationType) (m :: * -> *) a
     = Resolver o () m (a (Resolver o () m))

-- | Resolve (Maybe object)
type OptionalObject (o :: OperationType) (m :: * -> *)  a
     = Resolver o () m (Maybe (a (Resolver o () m)))

-- | Resolve [object!]!
type ArrayObject (o :: OperationType) (m :: * -> *) a
     = Resolver o () m [a (Resolver o () m)]

-- | Resolve [object!]
type OptionalArrayObject (o :: OperationType) (m :: * -> *) a
     = Resolver o () m (Maybe [a (Resolver o () m)])

-- | Resolve [object]
type ArrayOptions (o :: OperationType) (m :: * -> *) a
     = Resolver o () m [Maybe (Object o m a)]


type GraphQL o m
     = ( MonadIO (Resolver o () (m :: * -> *))
       , MonadTrans (Resolver o ())
       , WithOperation o
       )

---------------------------------------------------------------------------------
