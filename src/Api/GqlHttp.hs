{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds       #-}
{-|
Module      : Api.GqlHttp
Description : Bridge between Servant, GraphQL and the WebApp types
-}
module Api.GqlHttp where

-------------------------------------------------------------------------------
import           Data.Aeson                       hiding (Value)
import           Data.Aeson.Encode.Pretty
import           Protolude
-------------------------------------------------------------------------------
import           Control.Monad.Trans              (MonadTrans)
-------------------------------------------------------------------------------
import           AppTypes
import           Data.Morpheus.Types              (ComposedResolver, GQLRequest,
                                                   GQLResponse, Resolver,
                                                   ResolverO, WithOperation)
import           Data.Morpheus.Types.Internal.AST (OperationType)
import           Servant
-------------------------------------------------------------------------------
  --
-- == > Http -> Gql helper functions
-- |
-- Morpheus helpers
--
type GQLAPI (name :: Symbol) (version :: Symbol)
  = name     -- endpoint
  :> version -- endpoint
  :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse -- Servant Has Handler

-- == Gql endpoint type
-- |
-- Usage
--
--   > serveGQL ( interpreter rootResolver )
--   later,
--   serveObsEtlApi :: ServerT ObsEtlApi AppObs
--   serveObsEtlApi = serveGQL api
--
--   api :: GQLRequest -> AppObs GQLResponse
--   api = interpreter gqlRoot
--
serveGQL :: (GQLRequest -> AppObs GQLResponse)
         -> ServerT (GQLAPI name version) AppObs
serveGQL = identity


---------------------------------------------------------------------------------
-- == Webserver logging capacity
-- |
--
-- /Note/ To access the AppObs logging capacity
-- ~ runReaderT (runApp resolver) readerContext
-- logger :: (ToJSON a, GraphQL o) => a -> Value o ()
--
logger :: (ToJSON a, MonadIO m) => a -> m ()
logger = liftIO . putStrLn . encodePretty

---------------------------------------------------------------------------------
-- | Type level Resolvers
-- data Resolver (o :: OperationType) event (m :: * -> *) value
--
-- Resolve single value
type Value (o :: OperationType) a = Resolver o () AppObs a

-- | Resolve object (which includes other fields that need their own resolvers)
type Object (o :: OperationType) a
     = Resolver o () AppObs (a (Resolver o () AppObs))

-- | Resolve (Maybe object)
type OptionalObject (o :: OperationType) a
     = Resolver o () AppObs (Maybe (a (Resolver o () AppObs)))

-- | Resolve [object]!
type ArrayObject (o :: OperationType) a
     = Resolver o () AppObs [a (Resolver o () AppObs)]

-- | Resolve [object]
type OptionalArrayObject (o :: OperationType) a
     = Resolver o () AppObs (Maybe [a (Resolver o () AppObs)])

-- |
-- Resolve value
-- Updated, and not yet implemented
--
type Value' (o :: OperationType) (a :: k) = ResolverO o () AppObs a

-- |
-- Resolve (f value)
-- New and not yet implemented
--
type Composed' (o :: OperationType) f (a :: k) = ComposedResolver o () AppObs f a

type GraphQL o
     = ( MonadIO (Resolver o () AppObs)
       , MonadTrans (Resolver o ())
       , WithOperation o
       )




---------------------------------------------------------------------------------
  --
