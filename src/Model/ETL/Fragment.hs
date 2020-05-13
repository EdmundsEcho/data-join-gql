{-# LANGUAGE FunctionalDependencies #-}

module Model.ETL.Fragment where

--------------------------------------------------------------------------------
import           Protolude                 hiding (null)
--------------------------------------------------------------------------------
import           Control.Monad.Trans.Maybe
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.Request
---------------------------------------------------------------------------------

-- |
-- Unifying classtype for a fragment of ETL data ~ collection
-- without a key.
--
class Fragment fragment where
  null     :: fragment -> Bool
  len      :: fragment -> Int

class FragmentPlus fragment item | item -> fragment where
  filterF  :: [item] -> fragment -> fragment
  fromList :: [item] -> fragment
  toList   :: fragment -> [item]

instance Fragment a => Fragment (Maybe a) where
  null Nothing   = True
  null (Just vs) = null vs
  len Nothing   = 0
  len (Just vs) = len vs


-- |
-- ETL collection -> item
-- Min definition: getValues
--
-- Provides the @getFragment@ function that processes optional key input and
-- returns @Maybe (key, values)@. A @MaybeT@ implentation of the function is
-- also provided.
--
class GetEtlFragment collection k values | collection -> k values where

  -- |
  -- Minimum implementation: how to use a key from anywhere to pull
  -- a specific fragment (values) from a collection of fragments.
  --
  getValues :: Ord k
            => collection -> k -> Maybe values

  -- |
  -- collection of fragments -> fragment using a key from a request object.
  --
  getEtlFragment :: (Ord k, RequestKey request)
            => collection
            -> (Text -> k)
            -> request
            -> Maybe (k, values)

  getEtlFragment coll mkKey req = do
     keyReq  <- requestKey req
     values' <- getValues coll (mkKey keyReq)
     pure (mkKey keyReq, values')

  -- |
  -- MaybeT version
  getEtlFragmentT :: (Ord k, RequestKey request, Monad m)
            => collection
            -> (Text -> k)
            -> request
            -> MaybeT m (k, values)

  getEtlFragmentT coll mkKey req = MaybeT . pure $ getEtlFragment coll mkKey req



---------------------------------------------------------------------------------
