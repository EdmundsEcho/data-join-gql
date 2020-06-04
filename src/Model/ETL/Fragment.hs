{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Model.ETL.Key
-- Description : Describes the status of a 'Model.Request'
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
module Model.ETL.Fragment where

---------------------------------------------------------------------------------
import           Protolude                 hiding (from, get, null)
---------------------------------------------------------------------------------
import           Model.ETL.FieldValues     (FieldValues (..))
import           Model.ETL.Key             (Key)
import           Model.ETL.Span            (Span (Span))
import qualified Model.ETL.Span            as Span
import           Model.ETL.TagRedExp
---------------------------------------------------------------------------------
import qualified Data.Set                  as Set (fromList, intersection, null,
                                                   size, toList)
---------------------------------------------------------------------------------
import           Control.Monad.Logger
import           Control.Monad.Trans.Maybe
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.Request   (RequestKey (..))
---------------------------------------------------------------------------------
--
-- |
-- Unifying classtype for a fragment of ETL data ~ collection
-- without a key.
--
class Fragment fragment where
  null     :: fragment -> Bool
  len      :: fragment -> Int


instance Fragment a => Fragment (Maybe a) where
  null Nothing   = True
  null (Just vs) = null vs
  len Nothing   = 0
  len (Just vs) = len vs

instance Fragment [a] where
  null [] = True
  null _  = False
  len [] = 0
  len xs = length xs

instance Fragment FieldValues where
  null (TxtSet set)  = Set.null set
  null (IntSet set)  = Set.null set
  null (SpanSet set) = Set.null set
  null Empty         = True

  len (TxtSet set)  = Set.size set
  len (IntSet set)  = Set.size set
  len (SpanSet set) = Set.size set
  len Empty         = 0

-- ** Overview
--
-- Means to extract 'Model.ETL.FieldValues'
--
class ToList a item where -- | item -> a where
  toList :: a -> [item]

instance ToList FieldValues Text where
  toList (TxtSet vs) = Set.toList vs
  toList _           = panic $ message "toList" "Text"

instance ToList FieldValues Int where
  toList (IntSet vs) = Set.toList vs
  toList _           = panic $ message "toList" "Int"

instance ToList FieldValues Span where
  toList (SpanSet vs) = Set.toList vs
  toList _            = panic $ message "toList" "Span"

message :: Text -> Text -> Text
message func' type' = "ToList type mismatch:\n Called "
             <> type' <> " " <> func' <> " with the wrong input data type."

class FromList fragment item | item -> fragment where
   fromList :: [item] -> fragment

instance FromList FieldValues Text where
  fromList = TxtSet . Set.fromList

instance FromList FieldValues Int where
  fromList = IntSet . Set.fromList

instance FromList FieldValues Span where
  fromList = SpanSet . Span.fromListEtl

class Intersection a where
  intersection  :: a -> a -> a

instance Intersection FieldValues where
  intersection (TxtSet get)  (TxtSet from)  = TxtSet  $ Set.intersection  get from
  intersection (IntSet get)  (IntSet from)  = IntSet  $ Set.intersection  get from
  intersection (SpanSet get) (SpanSet from) = SpanSet $ Span.intersection get from
  intersection _ _ = panic "Type mismatch: intersection using different types"

-- ** FieldCount
-- |
-- === Overview
-- This is a /fullfilled/ 'Model.Request' concept. 'fieldCount'
-- reports the number of fields a fullfilled request generates
-- in the matrix. The count does not include derived fields.
--
class FieldCount fragment where

  fieldCount  :: fragment -> Int

  -- debugging capacity
  fieldCountM :: (MonadLogger m, Show fragment)
              => fragment -> m Int

  fieldCountM fragment = do
    let mess = ("fieldCount: "::Text) <> show (fieldCount fragment)
             <> "\nfragment:\n" <> show fragment
    logDebugN mess
    pure $ fieldCount fragment

-- |
--
instance FieldCount a  => FieldCount (Maybe a) where
  fieldCount Nothing  = 0
  fieldCount (Just v) = fieldCount v

-- |
-- ==== Contribution to the field count
-- 'Model.ETL.Span' is the only ETL construct that also needs implement this
-- typeclass.  This location for the implementation is to avoid the
-- orphan instance error.
--
-- Other implementations can be found in the 'Model.Request' module.
--
instance FieldCount Span where
  fieldCount Span { span = span' } = case span' of
    (Red _)     -> 1
    (Exp range) -> Span.rangeLength range

-- ** FieldCounts
-- |
-- === Overview
-- When the GQL constucts require item specific information where the @Model@
-- only represents the data in a @Map@ or other collection, the typeclass
-- provides access to the multiple meta-data values by way of @[(key, fieldCount)]@.
-- e.g., 'Model.Request.ReqComponents' implements
-- 'fieldCounts' @:: [(compName, fieldCount)]@. This may be
-- the only construct that benefits from implementing the typeclass.
--
-- Minimum implementation @fieldCounts@
--
class FieldCounts collection where
  fieldCounts :: collection -> [(Key, Int)]
  getCount    :: Key -> collection -> Int
  getCount key vs = maybe 0 snd (find (\ (k, _) -> key == k) (fieldCounts vs))


-- ** GetEtlFragment
-- |
-- === Overview
-- This is an ETL concept. Not a Request concept /per se/.
-- ETL collection -> item
--
-- === Benefit
-- Provides the 'getEtlFragment' function that processes optional key input and
-- returns @Maybe (key, values)@.  A @MaybeT@ implementation of the function is
-- also provided.
--
class GetEtlFragment collection k values | collection -> k values where

  -- |
  -- Minimum implementation: how to use a key from anywhere to pull
  -- a specific fragment (values) from a collection of fragments.
  --
  getValues :: Ord k => collection -> k -> Maybe values

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
  --
  getEtlFragmentT :: (Ord k, RequestKey request, Monad m)
            => collection
            -> (Text -> k)
            -> request
            -> MaybeT m (k, values)

  getEtlFragmentT coll mkKey req = MaybeT . pure $ getEtlFragment coll mkKey req



---------------------------------------------------------------------------------
