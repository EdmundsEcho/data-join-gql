{- |
   This module defines the family of functions
   ETL Collections -> [a]
-}
module Model.ETL.Transformers
  where

-------------------------------------------------------------------------------
import           Data.Maybe       (fromJust)
import           Protolude        hiding (toList)
-------------------------------------------------------------------------------
import qualified Data.Map.Strict  as Map (foldrWithKey')
-------------------------------------------------------------------------------
import           Model.ETL.ObsETL
import           Model.Request    (CompReqValues, ReqComponents (..),
                                   toListReqComponents)
-------------------------------------------------------------------------------

-- | Higher-order functions that facilitate the construction of GraphQL types
-- from the underlying Obs data structures.
--
-- Make GraphQL from the Map collection
-- (k -> v -> a) -> Map k v -> [a]
--
-- | Used by GraphQL Subject Type definition
fromQualities :: (QualKey -> QualValues -> a) -> Qualities -> [a]
fromQualities f o = fromMap f (qualities o)   -- o :: Qualities is a record type

-- | Used by GraphQL Obs Type definition
fromMeasurements :: (MeaKey -> Components -> a) -> Measurements -> [a]
fromMeasurements f o = fromMap f (measurements o)

-- | Used by GraphQL Measurement Type definition
-- This needs to be applied to a of the parent...
fromComponents :: (CompKey -> CompValues -> a) -> Components -> [a]
fromComponents f o = fromMap f (components o)

fromReqComponents :: (CompKey -> CompReqValues -> a) -> ReqComponents -> [a]
fromReqComponents f o = fmap (uncurry f) (ppJustValues o)
  where
    ppJustValues :: ReqComponents -> [(CompKey, CompReqValues)]
    ppJustValues o' =
      fmap fromJust <$> filter (\(_,b) -> isJust b) (toListReqComponents o')
-- toListReqComponents :: ReqComponents -> [(CompKey, Maybe CompReqValues)]

-- | Used by GraphQL QualityValues and ComponentValues Type definitions
fromFieldValues :: Ord a =>  ([a] -> b) -> Set a -> b
fromFieldValues f o = f (toList o)

-- |
-- Ideally, this would take 'FieldValues'.  However, the caller must
-- unwrap the values whilst pattern matching on the types defined
-- in the Sum type 'FieldValues'.
valuesToList :: Ord a => Set a -> [a]
valuesToList = toList

-- | Internal support function
{- HLINT ignore fromMap -}
fromMap :: forall k v a . (k -> v -> a) -> Map k v -> [a]
fromMap f = Map.foldrWithKey' foldFn []
  where
    foldFn :: k -> v -> [a] -> [a]
    foldFn k v acc = f k v : acc
