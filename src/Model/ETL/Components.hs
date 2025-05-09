{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module     : Model.ETL.Components
-- Description: Components of a Measurement
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Components describe the slices of a Measurement in the
-- 'Measurements' collection.
--
module Model.ETL.Components

  ( module Model.ETL.Components

  -- * re-exports
  , CompKey
  , CompValues
  , mkCompKey
  )
  where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Aeson            (ToJSON)
import           Data.Coerce
---------------------------------------------------------------------------------
import           Data.Map.Strict       (keys, union)
import qualified Data.Map.Strict       as Map (lookup, null, size, toList)
-------------------------------------------------------------------------------
import           Model.ETL.FieldValues
import           Model.ETL.Key
-------------------------------------------------------------------------------

-- * Components
-- | The Components node is a @Map@ with
-- @ Key :: CompKey @ and
-- @ Value (child) :: Set of CompValues @
--
-- /Note/: @CompValues@ are @FieldValues@.
newtype Components = Components
        { components :: Map Key CompValues
        } deriving (Show, Eq, Ord, Generic)

instance ToJSON Components

-- |
-- Shallow, left-bias union
--
instance Semigroup Components where
  (Components a) <> (Components b) = Components $ union a b

-- |
-- Shallow, left-bias union
--
instance Monoid Components where
  mempty = Components mempty
  mappend (Components a) (Components b) = Components $ union a b

-- |
-- Used to augment a request that only includes MeaKey
--
getComponentNames :: Components -> [Text]
getComponentNames = names . components

null :: Components -> Bool
null (Components cs) = Map.null cs

size :: Components -> Int
size (Components cs)  = Map.size cs

lookup :: Components -> CompKey -> Maybe CompValues
lookup o = flip Map.lookup (components o)

toList :: Components -> [(CompKey, CompValues)]
toList = Map.toList . coerce

-- | GQL documentation support
comsDes :: Text
comsDes = "A Map collection of the Components for a given Measurement.\n\
          \ Key :: CompKey - Component name (generally, FieldName)\n\
          \ Value :: A Set collection of the values (Field Values or Levels)\n\
          \ Note: Use SpanKey to retrieve the available time span values."
{-# DEPRECATED comsDes "Use the gql schema instead" #-}

-- | Private support
names :: Map Key vs -> [Text]
names mp = unKey <$> keys mp
