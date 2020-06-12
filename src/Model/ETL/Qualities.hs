{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Model.ETL.Qualities
-- Description : Qualities of a 'Subject'
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
-- |
--
-- 'Qualities' describe the properties of a 'Subject'.  A 'Subject' can have
-- several 'Qualities'.  A 'Subject' is a __product__ made up of 'Qualities'.
--
module Model.ETL.Qualities
  ( module Model.ETL.Qualities

  -- * re-exports
  , QualKey
  , QualValues
  , mkQualKey
  )
  where
---------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Aeson            (ToJSON)
import           Data.Coerce
-------------------------------------------------------------------------------
import           Data.Map.Strict       (keys, union)
import qualified Data.Map.Strict       as Map (lookup, null, size, toList)
-------------------------------------------------------------------------------
import           Model.ETL.FieldValues
import           Model.ETL.Key
-------------------------------------------------------------------------------
-- *** Qualities
-- | The @Qualities@ node is a @Map@ with
-- @ Key :: QualKey @ and
-- @ Value (child) :: Set of QualValues @
--
-- /Note/: QualValues are FieldValues.
--
newtype Qualities = Qualities
        { qualities :: Map Key QualValues
        } deriving (Show, Eq, Ord, Generic)

instance ToJSON Qualities

-- |
-- Shallow, left-bias union
--
instance Semigroup Qualities where
  (Qualities a) <> (Qualities b) = Qualities $ union a b

-- |
-- Shallow, left-bias union
--
instance Monoid Qualities where
  mempty = Qualities mempty
  (Qualities a) `mappend` (Qualities b) = Qualities $ union a b


-- |
-- Utilized by "Model.Matrix.Expression" to generate field names
getQualityNames :: Qualities -> [Text]
getQualityNames = names . qualities

null :: Qualities -> Bool
null (Qualities vs) = Map.null vs

size :: Qualities -> Int
size (Qualities vs) = Map.size vs

lookup :: Qualities -> QualKey -> Maybe QualValues
lookup o = flip Map.lookup (qualities o)

toList :: Qualities -> [(QualKey, QualValues)]
toList = Map.toList . coerce

-- | GQL documentation support
qualsDes :: Text
qualsDes = "A Map collection of the Qualities for the Subject.\n\
          \ Key :: QualKey - Quality name (generally, FieldName)\n\
          \ Value :: A Set collection of the values (Field Values or Levels)"
{-# DEPRECATED qualsDes "Use the gql schema instead" #-}

-- | Private support
names :: Map Key vs -> [Text]
names mp = unKey <$> keys mp
