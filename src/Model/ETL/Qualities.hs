-- |
-- Module     : Model.ETL.Qualities
-- Description: Qualities of a 'Subject'
--
-- 'Qualities' describe the properties of a 'Subject'.  A 'Subject' can have
-- several 'Qualities'.  A 'Subject' is a __product__ made up of 'Qualities'.
--
module Model.ETL.Qualities
  ( module Model.ETL.Qualities
  -- * Functions
  -- Utilized by 'Model.Matrix.Expression'
  -- , toList        -- Set a -> [a]
  -- , map           -- Set a -> Set b (note: not strictly a Functor)
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
import           Model.ETL.Fragment
import           Model.ETL.Key
import           Model.SearchFragment
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
--
instance GetEtlFragment Qualities QualKey (SearchFragment QualValues 'ETL) where
  getValues qualities k = Map.lookup k (coerce qualities)

-- |
-- Utilized by "Model.Matrix.Expression" to generate field names
getQualityNames :: Qualities -> [Text]
getQualityNames = names . qualities


null :: Qualities -> Bool
null (Qualities vs) = Map.null vs

len :: Qualities -> Int
len (Qualities vs) = Map.size vs

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
