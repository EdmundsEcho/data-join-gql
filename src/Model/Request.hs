{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PatternSynonyms       #-}

{-|
  = Overview

    The @Request@ is instantiated from @ObsETL@, user input and a validation function.

    @ ObsETL -> Request -> Matrix @

  = Summary

   This module echos the @ObsETL@ structure transformed by the user request.
   The @Request@ data structure is similar to but different than the @ObsETL@.

-}
module Model.Request
  (
  -- * Request
    Request(..)
  , validate

  -- * Branches
  -- ** Quality-related
  , QualityMix (..)
  , ReqQualities (..)
  , mkQualityMix
  , fromListReqQualities
  , toListReqQualities
  , minQualityMix
  , minSubResult

  -- ** Component-related
  , ComponentMixes (componentMixes)
  , fromListComponentMixes
  , toListComponentMixes
  , ReqComponents(reqComponents)
  , fromListReqComponents
  , toListReqComponents
  , fromComponents
  , fromCompValues

  -- ** FieldValues-related
  , CompReqValues (..)
  , fromFieldsCompReqValues
  , toTupleCompReqValues
  , ToQualValues(..)

  )
  where

---------------------------------------------------------------------------------
import           Protolude             hiding (null)
---------------------------------------------------------------------------------
import           Data.Aeson            (ToJSON)
---------------------------------------------------------------------------------
import           Data.Map.Strict       (union)
import qualified Data.Map.Strict       as Map (fromList, null, size, toList)
---------------------------------------------------------------------------------
import           Model.ETL.Components
import           Model.ETL.FieldValues
import           Model.ETL.Fragment
import           Model.ETL.Key
import           Model.ETL.TagRedExp
import           Model.Status
---------------------------------------------------------------------------------

-- * Request

-- | Request is a subset of the ObsETL
-- @ ~validate :: RequestInput -fetch-> Request @
data Request (status::Status) = Request
  { subReq  :: !QualityMix
  , meaReqs :: !ComponentMixes
  -- , status  :: Proxy 'Inprocess
  } deriving (Show, Eq, Generic)

instance ToJSON (Request status)

-- |
-- A placeholder for enforcing validation using the type-system.
--
validate :: Maybe (Request 'Inprocess) -> Maybe (Request 'Success)
validate (Just Request {..}) = Just $ Request {..}
validate Nothing             = Nothing

-- ** Branches

-- | @ map :: ETL values -> Requested values @
data QualityMix = QualityMix
  { subjectType :: !Key
  , qualityMix  :: !(Maybe ReqQualities)
  } deriving (Show, Eq, Generic)

instance ToJSON QualityMix

-- |
--
minQualityMix, minSubResult :: SubKey -> QualityMix
minQualityMix key
  = QualityMix { subjectType = key
               , qualityMix = Nothing
               }
minSubResult = minQualityMix

-- *** ReqQualities
-- |
-- Encoding
-- - QualKey Nothing => display the quality field, select all levels
-- - QualKey Just vs => display the quality field, select levels
--
-- /Note/: QualValues are FieldValues.
--
newtype ReqQualities = ReqQualities
        { reqQualities :: Map QualKey (Maybe QualValues)
        } deriving (Show, Eq, Ord, Generic)

instance Fragment ReqQualities where
  null (ReqQualities vs) = Map.null vs
  len  (ReqQualities vs) = Map.size vs

instance ToJSON ReqQualities

fromListReqQualities :: [(QualKey, Maybe QualValues)] -> ReqQualities
fromListReqQualities = ReqQualities . Map.fromList

toListReqQualities :: ReqQualities -> [(QualKey, Maybe QualValues)]
toListReqQualities (ReqQualities vs) = Map.toList vs

-- |
-- Limited to matching on key values with left bias
-- TODO: Prioritize bias using the value of Maybe
instance Semigroup ReqQualities where
  (ReqQualities a) <> (ReqQualities b) = ReqQualities $ union a b

-- | Left bias
instance Monoid ReqQualities where
  mempty = ReqQualities mempty
  (ReqQualities a) `mappend` (ReqQualities b) = ReqQualities $ union a b

-- |
-- Smart constructor.
--
mkQualityMix :: Key -> ReqQualities -> QualityMix
mkQualityMix key@(SubKey _) vs = QualityMix key (Just vs)
mkQualityMix _              _  = panic "mkQualityMix: Tried with wrong type."

-- |
--
newtype ComponentMixes = ComponentMixes
        { componentMixes :: Map CompKey (Maybe ReqComponents) -- MeaType
        } deriving (Show, Eq, Ord, Generic)

instance Fragment ComponentMixes where
  null (ComponentMixes vs) = Map.null vs
  len  (ComponentMixes vs) = Map.size vs

instance ToJSON ComponentMixes

-- |
-- Constructor
fromListComponentMixes :: [(MeaKey, Maybe ReqComponents)] -> ComponentMixes
fromListComponentMixes = ComponentMixes . Map.fromList

-- |
-- toList
toListComponentMixes :: ComponentMixes -> [(MeaKey, Maybe ReqComponents)]
toListComponentMixes (ComponentMixes v) = Map.toList v

instance Semigroup ComponentMixes where
  (ComponentMixes a) <> (ComponentMixes b) = ComponentMixes $ union a b

instance Monoid ComponentMixes where
  mempty = ComponentMixes mempty
  mappend (ComponentMixes a) (ComponentMixes b) = ComponentMixes $ union a b

-- * ReqComponents
-- |
-- Encoding
-- - CompKey Nothing => create a series of fields using all levels (Exp)
-- - CompKey Just vs =>
--     - Exp: create a series using the levels specified
--     - Red: create a single summary field using the levels specified
--
-- /Note/: @CompReqValues@ are @FieldValues@ tagged using @TagRedExp@.
--
newtype ReqComponents = ReqComponents
        { reqComponents :: Map Key (Maybe CompReqValues)  -- CompKey
        } deriving (Show, Eq, Ord, Generic)

instance Fragment ReqComponents where
  null (ReqComponents vs) = Map.null vs
  len  (ReqComponents vs) = Map.size vs

instance ToJSON ReqComponents

instance Semigroup ReqComponents where
  (ReqComponents a) <> (ReqComponents b) = ReqComponents $ union a b

instance Monoid ReqComponents where
  mempty = ReqComponents mempty
  mappend (ReqComponents a) (ReqComponents b) = ReqComponents $ union a b

-- |
-- Constructor
fromListReqComponents :: [(CompKey, Maybe CompReqValues)] -> ReqComponents
fromListReqComponents = ReqComponents . Map.fromList

-- |
-- toList
toListReqComponents :: ReqComponents -> [(CompKey, Maybe CompReqValues)]
toListReqComponents (ReqComponents vs) = Map.toList vs

-- |
-- Api helper
fromComponents :: (CompValues -> TagRedExp CompValues) -> Components -> ReqComponents
-- unwrap CompValues
-- wrap with Tag
-- how access CompValues inside Components? fmap
fromComponents redExp o =
  ReqComponents .  fmap (Just . CompReqValues . redExp) $ components o

-- | Wrapper to express Reduced vs Expressed request computation
--   of the associated measurement value. So, just the same FieldValues
--   plus an extra tag.  This fits throughout further down the Request tree.
newtype CompReqValues = CompReqValues { values :: TagRedExp CompValues }
  deriving (Show, Eq, Ord, Generic)

instance Fragment CompReqValues where
  null (CompReqValues (Red vs)) = null vs
  null (CompReqValues (Exp vs)) = null vs
  len  (CompReqValues (Red vs)) = len vs
  len  (CompReqValues (Exp vs)) = len vs

instance ToJSON CompReqValues

toTupleCompReqValues :: CompReqValues -> (CompValues, Reduced)
toTupleCompReqValues (CompReqValues (Red vs)) = (vs, True)
toTupleCompReqValues (CompReqValues (Exp vs)) = (vs, False)

-- |
--
fromCompValues :: (CompValues -> TagRedExp CompValues)
               -> CompValues -> CompReqValues
fromCompValues redExp = CompReqValues . redExp

fromFieldsCompReqValues :: Reduced -> CompValues -> CompReqValues
fromFieldsCompReqValues red vs
  | red       = CompReqValues { values = Red vs }
  | otherwise = CompReqValues { values = Exp vs }

-- | Synonym used to set TagRedExp value
type Reduced = Bool
