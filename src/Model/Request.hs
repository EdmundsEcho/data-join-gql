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

  -- * Branches
  -- ** Quality-related
  , QualityMix (..)

  -- ** Component-related
  , ComponentMix (..)
  , ReqComponents (..)

  -- ** FieldValues-related
  , CompReqValues (..)
  , ToCompReqValues(..)
  , toTupleCompReqValues
  , ToQualValues(..)

  -- * Constructors
  , mkQualityMix
  , mkComponentMix
  , fromListReqComponents
  , fromFieldsCompReqValues

  )
  where

-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Map.Strict       (union)
import qualified Data.Map.Strict       as Map (fromList)
-------------------------------------------------------------------------------
import           Model.ETL.FieldValues
import           Model.ETL.Key
import           Model.ETL.Qualities   hiding (names)
import           Model.ETL.TagRedExp
-------------------------------------------------------------------------------

-- * Request

-- | Request is a subset of the ObsETL
-- @ ~validate :: RequestInput -fetch-> Request @
data Request = Request
  { subReq  :: !QualityMix
  , meaReqs :: ![ComponentMix]
  } deriving (Show, Eq)

-- ** Branches

-- | @ map :: ETL values -> Requested values @
data QualityMix = QualityMix
  { subjectType :: !Key
  , qualityMix  :: !Qualities
  } deriving (Show, Eq)

-- | Smart constructor.
--
-- TODO: The module exports the constructors thus a circumvention.
mkQualityMix :: Key -> Qualities -> QualityMix
mkQualityMix key@(SubKey _) vs = QualityMix key vs
mkQualityMix _              _  = panic "mkQualityMix: Tried with wrong type."


-- | @ map :: ETL values -> Requested values @
-- Uses CompReqValues
--
-- Measurement Type is also bundled to facilitate referencing Source
-- (Source = Measurement type)
data ComponentMix = ComponentMix
  { measurementType :: !Key
  , componentMix    :: !ReqComponents
  } deriving (Show, Eq)

-- * ReqComponents
-- | The ReqComponents node is a @Map@ with
--
-- /Note/: @CompReqValues@ are @FieldValues@ tagged using @TagRedExp@.
newtype ReqComponents = ReqComponents
        { reqComponents :: Map Key CompReqValues
        } deriving (Show, Eq, Ord, Generic)

instance Semigroup ReqComponents where
  (ReqComponents a) <> (ReqComponents b) = ReqComponents $ union a b

instance Monoid ReqComponents where
  mempty = ReqComponents mempty
  mappend (ReqComponents a) (ReqComponents b) = ReqComponents $ union a b

-- | Smart constructor.
--
-- TODO: The module exports the constructors thus a circumvention.
mkComponentMix :: Key -> ReqComponents -> ComponentMix
mkComponentMix key@(MeaKey _) vs = ComponentMix key vs
mkComponentMix _              _  = panic "mkComponentMix: Tried with wrong type."

fromListReqComponents :: [(CompKey, CompReqValues)] -> ReqComponents
fromListReqComponents = ReqComponents . Map.fromList

-- | Wrapper to express Reduced vs Expressed request computation
--   of the associated measurement value. So, just the same FieldValues
--   plus an extra tag.  This fits throughout further down the Request tree.
newtype CompReqValues = CompReqValues { values :: TagRedExp CompValues }
  deriving (Show, Eq, Ord)

class ToCompReqValues a where
  toCompReqValues :: a -> CompReqValues

toTupleCompReqValues :: CompReqValues -> (CompValues, Reduced)
toTupleCompReqValues (CompReqValues (Red vs)) = (vs, True)
toTupleCompReqValues (CompReqValues (Exp vs)) = (vs, False)

fromFieldsCompReqValues :: Reduced -> CompValues -> CompReqValues
fromFieldsCompReqValues red vs
  | red       = CompReqValues { values = Red vs }
  | otherwise = CompReqValues { values = Exp vs }

-- | Synonym used to set TagRedExp value
type Reduced = Bool
