{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module     : Api.GQL.RequestInput
-- Description : ETL source of truth
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- ** Overview
--
-- Top-level ETL Model (where GQL is a view).  This object is instantiated
-- using "Schemas.ETL.Types.ObsETLInput".
--
module Model.ETL.ObsETL
  ( module Model.ETL.ObsETL
  -- * Types re-exported
  , module Model.ETL.ID       -- beginnings of a Node UID generator; now for root only.
  , module Model.ETL.Qualities
  , module Model.ETL.Key
  , module Model.ETL.Components
  , module Model.ETL.FieldValues
  , module Model.ETL.Span
  )
  where
-------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import qualified Data.Map.Strict       as Map (fromList, lookup, null, size,
                                               union)
-- import           Data.Text             (append)
-------------------------------------------------------------------------------
import           Data.Aeson            (ToJSON)
---------------------------------------------------------------------------------
-- import           Lib.NodeManager
import           Model.ETL.Components  hiding (lookup, names, null, size,
                                        toList)
import           Model.ETL.FieldValues
import           Model.ETL.ID
import           Model.ETL.Key
import           Model.ETL.Qualities   hiding (lookup, null, size, toList)
import           Model.ETL.Span        hiding (intersection, subset)
-------------------------------------------------------------------------------
-- = Observation model
-- |
data ObsETL = ObsETL
        { obsID           :: !ID
        , obsSubject      :: !Subject
        , obsMeasurements :: !Measurements
        } deriving (Show, Eq, Ord, Generic)

instance ToJSON ObsETL

-- | Private smart constructor that utilizes an ID generator.
mkObsETL :: ID -> Subject -> Measurements -> ObsETL
mkObsETL = ObsETL
  -- idx <- generateIdx   -- just give me state (moved to value channel)
  --  return $ ObsETL id s ms -- return (a :: ObsETL)

--    where
--       mkID' :: Int32 -> ID
--       mkID' n = NewID $
--         ("OID"::Text) `append` show d3 `append` show d2 `append` show d1
--           where { d3 = n`quot`100; d2 = n`quot`10; d1 = n`quot`1 }

-- == Subject
-- | The @Subject@ node is a record with
-- @ Key :: SubKey @ and
-- @ Value (child) :: Set of QualValues @
data Subject = Subject
        { subType      :: !Key
        , subQualities :: !Qualities
        } deriving (Show, Eq, Ord, Generic)

instance ToJSON Subject

-- == Measurements
-- | The @Measurements@ node is a @Map@ with
-- @ Key :: MeaKey @ and
-- @ Value (child) :: Map of Components @
newtype Measurements = Measurements
        { measurements  :: Map Key Components
        } deriving (Show, Eq, Ord, Generic)

instance ToJSON Measurements

-- |
-- Shallow, left-bias union
--
instance Semigroup Measurements where
  (Measurements a) <> (Measurements b) = Measurements $ Map.union a b

-- |
-- Shallow, left-bias union
--
instance Monoid Measurements where
  mempty = Measurements mempty
  Measurements a `mappend` Measurements b = Measurements $ Map.union a b

null :: Measurements -> Bool
null = Map.null . measurements

size :: Measurements -> Int
size = Map.size . measurements

lookup :: Measurements -> MeaKey -> Maybe Components
lookup vs = flip Map.lookup (measurements vs)

-- | As of yet, unused support function (perhaps for @Matrix@)
meaTypes :: Measurements -> [Text]
meaTypes = names . measurements

-- ** Instantiation
-- |
-- Utilized by 'Api.GQL.ObsETL'
--
measFromList :: Ord k => [(k,vs)] -> Map k vs
measFromList = mapFromList

compsFromList :: Ord k => [(k,vs)] -> Map k vs
compsFromList = mapFromList

qualsFromList :: Ord k => [(k,vs)] -> Map k vs
qualsFromList = mapFromList

mapFromList :: Ord k => [(k,vs)] -> Map k vs
mapFromList = Map.fromList
