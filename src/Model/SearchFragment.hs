{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Model.SearchFragment
-- Description : Wrapper to tag data source
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
module Model.SearchFragment where

-------------------------------------------------------------------------------
import           Protolude  hiding (null)
-------------------------------------------------------------------------------
import           Data.Aeson (ToJSON)
-------------------------------------------------------------------------------
  --
-- |
-- The host for the state required to enforce the subsetting constraints.
--
-- Early design: likely limited to 'Model.ETL.FieldValues'.
--
newtype SearchFragment a (source :: Source)
  = SearchFragment { fragment :: a } deriving (Show, Eq, Generic)

-- ** Overview
-- |
-- A phantom type used to track data fragments used to compute a user request
-- for ETL data.
--
data Source
     = ETL        -- from Mode.ETL -> FieldValues
     | Req        -- from GqlInput -> FieldValues
     | ETLSubset  -- from FieldValues -> FieldValues -> FieldValues
     deriving (Generic)

-- |
--
instance Semigroup a => Semigroup (SearchFragment a b) where
  SearchFragment a1 <> SearchFragment a2 = SearchFragment $ a1 <> a2

instance ToJSON a => ToJSON (SearchFragment a b)
