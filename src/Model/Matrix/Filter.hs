{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module     : Model.Matrix.Filter
-- Description : ~ SQL Where clause
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
module Model.Matrix.Filter
  (
    Filter(..)
  , Relation(Relation)   -- uses PatternSynonym to keep Relation_ private
  , lhs
  , rhs
  , relation
  , LHS
  , mkLHS
  , unLHS
  , RHS(..)    -- TODO: somehow make constructors private
  , showFilter
  , RelSymbol

  -- re-export of Span FilterValue
  , FilterRange(FilterRange) -- pattern synonym
  , mkFilterRange            -- smart constructor
  , filterStart              -- record
  , filterEnd                -- record
  )
  where

---------------------------------------------------------------------------------
import           Protolude                     hiding (EQ, intercalate)
---------------------------------------------------------------------------------
import           Data.Aeson.Types
import           Data.Text                     (intercalate)
---------------------------------------------------------------------------------
import           Model.ETL.ObsETL              (FieldValues (..),
                                                FilterRange (FilterRange), Key,
                                                filterEnd, filterSize,
                                                filterStart, len, mkFilterRange,
                                                showFilterRange)
import           Model.Matrix.Filter.FieldName
---------------------------------------------------------------------------------

-- ** Overview
-- |
--
-- This module defines the types required to express the equivalent
-- of the `Where` clause in `SQL`.
--
newtype Filter = Filter { unFilter :: [Relation] }
  deriving (Show, Eq, Generic)

instance ToJSON Filter

showFilter :: Filter -> Text
showFilter (Filter flt) = intercalate "|" $ showRelation <$> flt -- "[" <> "]"
-- payer:medicaid|span:1_1
-- LHS:RHS|LHS:RHS

-- |
-- Relation = LHS Relation RHS
-- Used by a filter that describes the values required by the matrix
-- The relation is a Sum type IN | EQ and can be derived by the shape of the RHS
-- RHS -> Relation
--
data Relation = Relation_
  { lhs      :: !LHS
  , rhs      :: !RHS
  , relation :: !RelSymbol
  } deriving (Show, Eq, Generic)

instance ToJSON Relation

-- | Relation constructor (derives the relation) {{{
-- The data that inform the relation are of two types:
-- QualityMix
-- ComponentMix
-- }}}
pattern Relation :: LHS -> RHS -> Relation
pattern Relation l r <- Relation_ l r _ where
  Relation lhs rhs = Relation_ lhs rhs relation
     where relation | size rhs == 1 = EQ
                    | otherwise    = IN
{-# COMPLETE Relation :: Relation #-}

-- | Private constructors
data RelSymbol = IN | EQ
  deriving (Show, Eq, Generic)

instance ToJSON RelSymbol

-- | RHS hosts the requested FieldValues (either QualValues or CompValues).{{{
-- Like FieldValues, it is a unifying data type required to process collections
-- of Relations in a single computational stream.
-- TODO: Design a smart constructor that limits input to FieldValues or a
-- derivative thereof.}}}
data RHS
  = RhsValues !FieldValues          -- QualValues
  | RhsTxt    !Text                 -- CompValue
  | RhsInt    !Int32                -- CompValue
  | RhsSpan   !FilterRange          -- Span used for Filter
  deriving (Show, Eq, Generic)

instance ToJSON RHS

-- | size is used to determine the RelSymbol IN or EQ
size :: RHS -> Int
size (RhsValues vs) = len vs       -- QualValues
size (RhsTxt _)     = 1              -- CompValue
size (RhsInt _)     = 1              -- CompValue
size (RhsSpan r)    = filterSize r   -- CompValue, Span -> FilterRange

-- | FieldName in a Relation comes from QualName or CompName (both :: Key)
-- Use a smart constructor to limit what is used as input
-- FieldName refers to a field in the ETL store; a SubKey or MeaKey
newtype LHS = LHS { unLHS :: FieldName } deriving (Show, Eq, Generic)

instance ToJSON LHS

-- | Constructor that limits the source.
mkLHS :: Key -> LHS
mkLHS = LHS . mkFieldName

-- | Utilized to create a field name for the Matrix specification.
-- Field name here is refering to what is expected in the Matrix Header
showRelation :: Relation -> Text
showRelation (Relation (LHS name) (RhsTxt  t)   ) = name <> ":" <> t
showRelation (Relation (LHS name) (RhsInt  i)   ) = name <> ":" <> show i
showRelation (Relation (LHS name) (RhsSpan s)   ) = name <> ":" <> showFilterRange s
showRelation (Relation (LHS name) (RhsValues vs)) = name <> ":" <> show vs
