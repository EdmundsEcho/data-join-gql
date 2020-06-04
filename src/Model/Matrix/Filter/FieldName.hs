{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module     : Model.Matrix.Filter.FieldName
-- Description : Used by 'Filter' as the LHS of a Relation.
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
module Model.Matrix.Filter.FieldName
  ( FieldName
  , mkFieldName
  )
  where
---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import           Model.ETL.ObsETL (Key (CompKey, QualKey, SpanKey), unKey)
---------------------------------------------------------------------------------

type FieldName = Text -- in Models ETL, FieldName is a Key value

mkFieldName :: Key -> FieldName
mkFieldName (QualKey k) = k
mkFieldName (CompKey k) = k
mkFieldName SpanKey     = unKey SpanKey
mkFieldName _ = panic "mkFieldName: Tried to make FieldName with the wrong Key value"
