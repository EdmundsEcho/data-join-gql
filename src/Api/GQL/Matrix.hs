{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module     : Api.GQL.Matrix
-- Description : Interpretation of 'Model.Request' to generate the data Matrix
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
module Api.GQL.Matrix
  where
---------------------------------------------------------------------------------
import           Protolude              hiding (toList)
---------------------------------------------------------------------------------
import           Control.Monad.Logger
import           Data.Maybe             (fromJust)
---------------------------------------------------------------------------------
import qualified Model.ETL.FieldValues  as Values (FilterRange, areSpanValues,
                                                   filterEnd, filterStart,
                                                   mkFilterRange)
import           Model.ETL.Fragment
import qualified Model.ETL.ObsETL       as Model (FieldValues (..), Key, MeaKey,
                                                  unKey)
import qualified Model.ETL.TagRedExp    as Tag
import           Model.ETL.Transformers (fromReqComponents, fromReqQualities)
import qualified Model.Request          as Model (CompReqValues (..),
                                                  ComponentMixes (..),
                                                  QualityMix (..),
                                                  ReqComponents (..),
                                                  ReqQualities (..),
                                                  Request (..),
                                                  getReqQualityNames, isExp,
                                                  isRed, toCompValuesList,
                                                  toListComponentMixes)
import           Model.Status
---------------------------------------------------------------------------------
import           Api.GqlHttp
---------------------------------------------------------------------------------
import qualified Api.GQL.ObsETL         as Shared
import qualified Api.GQL.Schemas.Matrix as GqlType
---------------------------------------------------------------------------------
  --
---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- Request data types -> Types specified in the schema
-- Request (Resolver o () AppObs)
--
resolverMatrix :: GraphQL o => Model.Request 'Success -> Object o GqlType.Matrix
resolverMatrix req@Model.Request {..} = do

  lift . logDebugN $ ("\n--------------\n"::Text)
  logger subReq
  lift . logDebugN $ ("\n--------------\n"::Text)
  logger meaReqs
  lift . logDebugN $ ("\n--------------\n"::Text)

  pure $
    GqlType.Matrix

      { subExpression =  resolverSubExp  subReq
        -- :: GraphQL o => Object o GqlType.SubExpression

      , meaExpressions = resolverMeaExps meaReqs
        -- :: GraphQL o => ArrayObject o GqlType.MeaExpressions

      , header = pure Nothing
        -- :: GraphQL o => OptionalArrayObject o GqlType.FieldName

      , fieldCount = pure $ fieldCount req

      }

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- QualityMix -> Expression
--
-- From ~ EtlUnit -> Matrix
--   * Minimum:   ReqQualities -> Empty
--   * Generally: ReqQualities -> Filter [QualityName IN Values]
--
resolverSubExp :: GraphQL o => Model.QualityMix -> Object o GqlType.Expression
resolverSubExp mix = pure $
  GqlType.Expression {
      source  = resolverSource (Model.subjectType mix)
    , filter  = mkFilter mReqQuals  -- relations: [Relation]
    , fields  = mkFields            -- [FieldName!]!
    , reducer = pure Nothing
  }
  where
    mReqQuals :: Maybe Model.ReqQualities
    mReqQuals = Model.qualityMix mix

    fields :: [Text]
    fields = fromJust $ Just ["SUB"] <|> Model.getReqQualityNames <$> mReqQuals

    mkName :: GraphQL o => Text -> Object o GqlType.FieldName
    mkName = pure . GqlType.FieldName . pure

    mkFields :: GraphQL o => OptionalArrayObject o GqlType.FieldName
    mkFields = Just <$> traverse mkName fields

    mkFilter :: GraphQL o => Maybe Model.ReqQualities
             -> OptionalObject o GqlType.Filter
    mkFilter = traverse (pure . GqlType.Filter . mkRelations)

    mkRelations :: GraphQL o => Model.ReqQualities
                -> ArrayObject o GqlType.Relation
    mkRelations reqQuals =
      traverse identity (fromReqQualities resolverRelation reqQuals)


-----------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- :: Model.ComponentMixes -> [GqlType.ComponentMix!]
--
resolverMeaExps :: GraphQL o
                => Model.ComponentMixes
                -> ArrayObject o GqlType.Expression
resolverMeaExps mixes
                = traverse identity
                . concat
                $ resolverComponentMix
                <$> Model.toListComponentMixes mixes

-- ** ComponentMix -> [Expression]
-- |
-- Interpret the 'Model.Request' to instantiate the @Matrix@.
--
-- Maybe ReqComponents
-- * Nothing => Include Measurement with no filters
--
-- Maybe CompReqValues
-- * Nothing => CompReqValues Expressed All FieldValues
--
-- ReqComponents -> [Filter] 1:N
-- [Filter] -> [Exp]         N:N
--
-- CompKey, ReqCompValues -> [Relation] :: distributeCompNameOverValues
-- [[Relation]] -> [[Relation]]         :: cartProd
--
-- [Relation] -> Filter
-- Filter   -> Exp           1:1
--
resolverComponentMix :: GraphQL o
                     => (Model.MeaKey, Maybe Model.ReqComponents)
                     -> [Object o GqlType.Expression]
resolverComponentMix mix =
  case snd mix of
     Just vs -> mkExps (fst mix) (mkFilters vs)
     Nothing -> [pure $ GqlType.Expression { source = resolverSource (fst mix)
                                    , filter = pure Nothing
                                    , fields = pure Nothing
                                    , reducer = resolverReducer
                                    }]

  where

    mkFilters :: GraphQL o => Model.ReqComponents -> [Object o GqlType.Filter]
    mkFilters vs = fmap mkFilter (temp vs)

    temp :: GraphQL o => Model.ReqComponents -> [[Object o GqlType.Relation]]
    temp = cartProd . fromReqComponents interpretExpRed
           -- [Filters] ~ [[Relation]]
           --
    mkFilter :: GraphQL o => [Object o GqlType.Relation] -> Object o GqlType.Filter
    mkFilter rls = pure $ GqlType.Filter { relations = traverse identity rls }


    mkExps :: (GraphQL o) => Model.Key -> [Object o GqlType.Filter]
           -> [Object o GqlType.Expression]
    mkExps src filters =
      fmap (\flt -> GqlType.Expression { source  = resolverSource src
                                  , filter  = pure $ Just flt
                                  , fields  = pure Nothing    -- TODO: create fields/header
                                  , reducer = resolverReducer
                                  }) <$> filters

---------------------------------------------------------------------------------
-- ** Interpretation of 'Model.ETL.TagExpRed'
-- |
-- The expression of what the 'Model.ETL.TagExpRed' means for
-- 'Model.ETL.FieldValues' is in the number and grouping of
-- 'Api.GQL.Schemas.Matrix.Filter'.
--
-- e.g., @ payers {"CASH","PRIVATE"} -> payers "CASH", payers "PRIVATE" @
--
-- /Note/: SpanValues use this tag twice. The @Tag FieldValues@ should not
-- impact the ultimate interpretation of the request. TODO: Test accordingly.
--
-- Each pair is a 'Relation'. Number of @Relations@ generated = Number of Values
--
-- Tasks:
--
--   (1) Distribute a copy of the LHS 'Api.GQL.Schemas.Matrix.FieldName' to
--   each in the collection of RHS values.  RHS values are instantiated using
--   'Model.ETL.FieldValues'.
--
--    2. Send result to 'resolverRelation'
--
--       * (Key, FieldValues)
--
-- This computation is specific for the measurement arm of the request where the
-- task involves:
--
--    Long View -> Wide View transformation
--
interpretExpRed :: GraphQL o
                 => Model.Key -> Model.CompReqValues
                 -> [Object o GqlType.Relation]

interpretExpRed fldName vs

  | Model.isExp vs = uncurry resolverRelation <$>
       repeat fldName `zip` Model.toCompValuesList vs

        -- CompReqValues Exp FieldValues fromList ["cash", "medicare"]
        -- Relation payer EQ ["cash"]
        -- Relation payer EQ ["medicare"]

  | Model.isRed vs =
       let Model.CompReqValues tvs = vs
        in [resolverRelation fldName (Tag.unTag tvs)]

        -- CompReqValues Red FieldValues fromList ["cash", "medicare"]
        -- Relation payer IN ["cash", "medicare"]

  | otherwise = panic "interpretExpRed should not have reached here"

---------------------------------------------------------------------------------
-- ** Relation (LHS relationSymbol RHS)
-- |
-- input FieldValues
-- Generic
--
-- Component perspective:
--
-- Called by interpretExpRed where
-- RHS is a list with one value (Expressed) or many values (Reduced)
--
resolverRelation :: GraphQL o => Model.Key -> Model.FieldValues
                 -> Object o GqlType.Relation

resolverRelation key vs' = pure $
  GqlType.Relation {
      lhs      = resolverLHS key
    , rhs      = resolverRHS vs'   -- TxtValues, IntValues, SpanValues
    , relation = resolverRelationSymbol vs'
  }
  where
     resolverRelationSymbol vs
       | len vs == 1 = pure GqlType.EQ
       | otherwise  = pure GqlType.IN

     ----------------------------------------------------------------------------
     -- input Key
     --
     resolverLHS :: GraphQL o => Model.Key -> Object o GqlType.LHS
     resolverLHS = pure . GqlType.LHS
                 . pure . GqlType.FieldName
                 . pure . Model.unKey
     ----------------------------------------------------------------------------
     -- input FieldValues
     --
     resolverRHS :: GraphQL o => Model.FieldValues -> Object o GqlType.RHS
     resolverRHS (Model.TxtSet o')  = GqlType.RHSTxtValues
                                    <$> Shared.resolverTxtValues (Model.TxtSet o')

     resolverRHS (Model.IntSet o')  = GqlType.RHSIntValues
                                    <$> Shared.resolverIntValues (Model.IntSet o')

     resolverRHS (Model.SpanSet o') = GqlType.RHSSpanFilters
                                    <$> resolverSpanFilters (Model.SpanSet o')

     resolverRHS Model.Empty = panic "Tried to build RHS with Empty"

---------------------------------------------------------------------------------
-- ** Span Filters
-- |
-- input SpanValues :: FieldValues
--
-- Steps to building RHS for Span values
--
-- 1. Start with SpanSet of Model.Span
-- 2. Set Span -> [FilterRange]
-- 3. Model [FilterRange] -> GQL [FilterRange]
--
-- > type SpanFilters {
-- >   filters: [FilterRange!]!
-- > }
-- > type FilterRange {
-- >   filterStart: Int!
-- >   filterEnd: Int!
-- > }
--
resolverSpanFilters :: GraphQL o => SpanValues -> Object o GqlType.SpanFilters
resolverSpanFilters vs
  | Values.areSpanValues vs = pure . GqlType.SpanFilters $ resolverFilterRanges
  | otherwise = panic "Tried to use a Span resolver with non-span values."

    where

      resolverFilterRanges :: GraphQL o => ArrayObject o GqlType.FilterRange
      resolverFilterRanges = traverse identity $ pure . mkFilterRange <$> listOfFilters

      listOfFilters :: [Values.FilterRange]
      listOfFilters = concat $ Values.mkFilterRange <$> toList vs

      mkFilterRange :: Applicative f => Values.FilterRange -> GqlType.FilterRange f
      mkFilterRange = GqlType.FilterRange
        <$> pure . Values.filterStart
        <*> pure . Values.filterEnd

type SpanValues = Model.FieldValues

---------------------------------------------------------------------------------
-- |
-- input Constant
-- TODO: This needs to be a configurable value.
--
resolverReducer :: Applicative f => f (Maybe GqlType.ReducerEnum)
resolverReducer = pure $ Just GqlType.SUM

---------------------------------------------------------------------------------
-- |
-- input Key
-- Generic
--
-- ETLTable is a source in the ETL wharehouse.  Tables in the warehouse are
-- organized by @etlUnit@.  These units represent valid functions that
-- describe the 'Model.ETL.ObsETL' data. The identifying feature is the
-- codomain (output) of the function, thus the name of the source.
--
-- * type Quality
--
--      @ Subject -> Quality @
--
-- * type Measurement
--
--      @ Subject -> zero+ [Component] -> SpanType -> Measurement Value
--
resolverSource :: GraphQL o => Model.Key -> Object o GqlType.ETLTable
resolverSource = pure . GqlType.ETLTable . pure . Model.unKey

---------------------------------------------------------------------------------
-- |
-- Part of the [Components] Long -> Wide view computation
--
cartProd :: [[a]] -> [[a]]
cartProd [] = [[]]
cartProd (xs:xss) = [x:ys | x<- xs, ys <-yss]
                        where yss = cartProd xss



---------------------------------------------------------------------------------
  --
