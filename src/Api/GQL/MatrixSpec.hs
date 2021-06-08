{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module     : Api.GQL.MatrixSpec
-- Description : Interpretation of Request to generate the data Matrix
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
module Api.GQL.MatrixSpec
  where
---------------------------------------------------------------------------------
import           Protolude                  hiding (intercalate, toList)
---------------------------------------------------------------------------------
import           Control.Monad.Logger
import           Data.Text                  (intercalate)
---------------------------------------------------------------------------------
import qualified Model.ETL.FieldValues      as Values (FieldValues (..),
                                                       FilterRange (..),
                                                       filterEnd, filterStart,
                                                       mkFilterRange)
import           Model.ETL.Fragment         hiding (fieldCount)
import qualified Model.ETL.Fragment         as Fragment (fieldCount)
import qualified Model.ETL.ObsETL           as Model (FieldValues (..), Key,
                                                      MeaKey, unKey)
import qualified Model.ETL.TagRedExp        as Tag
import           Model.ETL.Transformers     (fromReqComponents,
                                             fromReqQualities)
import qualified Model.Request              as Model (CompReqValues (..),
                                                      ComponentMixes (..),
                                                      QualityMix (..),
                                                      ReqComponents (..),
                                                      ReqQualities (..),
                                                      Request (..),
                                                      areSpanValues,
                                                      getReqQualityNames, isExp,
                                                      isRed, toCompValuesList,
                                                      toListComponentMixes)
import           Model.Status
---------------------------------------------------------------------------------
import           Api.GQL.Types
import           WithAppContext
---------------------------------------------------------------------------------
import qualified Api.GQL.ObsETL             as Shared
import           Api.GQL.Schemas.MatrixSpec
import qualified Api.GQL.Schemas.MatrixSpec as GqlType
---------------------------------------------------------------------------------
  --
---------------------------------------------------------------------------------
-- |
-- 'Model.Request' -> 'Api.GQL.MatrixSpec'
--
-- From ~ [EtlUnit] -> Matrix
--
resolverMatrixSpec :: (GraphQL o m, WithAppContext m)
                   => Model.Request 'Success
                   -> Object o m GqlType.MatrixSpec

resolverMatrixSpec req@Model.Request {..} = do

  lift $ logInfoN ("Processing Input Request for MatrixSpec"::Text)

  lift $ logDebugN ("\n------ TOP --------\n"::Text)
  lift $ logDebugF req
  lift $ logDebugN ("\n------- BOTTOM -------\n"::Text)

  pure $
    GqlType.MatrixSpec

      { subExpression =  resolverSubExp  subReq
        -- :: GraphQL o m => Object o GqlType.SubExpression

      , meaExpressions = resolverMeaExps meaReqs
        -- :: GraphQL o m => ArrayObject o GqlType.MeaExpressions

      , header = pure Nothing
        -- :: GraphQL o m => OptionalArrayObject o GqlType.FieldName

      , fieldCount = pure $ Fragment.fieldCount req

      }

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
--
-- Model.QualityMix -> GqlType.Expression
--
--   * Minimum:   ReqQualities -> Empty
--
--   * Generally: ReqQualities -> Filter [QualityName IN Values]
--
resolverSubExp :: (GraphQL o m, MonadLogger m)
               => Model.QualityMix -> Object o m GqlType.Expression
resolverSubExp mix = do

  lift . logDebugN $ ("fields from mReqQuals: "::Text) <> show fields

  pure $
     GqlType.Expression {
         source  = resolverSource (Model.subjectType mix)
       , filter  = mkFilter mReqQuals  -- relations: [Relation]
       , fields  = mkFields            -- [FieldName!]
       , reducer = pure Nothing
     }
  where
    mReqQuals :: Maybe Model.ReqQualities
    mReqQuals = Model.qualityMix mix

    mkName :: GraphQL o m => Text -> Object o m GqlType.FieldName
    mkName = pure . GqlType.FieldName . pure

    mkFields :: GraphQL o m => OptionalArrayObject o m GqlType.FieldName
    mkFields = traverse identity
               (traverse identity . fmap mkName <$> fields)

    fields :: Maybe [Text]
    fields = Model.getReqQualityNames <$> mReqQuals

    mkFilter :: GraphQL o m => Maybe Model.ReqQualities
             -> OptionalObject o m GqlType.Filter
    mkFilter = traverse
      (\vs -> pure $ GqlType.Filter
               { relations = mkRelations vs
               , fieldName = pure Nothing
               })

    mkRelations :: GraphQL o m => Model.ReqQualities
                -> ArrayObject o m GqlType.Relation
    mkRelations reqQuals =
      traverse identity (fromReqQualities go reqQuals)

        where -- compose fst . resolverRelation
          go :: GraphQL o m
             => Model.Key -> Model.FieldValues
             -> Object o m GqlType.Relation
          go key vs = fst $ resolverRelation key (Values vs)


-----------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
--
-- Model.ComponentMixes ~ [GqlType.ComponentMix!]
--
-- 🦀 Model.toListComponentMixes mixes
--    The Map structure requires checking if a key exists and merging the
--    requests when it makes sense... similarly for Components.
--
resolverMeaExps :: (GraphQL o m, MonadLogger m)
                => Model.ComponentMixes
                -> ArrayObject o m GqlType.Expression
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
--
-- * Nothing => Include Measurement with no filters
--
-- Maybe CompReqValues
--
-- * Nothing => CompReqValues Expressed All FieldValues
--
-- > ReqComponents -> [Filter] 1:N
-- > [Filter] -> [Exp]         N:N
--
-- > CompKey, ReqCompValues -> [Relation] :: distributeCompNameOverValues
-- > [[Relation]] -> [[Relation]]         :: cartProd
--
-- > [Relation] -> Filter
-- > Filter   -> Exp           1:1
--
resolverComponentMix :: (GraphQL o m, MonadLogger m)
                     => (Model.MeaKey, Maybe Model.ReqComponents)
                     -> [Object o m GqlType.Expression]
resolverComponentMix mix =
  case snd mix of
     Just vs -> mkExps (fst mix) (mkFilters vs)
     Nothing -> [pure $ GqlType.Expression
        { source = resolverSource (fst mix)
        , filter = pure Nothing
        , fields = pure field
        , reducer = resolverReducer
        }]

      -- temporary hack until I can figure out the types
      where
        field = traverse
                identity
                [pure $ GqlType.FieldName (pure $ nameTag (fst mix))]

  where

    --
    mkFilters :: (GraphQL o m, MonadLogger m)
              => Model.ReqComponents
              -> [Object o m GqlType.Filter]

    mkFilters vs = mkFilter <$> mkRelations vs

    --
    mkRelations :: (GraphQL o m, MonadLogger m)
                => Model.ReqComponents
                -> [[(Object o m GqlType.Relation, NameTag)]]

    mkRelations = cartProd . fromReqComponents interpretExpRed
           -- [Filters] ~ [[Relation]]

    --
    mkFilter :: GraphQL o m
             => [(Object o m GqlType.Relation, NameTag)]
             -> Object o m GqlType.Filter

    mkFilter relations' = pure $
      GqlType.Filter { relations = traverse identity (fst <$> relations')
                     , fieldName =
                                ( pure
                                 . Just
                                 . GqlType.FieldName
                                 . pure ) . intercalate "."
                                 $ nameTag (fst mix):(snd <$> relations')
                     }

    -- nameRHS (RHSTxtValues vs)   = intercalate "." $ show vs
    -- fieldName: MeaType::value.Name::[value].Name::[value].SpanKey::2_3
    -- fieldName: MeaType::value.LHS::RHS.LHS::RHS.LHS::2_3

    mkExps :: (GraphQL o m)
           => Model.MeaKey -> [Object o m GqlType.Filter]
           -> [Object o m GqlType.Expression]
    mkExps src filters =
      fmap (\flt -> GqlType.Expression { source  = resolverSource src
                                   , filter  = pure $ Just flt
                                   , fields  = pure field
                                   , reducer = resolverReducer -- TODO: read from request
                                   }) <$> filters
      where
        field = traverse
                identity
                [pure $ GqlType.FieldName (pure $ nameTag src)]

---------------------------------------------------------------------------------
-- ** Interpretation of 'Model.ETL.TagExpRed'
-- |
-- The expression of what the 'Model.ETL.TagExpRed' means for
-- 'Model.ETL.FieldValues' is in the number and grouping of
-- 'Api.GQL.Schemas.Matrix.Filter'.
--
-- e.g.,
--
-- > payers {"CASH","PRIVATE"} -> payers "CASH", payers "PRIVATE"
--
-- /Note/: SpanValues use this tag twice. The @Tag FieldValues@ should not
-- impact the ultimate interpretation of the request. Tested accordingly.
--
-- Each pair is a 'Relation'. The number of @Relations@ generated =
-- Number of Values.
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
-- > Long View -> Wide View transformation
--
interpretExpRed :: (GraphQL o m, MonadLogger m)
                 => Model.Key -> Model.CompReqValues
                 -> [(Object o m GqlType.Relation, NameTag)]

interpretExpRed fldName vs

  | Model.areSpanValues vs = uncurry resolverRelation
       <$> repeat fldName `zip`
           (Range <$> concat (Values.mkFilterRange <$> toList vs))

       -- mkFilterRange :: Span -> [FilterRange] is special;
       -- it normalizes how Span values are expressed compared to others
       -- e.g., Red 0 3 -> Range 0 3, Exp 0 2 -> [Range 0 1, Range 1 1]

  | Model.isExp vs = uncurry resolverRelation
       <$> repeat fldName `zip` (Values <$> Model.toCompValuesList vs)

       -- CompReqValues Exp FieldValues fromList ["cash", "medicare"]
       -- Relation payer EQ ["cash"]
       -- Relation payer EQ ["medicare"]

  | Model.isRed vs =

       let Model.CompReqValues tvs = vs
        in [resolverRelation fldName (Values $ Tag.unTag tvs)]

        -- CompReqValues Red FieldValues fromList ["cash", "medicare"]
        -- Relation payer IN ["cash", "medicare"]

  | otherwise = panic "interpretExpRed should not have reached here"


---------------------------------------------------------------------------------
-- |
-- Temp, local unifying type structure to host the RHS of
-- 'resolverRelation' input
--
data RelationInputValues
  = Values Values.FieldValues
  | Range  Values.FilterRange

isRange :: RelationInputValues -> Bool
isRange (Range _) = True
isRange _         = False

getRangeValue :: RelationInputValues -> Values.FilterRange
getRangeValue (Range vs) = vs
getRangeValue _          = panic "getRangeValue: Unreachable"

getOtherValues :: RelationInputValues -> Values.FieldValues
getOtherValues (Values vs) = vs
getOtherValues _           = panic "getOtherValues: Unreachable"

instance NameTagC a => NameTagC (a, RelationInputValues) where
  nameTag (a, vs) = nameTag a <> "::" <> nameTag vs

instance NameTagC RelationInputValues where
  nameTag (Values vs) = nameTag vs
  nameTag (Range v)   = nameTag v

---------------------------------------------------------------------------------
-- ** Relation (LHS relationSymbol RHS)
-- |
-- input FieldValues
--
-- Generic
--
-- Component perspective:
--
-- Called by interpretExpRed where
-- RHS is a list with one value (Expressed) or many values (Reduced)
--
resolverRelation :: GraphQL o m => Model.Key -> RelationInputValues
                 -> (Object o m GqlType.Relation, NameTag)

resolverRelation key vs' = (pure $
  GqlType.Relation {
      lhs      = resolverLHS key
    , rhs      = resolverRHS vs'   -- TxtValues, IntValues
    , relation = resolverRelationSymbol vs'
  }, nameTag (key, vs'))

  where
     ----------------------------------------------------------------------------
     -- input RelationInputValues
     --
     resolverRelationSymbol vs
       | isRange vs = if len (getRangeValue vs) > 1
                 then pure GqlType.BETWEEN
                 else pure GqlType.EQ

       | len (getOtherValues vs) == 1 =
                      pure GqlType.EQ

       | otherwise  = pure GqlType.IN

     ----------------------------------------------------------------------------
     -- input Key
     --
     resolverLHS :: GraphQL o m => Model.Key -> Object o m GqlType.LHS
     resolverLHS = pure . GqlType.LHS
                 . pure . GqlType.FieldName
                 . pure . Model.unKey

     ----------------------------------------------------------------------------
     -- input FieldValues
     --
     resolverRHS :: GraphQL o m => RelationInputValues -> Object o m GqlType.RHS

     resolverRHS values
       | not (isRange values) = case values of

          (Values (Model.TxtSet vs)) -> GqlType.RHSTxtValues
                               <$> Shared.resolverTxtValues (Model.TxtSet vs)

          (Values (Model.IntSet vs)) -> GqlType.RHSIntValues
                               <$> Shared.resolverIntValues (Model.IntSet vs)

          _                 -> panic "Tried to build RHS with Empty"

       | otherwise = GqlType.RHSSpanFilter
                   <$> resolverSpanFilter (getRangeValue values)

---------------------------------------------------------------------------------
-- ** Span Filter
-- |
-- input Model.FilterRange
--
-- > type SpanFilter {
-- >   range: FilterRange!
-- > }
-- > type FilterRange {
-- >   filterStart: Int!
-- >   filterEnd: Int!
-- > }
--
resolverSpanFilter :: GraphQL o m
                    => Values.FilterRange
                    -> Object o m GqlType.SpanFilter

resolverSpanFilter Values.FilterRange { .. }
  = pure . GqlType.SpanFilter
  . pure $ GqlType.FilterRange (pure filterStart) (pure filterEnd)

---------------------------------------------------------------------------------
-- |
-- input Constant
--
-- TODO: This needs to be a configurable value.
--
resolverReducer :: Applicative f => f (Maybe GqlType.ReducerEnum)
resolverReducer = pure $ Just GqlType.SUM

---------------------------------------------------------------------------------
-- |
-- input Key
--
-- Generic
--
-- ETLTable is a source in the ETL wharehouse.  Tables in the warehouse are
-- organized by @etlUnit@.  These units represent valid functions that
-- describe the 'Model.ETL.ObsETL' data. The identifying feature is the
-- codomain (output) of the function, thus the name of the source.
--
-- * type Quality
--
--      > Subject -> Quality
--
-- * type Measurement
--
--      > Subject -> zero+ [Component] -> SpanType -> Measurement Value
--
resolverSource :: GraphQL o m => Model.Key -> Object o m GqlType.ETLTable
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
