{-# OPTIONS_HADDOCK ignore-exports #-}
-- {-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Api.GQL.LevelsResolver
-- Description : Resolvers for the selectValues QUERY
-- Copyright   : (c) Lucivia LLC, 2021
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This endpoint implements the 'Connection' pattern in order to support
-- Relay pagination.
--
module Api.GQL.LevelsResolver
        ( resolverLevels
        )
where
---------------------------------------------------------------------------------
import           Protolude               hiding ( first
                                                , toList
                                                )
---------------------------------------------------------------------------------
import qualified Model.ETL.ObsETL              as Model
import qualified Model.ETL.Fragment            as Model hiding (member)
---------------------------------------------------------------------------------
import           WithAppContext                 ( WithAppContext )
import qualified WithAppContext                as App
                                         hiding ( WithAppContext )
---------------------------------------------------------------------------------
import           Api.GQL.Types
import           Api.GQL.Schemas.Levels
import           Api.GQL.Schemas.ObsETL
import           Api.GQL.Schemas.Root
---------------------------------------------------------------------------------
import qualified Api.ETL                       as ETL
---------------------------------------------------------------------------------
--
-- |
--
resolverLevels
        :: (GraphQL o m, WithAppContext m)
        => LevelsArgs
        -> Model.ObsETL
        -> Object o m LevelsConnection

resolverLevels args@LevelsArgs {..} obsEtl
  |
  -- run the filter before returning list of values
          Just filterQuality' <- fromQuality = do
                lift $ App.logInfoN "Received the levels page request"
                -- get the etl data
                case
                                ( getQualityContains filterQuality'
                                , getQualityStartsWith filterQuality'
                                , getQualityEndsWith filterQuality'
                                , ETL.getQualityValues
                                        (getQualityName filterQuality')
                                        obsEtl
                                )
                        of
                                (Nothing, Nothing, Nothing, Nothing) ->
                                        resolverLevelsConnection Nothing args

                                (Nothing, Nothing, Nothing, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just etlData)
                                                args

                                (Just contains, Nothing, Nothing, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just $
                                                ETL.filterValues ETL.isSubstringP contains etlData
                                                )
                                                args

                                (Nothing, Just startsWith, Nothing, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just $
                                                ETL.filterValues ETL.startsWithP startsWith etlData
                                                )
                                                args

                                (Nothing, Nothing, Just endsWith, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just $
                                                ETL.filterValues ETL.endsWithP endsWith etlData
                                                )
                                                args

                                (Nothing, Just startsWith, Just endsWith, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just .
                                                ETL.filterValues ETL.startsWithP startsWith $
                                                ETL.filterValues ETL.endsWithP endsWith etlData
                                                )
                                                args

                                (Just contains, Just startsWith, Just endsWith, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just .
                                                ETL.filterValues ETL.isSubstringP contains .
                                                ETL.filterValues ETL.startsWithP startsWith $
                                                ETL.filterValues ETL.endsWithP endsWith etlData
                                                )
                                                args

                                (Just contains, Nothing, Just endsWith, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just .
                                                ETL.filterValues ETL.isSubstringP contains $
                                                ETL.filterValues ETL.endsWithP endsWith etlData
                                                )
                                                args

                                (Just contains, Just startsWith, Nothing, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just .
                                                ETL.filterValues ETL.isSubstringP contains $
                                                ETL.filterValues ETL.startsWithP startsWith etlData
                                                )
                                                args

                                (_, _, _, Nothing) ->
                                        resolverLevelsConnection Nothing args
  |
  -- run the filter before returning list of values
       Just filterComponent' <- fromComponent = do
                lift $ App.logInfoN "Received the levels page request"
                -- get the etl data
                case
                                ( getComponentContains filterComponent'
                                , getComponentStartsWith filterComponent'
                                , getComponentEndsWith filterComponent'
                                , ETL.getComponentValues
                                        (getMeasurementType filterComponent')
                                        (getComponentName filterComponent')
                                        obsEtl
                                )
                        of
                                (Nothing, Nothing, Nothing, Nothing) ->
                                        resolverLevelsConnection Nothing args

                                (Nothing, Nothing, Nothing, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just etlData)
                                                args

                                (Just contains, Nothing, Nothing, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just $
                                                ETL.filterValues ETL.isSubstringP contains etlData
                                                )
                                                args

                                (Nothing, Just startsWith, Nothing, Just etlData) -> do
                                        lift $ App.logInfoN ("Starts with" <> startsWith)
                                        resolverLevelsConnection
                                                (Just $
                                                ETL.filterValues ETL.startsWithP startsWith etlData
                                                )
                                                args

                                (Nothing, Nothing, Just endsWith, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just $
                                                ETL.filterValues ETL.endsWithP endsWith etlData
                                                )
                                                args

                                (Nothing, Just startsWith, Just endsWith, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just .
                                                ETL.filterValues ETL.startsWithP startsWith $
                                                ETL.filterValues ETL.endsWithP endsWith etlData
                                                )
                                                args

                                (Just contains, Just startsWith, Just endsWith, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just .
                                                ETL.filterValues ETL.isSubstringP contains .
                                                ETL.filterValues ETL.startsWithP startsWith $
                                                ETL.filterValues ETL.endsWithP endsWith etlData
                                                )
                                                args

                                (Just contains, Nothing, Just endsWith, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just .
                                                ETL.filterValues ETL.isSubstringP contains $
                                                ETL.filterValues ETL.endsWithP endsWith etlData
                                                )
                                                args

                                (Just contains, Just startsWith, Nothing, Just etlData) ->
                                        resolverLevelsConnection
                                                (Just .
                                                ETL.filterValues ETL.isSubstringP contains $
                                                ETL.filterValues ETL.startsWithP startsWith etlData
                                                )
                                                args

                                (_, _, _, Nothing) ->
                                        resolverLevelsConnection Nothing args
  | otherwise = do
       lift $ App.logInfoN "Received the levels page request"
       resolverLevelsConnection Nothing args

-- |
-- Top-level resolver for the LevelsResolver package
--
-- /Note/: Utilized by root level -> LevelsConnection
--
resolverLevelsConnection
        :: (GraphQL o m, WithAppContext m)
        => Maybe Model.FieldValues
        -> LevelsArgs
        -> Object o m LevelsConnection

resolverLevelsConnection (Just values) args = do
        -- 🚧 use monad context edgesToReturn
        let (values', pageInfo) = edgesToReturn args values
        case values' of
           -- re-apply with empty values when the filters return Empty
           Model.Empty -> resolverLevelsConnection Nothing args
           -- ... otherwise proceed to report out the values
           _ -> do
                   lift $ App.logInfoN "✅ The ETL data exists..."

                   pure $ LevelsConnection
                        { edges      = Just <$> traverse resolverLevelEdge (toList values')
                        , pageInfo   = resolverPageInfo (Just pageInfo)
                        , totalCount = pure $ Model.len values -- length edges
                        }

resolverLevelsConnection Nothing _ = do
        lift $ App.logInfoN "⚠️  There is no ETL data for this request."
        pure $ LevelsConnection { edges      = pure Nothing
                                , pageInfo   = resolverPageInfo Nothing
                                , totalCount = pure 0
                                }

-- |
-- Interpret the range of input possibilities
--
edgesToReturn
        :: LevelsArgs
        -> Model.FieldValues
        -> (Model.FieldValues, InternalPageInfo)

edgesToReturn args@LevelsArgs {..} values
        | (Just first', Nothing) <- (first, after) =
                ( Model.take first' values
                , InternalPageInfo
                     { hasNextPage = first' < Model.len values
                     , hasPreviousPage = False
                     , startCursor = Model.encodeFieldValue <$> Model.elemAt 0 values
                     , endCursor = Model.encodeFieldValue <$> Model.elemAt (first' - 1) values
                     }
                )

        -- splitAt after then take first
        | (Just first', Just after') <- (first, after) = if Model.member after' values
           then
                let (_, forwardValues) = Model.splitAt (1 + Model.findIndex after' values) values
                in ( Model.take first' forwardValues
                   , InternalPageInfo
                     { hasNextPage = first' < Model.len forwardValues
                     , hasPreviousPage = True
                     , startCursor = Model.encodeFieldValue <$> Model.elemAt 0 forwardValues
                     , endCursor = Model.encodeFieldValue <$> Model.elemAt (first' - 1) forwardValues
                     }
                )

           -- what it means when the after cursor does not exist
           else ( Model.Empty, emptyPageInfo )

        -- Backwards
        -- splitAt length - last, then take last (return split)
        -- use the first half of the split
        -- end of slice = before
        -- start of slice = ...
        | (Just last', Nothing) <- (last, before) =
                let backwardValues = values
                    splitHere = if Model.len backwardValues - last' < 0 then 0
                                else Model.len backwardValues - last'

                    (_, slice) = Model.splitAt splitHere backwardValues

                in ( slice , InternalPageInfo
                     { hasNextPage = False
                     , hasPreviousPage = splitHere /= 0
                     , startCursor = Model.encodeFieldValue <$> Model.elemAt 0 slice
                     , endCursor = Model.encodeFieldValue <$> Model.elemAt (Model.len slice - 1) slice
                     }
                )

        -- splitAt length - last, then take last (return split)
        -- use the first half of the split
        -- end of slice = before
        -- start of slice = ...
        | (Just last', Just before') <- (last, before) = if Model.member before' values
           then
                let (backwardValues, _) = Model.splitAt (Model.findIndex before' values) values
                    splitHere = if Model.len backwardValues - last' < 0 then 0
                                else Model.len backwardValues - last'

                    (_, slice) = Model.splitAt splitHere backwardValues

                in ( slice , InternalPageInfo
                     { hasNextPage = True
                     , hasPreviousPage = splitHere /= 0
                     , startCursor = Model.encodeFieldValue <$> Model.elemAt 0 slice
                     , endCursor = Model.encodeFieldValue <$> Model.elemAt (Model.len slice - 1) slice
                     }
                )

           -- what it means when the before cursor does not exist
           else ( Model.Empty, emptyPageInfo )

        -- recasts application of edgesToReturn
        -- 🚧 Append a warning message to the response
        --
        | (Just _, Just _) <- (first, last) =
                edgesToReturn (args { last = Nothing }) values

        | (Just _, Just _) <- (after, before) =
                edgesToReturn (args { before = Nothing }) values

        | (Just _, Just _) <- (last, after) =
                edgesToReturn (args { after = Nothing }) values

        | (Just _, Just _) <- (first, before) =
                edgesToReturn (args { before = Nothing }) values

        | otherwise = ( Model.Empty, emptyPageInfo )

data InternalPageInfo =
        InternalPageInfo
                  { hasNextPage:: Bool
                  , hasPreviousPage :: Bool
                  , startCursor :: Maybe Text
                  , endCursor :: Maybe Text
                  }

emptyPageInfo :: InternalPageInfo
emptyPageInfo = InternalPageInfo
                     { hasNextPage = False
                     , hasPreviousPage = False
                     , startCursor = Nothing
                     , endCursor = Nothing
                     }

-- |
--
toList :: Model.FieldValues -> [Model.FieldValue]
toList xs@(Model.TxtSet  _) = Model.TxtValue <$> Model.toList xs
toList xs@(Model.IntSet  _) = Model.IntValue <$> Model.toList xs
toList xs@(Model.SpanSet _) = Model.SpanValue <$> Model.toList xs
toList Model.Empty          = [Model.EmptyValue]

-- |
--
resolverLevelEdge
        :: (GraphQL o m, WithAppContext m)
        => Model.FieldValue
        -> Object o m LevelEdge

resolverLevelEdge level = pure $ LevelEdge
        { cursor = pure $ Model.encodeFieldValue level
        , node   = resolverLevel level
        }

-- |
--
resolverPageInfo :: GraphQL o m => Maybe InternalPageInfo -> Object o m PageInfo
resolverPageInfo Nothing = pure $ PageInfo { hasNextPage     = pure False
                                           , hasPreviousPage = pure False
                                           , startCursor     = pure Nothing
                                           , endCursor       = pure Nothing
                                           }

resolverPageInfo (Just InternalPageInfo {..}) = pure $ PageInfo
        { hasNextPage     = pure hasNextPage
        , hasPreviousPage = pure hasPreviousPage
        , startCursor     = pure startCursor
        , endCursor       = pure endCursor
        }

-- |
--
resolverLevel :: GraphQL o m => Model.FieldValue -> Object o m Level
resolverLevel (Model.TxtValue value) = LevelTxtValue <$> resolverTxtValue value
resolverLevel (Model.IntValue value) = LevelIntValue <$> resolverIntValue value
resolverLevel _                      = panic "field level is either empty or otherwise not supported"


-- |
--
resolverTxtValue :: GraphQL o m => Text -> Object o m TxtValue
resolverTxtValue x = pure $ TxtValue { level = pure x }

-- |
--
resolverIntValue :: GraphQL o m => Int -> Object o m IntValue
resolverIntValue x = pure $ IntValue { level = pure x }

-- resolverSpanValue :: GraphQL o m => Model.Span -> Object o m Span
-- resolverSpanValue x = pure $ Span x

-- |
-- Api ETL data helper
--
getQualityName :: FromQuality -> Text
getQualityName FromQuality { qualityName } = qualityName

-- |
-- Api ETL data helper
--
getMeasurementType :: FromComponent -> Text
getMeasurementType FromComponent { measurementType } = measurementType

-- |
-- Api ETL data helper
--
getComponentName :: FromComponent -> Text
getComponentName FromComponent { componentName } = componentName


-- |
-- Api ETL data helper
--
getComponentContains :: FromComponent -> Maybe Text
getComponentContains FromComponent { contains } = contains

-- |
-- Api ETL data helper
--
getComponentStartsWith :: FromComponent -> Maybe Text
getComponentStartsWith FromComponent { startsWith } = startsWith

-- |
-- Api ETL data helper
--
getComponentEndsWith :: FromComponent -> Maybe Text
getComponentEndsWith FromComponent { endsWith } = endsWith

-- |
-- Api ETL data helper
--
getQualityContains :: FromQuality -> Maybe Text
getQualityContains FromQuality { contains } = contains
-- |
-- Api ETL data helper
--
getQualityStartsWith :: FromQuality -> Maybe Text
getQualityStartsWith FromQuality { startsWith } = startsWith

-- |
-- Api ETL data helper
--
getQualityEndsWith :: FromQuality -> Maybe Text
getQualityEndsWith FromQuality { endsWith } = endsWith

