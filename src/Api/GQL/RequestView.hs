{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module     : Api.GQL.RequestView
-- Description: UI view
--
module Api.GQL.RequestView where
---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import           Control.Monad.Logger
import qualified Data.Set                as Set
---------------------------------------------------------------------------------
import qualified Model.ETL.FieldValues   as FV (areSpanValues)
import           Model.ETL.Fragment      (fieldCount, getCount)
import qualified Model.ETL.ObsETL        as Model (CompKey, MeaKey, QualKey,
                                                   QualValues, mkCompKey, unKey)
import qualified Model.Request           as Model (CompReqValues (..),
                                                   ComponentMixes (..),
                                                   QualityMix (..),
                                                   ReqComponents (..),
                                                   ReqQualities (..),
                                                   Request (..),
                                                   toListComponentMixes,
                                                   toListReqComponents,
                                                   toListReqQualities,
                                                   toTupleCompReqValues)
import           Model.Status
---------------------------------------------------------------------------------
import           Api.GqlHttp
---------------------------------------------------------------------------------
import qualified Api.GQL.ObsETL          as Shared
import qualified Api.GQL.Schemas.Request as GqlType
---------------------------------------------------------------------------------
  --
---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- Request data types -> Types specified in the schema
-- Request (Resolver o () AppObs)
resolverRequest :: GraphQL o => Model.Request 'Success -> Object o GqlType.Request
resolverRequest req@Model.Request {..} = do

  lift . logDebugN $ ("\n--------------\n"::Text)
  logger subReq
  lift . logDebugN $ ("\n--------------\n"::Text)
  logger meaReqs
  lift . logDebugN $ ("\n--------------\n"::Text)

  pure $
    GqlType.Request

      { subReq =  resolverQualityMix subReq
        :: GraphQL o => Object o GqlType.QualityMix

      , meaReqs = resolverComponentMixes meaReqs
        :: GraphQL o => ArrayObject o GqlType.ComponentMix

      , fieldCount = pure $ fieldCount req

      }

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
resolverQualityMix :: GraphQL o => Model.QualityMix -> Object o GqlType.QualityMix
resolverQualityMix req@Model.QualityMix {..} =

    let warning' = if isJust qualityMix
                     then Nothing
                     else Just "No subject qualifiers included in this request"

   in pure $
     GqlType.QualityMix
       {   subjectType = Shared.resolverSubType subjectType  -- :: Model.Key -> String!
         , qualityMix  = resolverReqQualities qualityMix
                      :: GraphQL o => OptionalArrayObject o GqlType.ReqQuality
         , warning = pure warning'
         , fieldCount = pure $ fieldCount req
       }

resolverReqQualities :: GraphQL o
                     => Maybe Model.ReqQualities -> OptionalArrayObject o GqlType.ReqQuality
resolverReqQualities Nothing   = pure Nothing
resolverReqQualities (Just vs) =
  Just <$> traverse fromTuple (Model.toListReqQualities vs)

  where
    fromTuple :: GraphQL o
              => (Model.QualKey, Maybe Model.QualValues) -> Object o GqlType.ReqQuality

    -- no values
    fromTuple (key, Nothing) =
      pure $ GqlType.ReqQuality
        { qualityName = pure $ Model.unKey key
        , values      = pure Nothing
        , message     = pure $ Just "Field included; fullset request."
        }

    -- values
    fromTuple (key, Just qualValues) =
      pure $ GqlType.ReqQuality
        { qualityName = pure $ Model.unKey key
        , values      = Just <$> Shared.resolverQualValues qualValues
        , message     = pure $ Just "Field included with a *subset* of levels selected."
        }
        -- qualityName :: GraphQL o =>  Value o Text
        -- values      :: GraphQL o =>  OptionalObject o GqlType.QualityValues
---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- :: Model.ComponentMixes -> [GqlType.ComponentMix!]
resolverComponentMixes :: GraphQL o
                       => Model.ComponentMixes -> ArrayObject o GqlType.ComponentMix
resolverComponentMixes mixes =
  -- lift . logDebugN $ ("\nHERE"::Text)
  -- lift . logDebugN $ ("mixes: "::Text) <> show mixes
  traverse resolverComponentMix (Model.toListComponentMixes mixes)

  where
    resolverComponentMix :: GraphQL o
                         => (Model.MeaKey, Maybe Model.ReqComponents)
                         -> Object o GqlType.ComponentMix

    -- TODO: review how to display request for the Measurement field
    --       => all reduced.  Is it worthwhile to show all levels? No.
    --
    resolverComponentMix (meaType, maybeReqComponents) = do

      let message' = if isJust maybeReqComponents
              then Nothing
              else Just "A single summary computation for the measurement"

      pure $
         GqlType.ComponentMix
           { measurementType = pure $ Model.unKey meaType                        -- :: Text
           , componentMix    = traverse resolverReqComponents maybeReqComponents -- :: Maybe [ReqComponent]
           , fieldCount      = pure $ getCount meaType mixes
           , message         = pure message'
           }

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
--
-- TODO: Make sure decision whether to display all levels when the intent is
--       to generate a series is consistently applied.
--
-- > ReqComponents
-- >   { reqComponents :: Map Key (Maybe CompReqValues)
-- >   } deriving (Show, Eq, Ord, Generic)
--
-- > type ReqComponent {
-- >   componentName: String!
-- >   values: CompReqValues!
-- > }
--
resolverReqComponents :: GraphQL o
                      => Model.ReqComponents
                      -> ArrayObject o GqlType.ReqComponent
resolverReqComponents reqComps =
  -- lift . logDebugN $ ("\nHERE"::Text)
  -- lift . logDebugN $ ("compKey: "::Text) <> show reqComps
  traverse resolverReqComponent (Model.toListReqComponents reqComps)

  where

    resolverReqComponent :: GraphQL o
                         => (Model.CompKey, Maybe Model.CompReqValues)
                         -> Object o GqlType.ReqComponent

    -- TODO: review how to display request for the Measurement field
    --       => all reduced.  Is it worthwhile to show all levels?

    resolverReqComponent (compKey, maybeCompReqValues) = do

      -- CompReqValues encodes both TagRedExp and Values
      let maybeValues  = fst . Model.toTupleCompReqValues <$> maybeCompReqValues
      let maybeReduced = snd . Model.toTupleCompReqValues <$> maybeCompReqValues
           -- Shared.resolverCompValues $ (fst . Model.toTupleCompReqValues) <$> maybeCompReqVs

      let reduced' = fromMaybe True maybeReduced

      let message' = if reduced'
                        then "A single summary field using records matching the "
                          <> "selected component values"
                        else "A series of fields; the number of fields depends "
                          <> "on the number of values included in the requested "
                          <> "mix of components"

      -- debugging SpanType
      if compKey == Model.mkCompKey "SpanType"
         then do
            lift . logDebugN $ ("\n"::Text)
            lift . logDebugN $ ("compKey: "::Text) <> show compKey
            lift . logDebugN $ ("maybeValues: "::Text) <> show maybeValues
            lift . logDebugN $ ("areSpanValues: "::Text) <> show (FV.areSpanValues <$> maybeValues)
         else
            pure ()


      pure $
         GqlType.ReqComponent
           { componentName = pure $ Model.unKey compKey                      -- :: Text
           , values        = traverse Shared.resolverCompValues maybeValues  -- :: Maybe ComponentValues
           , reduced       = pure reduced'                                   -- :: Bool
           , message       = pure $ Just message'                            -- :: Maybe Text
           }


---------------------------------------------------------------------------------
-- Utility function
dedup :: Ord a => [a] -> [a]
dedup = Set.toList . Set.fromList
