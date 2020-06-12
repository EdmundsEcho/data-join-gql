{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Api.GQL.RequestView
-- Description : User specified in the UI "workbench"
-- Description : The product of the workbench UI view
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
module Api.GQL.RequestView where
---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import qualified Data.Set                as Set
---------------------------------------------------------------------------------
import           Model.ETL.Fragment      (fieldCount, getCount)
import qualified Model.ETL.ObsETL        as Model (CompKey, MeaKey, QualKey,
                                                   QualValues, unKey)
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
import           Api.GQL.Types
import           WithAppContext
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
--
resolverRequest
  :: (GraphQL o m, WithAppContext m)
  => Model.Request 'Success
  -> Object o m GqlType.Request
resolverRequest req@Model.Request {..} = do

  lift $ logDebugN ("\n--------------\n" :: Text)
  -- lift $ logDebugF subReq
  lift $ logDebugN ("\n--------------\n" :: Text)
  -- lift $ logDebugF meaReqs
  lift $ logDebugN ("\n--------------\n" :: Text)

  pure $ GqlType.Request
    { subReq     = resolverQualityMix subReq :: GraphQL o m
                   => Object o m GqlType.QualityMix
    , meaReqs    = resolverComponentMixes meaReqs :: GraphQL o m
                   => ArrayObject o m GqlType.ComponentMix
    , fieldCount = pure $ fieldCount req
    , meta       = resolverRequestMeta req
    }

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- [(Text,Text)]
-- f1 [f2 (Meta m)]
resolverRequestMeta :: (GraphQL o m, WithAppContext m)
                    => Model.Request 'Success
                    -> ArrayObject o m GqlType.Meta
resolverRequestMeta Model.Request { meta } = case meta of
  [] -> pure []
  xs -> traverse identity (fmap (\(k,v) -> pure (GqlType.Meta (pure k) (pure v))) xs)

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
resolverQualityMix :: GraphQL o m
                   => Model.QualityMix
                   -> Object o m GqlType.QualityMix
resolverQualityMix req@Model.QualityMix {..} =

  let warning' = if isJust qualityMix
        then Nothing
        else Just "No subject qualifiers included in this request"
  in  pure $ GqlType.QualityMix
        { subjectType = Shared.resolverSubType subjectType  -- :: Model.Key -> String!
        , qualityMix  = resolverReqQualities qualityMix
                          -- :: GraphQL o m => OptionalArrayObject o m GqlType.ReqQuality
        , warning     = pure warning'
        , fieldCount  = pure $ fieldCount req
        }

resolverReqQualities :: GraphQL o m
                     => Maybe Model.ReqQualities
                     -> OptionalArrayObject o m GqlType.ReqQuality
resolverReqQualities Nothing   = pure Nothing
resolverReqQualities (Just vs) = Just
  <$> traverse fromTuple (Model.toListReqQualities vs)

 where
  fromTuple
    :: GraphQL o m
    => (Model.QualKey, Maybe Model.QualValues)
    -> Object o m GqlType.ReqQuality

  -- no values
  fromTuple (key, Nothing) = pure $ GqlType.ReqQuality
    { qualityName = pure $ Model.unKey key
    , values      = pure Nothing
    , message     = pure $ Just "Field included; fullset request."
    }

  -- values
  fromTuple (key, Just qualValues) = pure $ GqlType.ReqQuality
    { qualityName = pure $ Model.unKey key
    , values = Just <$> Shared.resolverQualValues qualValues
    , message = pure $ Just "Field included with a *subset* of levels selected."
    }
        -- qualityName :: GraphQL o m =>  Value o Text
        -- values      :: GraphQL o m =>  OptionalObject o GqlType.QualityValues

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- :: Model.ComponentMixes -> [GqlType.ComponentMix!]
resolverComponentMixes
  :: GraphQL o m => Model.ComponentMixes -> ArrayObject o m GqlType.ComponentMix
resolverComponentMixes mixes = traverse resolverComponentMix
                                        (Model.toListComponentMixes mixes)

 where
  resolverComponentMix
    :: GraphQL o m
    => (Model.MeaKey, Maybe Model.ReqComponents)
    -> Object o m GqlType.ComponentMix

  -- TODO: review how to display request for the Measurement field
  --       => all reduced.  Is it worthwhile to show all levels? No.
  --
  resolverComponentMix (meaType, maybeReqComponents) = do

    let message' = if isJust maybeReqComponents
          then Nothing
          else Just "A single summary computation for the measurement"

    pure $ GqlType.ComponentMix
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
resolverReqComponents :: GraphQL o m
                      => Model.ReqComponents
                      -> ArrayObject o m GqlType.ReqComponent
resolverReqComponents reqComps =
  traverse
  resolverReqComponent
  (Model.toListReqComponents reqComps)

 where
   resolverReqComponent :: GraphQL o m
    => (Model.CompKey, Maybe Model.CompReqValues)
    -> Object o m GqlType.ReqComponent

  -- TODO: review how to display request for the Measurement field
  --       => all reduced.  Is it worthwhile to show all levels?

   resolverReqComponent (compKey, maybeCompReqValues) = do

     -- CompReqValues encodes both TagRedExp and Values
     let maybeValues  = fst . Model.toTupleCompReqValues <$> maybeCompReqValues
     let maybeReduced = snd . Model.toTupleCompReqValues <$> maybeCompReqValues
          -- Shared.resolverCompValues $ (fst . Model.toTupleCompReqValues) <$> maybeCompReqVs

     let reduced'     = fromMaybe True maybeReduced

     let message' = if reduced'
           then
             "A single summary field using records matching the "
               <> "selected component values"
           else
             "A series of fields; the number of fields depends "
             <> "on the number of values included in the requested "
             <> "mix of components"

     pure $ GqlType.ReqComponent
       { componentName = pure $ Model.unKey compKey                      -- :: Text
       , values        = traverse Shared.resolverCompValues maybeValues  -- :: Maybe ComponentValues
       , reduced       = pure reduced'                                   -- :: Bool
       , message       = pure $ Just message'                            -- :: Maybe Text
       }


---------------------------------------------------------------------------------
-- Utility function
dedup :: Ord a => [a] -> [a]
dedup = Set.toList . Set.fromList
