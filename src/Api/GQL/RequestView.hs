{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module     : Api.GQL.Request
-- Description: UI access point
--
-- This module implements the resolvers for a 'Request'. The GQL inputs and
-- views are specified in @schemas.request.graphql@ and what is shared
-- with the 'Api.GQL.ObsETL' module; see @schemas.shared.graphql@.
--
-- The 'Model.Reqest' is a host for requested data; data fetched from 'Model.ETL'
-- stored in the @Database@ 'AppTypes.Env'.
--
-- The design tries to leverage the 'Model.ObsETL' data types where
-- possible.  However, in order to maintain a clear distinction between request
-- and the ETL source of truth, the design likely would have been better
-- had all request inputs be tagged (one way or another) with a 'Request' moniker.
--
-- Keep in mind,
-- a request is a series of search terms. The @schemas.request.graphql@ define
-- the input object specifications accordingly. However, a 'Request' is not
-- data unto themselves. This line is blurred by the fact that 'Model.Request'
-- is instantiated using 'Model.ObsETL' data. However, is viewed using the
-- data types described in @schemas.request.graphql@.
--
-- Also note, the use of 'Model.ETL.TagRedExp.TagRedExp' is used in two ways.
-- First for Span both in the ETL and Request phases.  Second, to qualify the
-- requests for Components ('Model.Request.ReqComponents'). There is likely
-- redundant use of the 'Model.ETL.TagRedExp.TagRedExp' for a
-- 'Model.ETL.Span.Span' included in a 'Request'.
--
module Api.GQL.RequestView where
---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import           Data.Maybe              (fromJust)
import qualified Data.Set                as Set
---------------------------------------------------------------------------------
import qualified Model.ETL.ObsETL        as Model (CompKey, MeaKey, QualKey,
                                                   QualValues, isRed, unKey)
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
resolverRequest :: GraphQL o => Model.Request -> Object o GqlType.Request
resolverRequest (Model.Request subReq meaReqs) =
  pure $
    GqlType.Request

      { subReq =  resolverQualityMix subReq
          :: GraphQL o => Object o GqlType.QualityMix

      , meaReqs = resolverComponentMixes meaReqs
          :: GraphQL o => ArrayObject o GqlType.ComponentMix
      }

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
resolverQualityMix :: GraphQL o => Model.QualityMix -> Object o GqlType.QualityMix
resolverQualityMix Model.QualityMix {..} =
  pure $
    GqlType.QualityMix
      { subjectType  = Shared.resolverSubType subjectType  -- :: Model.Key -> String!
        , qualityMix = resolverReqQualities qualityMix
                       :: GraphQL o => OptionalArrayObject o GqlType.ReqQuality
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
        , values = pure Nothing
        }

    -- values
    fromTuple (key, Just qualValues) =
      pure $ GqlType.ReqQuality
        { qualityName = pure $ Model.unKey key
                        :: GraphQL o =>  Value o Text

        , values      = Just <$> Shared.resolverQualValues qualValues
                        :: GraphQL o =>  OptionalObject o GqlType.QualityValues
        }

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- :: Model.ComponentMixes -> [GqlType.ComponentMix!]
resolverComponentMixes :: GraphQL o
                       => Model.ComponentMixes -> ArrayObject o GqlType.ComponentMix
resolverComponentMixes mixes =
  traverse resolverComponentMix (Model.toListComponentMixes mixes)

  where
    resolverComponentMix :: GraphQL o
                         => (Model.MeaKey, Maybe Model.ReqComponents)
                         -> Object o GqlType.ComponentMix
    -- TODO: review how to display request for the Measurement field
    --       => all reduced.  Is it worthwhile to show all levelts?
    resolverComponentMix (meaType, compMix) =
      let displayCompMix =
           if isJust compMix
              then (resolverReqComponents (fromJust compMix), Nothing)
              else ( pure []
                   , Just "A single summary computation for the measurement")

      in pure $
         GqlType.ComponentMix
           { measurementType = pure $ Model.unKey meaType  -- :: Model.Key -> String!
           , componentMix    = fst displayCompMix
           , message         = pure $ snd displayCompMix
           }

---------------------------------------------------------------------------------
-- |
-- Model -> GraphQL View
-- /Note/: The use of @traverse identity :: [Object o a] -> ArrayObject o a
-- to coerce the change in type.
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
  traverse resolverReqComponent (Model.toListReqComponents reqComps)

  where

    resolverReqComponent :: GraphQL o
                         => (Model.CompKey, Maybe Model.CompReqValues)
                         -> Object o GqlType.ReqComponent

    -- TODO: review how to display request for the Measurement field
    --       => all reduced.  Is it worthwhile to show all levelts?
    resolverReqComponent (compKey, mCompReqVs) =

      let displayCompReqValues =
           if isJust mCompReqVs
              then ( Just <$> Shared.resolverCompValues ( fst
                                                      . Model.toTupleCompReqValues
                                                      $ fromJust mCompReqVs
                                                      )
                   , Nothing)

              else ( pure Nothing
                   , Just "A series of fields; one for each level in this component")

      in pure $
         GqlType.ReqComponent
           { componentName = pure $ Model.unKey compKey
           , values        = fst displayCompReqValues -- :: ComponentValues
           , reduced       = pure . isRed $ fromJust mCompReqVs
           , message       = pure $ snd displayCompReqValues
           }

      where
        isRed Model.CompReqValues { values } = Model.isRed values


---------------------------------------------------------------------------------
-- |
-- Utility function
dedup :: Ord a => [a] -> [a]
dedup = Set.toList . Set.fromList
