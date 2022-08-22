{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module     : Api.GQL.ObsEtl
-- Description: ObsEtl GQL Input and GQL View
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
-- ** Overview
--
-- There are many GQL resolvers
--
--   > Query :: GqlInput -> Model (GqlInput -> Model -> Model) -> View
--
--   > Mutation :: GqlInput -> Model (GqlInput -> Model -> Model) -> View
--
--   > View :: Model -> Gql
--
module Api.GQL.ObsETL
  (
  -- * Root resolver
    resolverObsEtl

  -- * Shared Views
  , fromInputQualities
  , fromInputQualValues
  -- , fromInputCompValues
  , ObsETL
  , Quality
  , Span
  , ComponentValues(..)
  , resolverCompValues
  , resolverValuesReqEnum
  , resolverSpanValue
  , resolverSubType

  -- * Api.GQL.Matrix shared types
  , resolverTxtValues
  , resolverIntValues

  -- * Api.GQL Shared Inputs
  , QualityInput(..)
  , QualValuesInput(..)
  , CompValuesInput(..)
  , SpanInput(..)

  -- * Required by root
  , ObsEtlInput
  , fromInputObsEtl
  )
where
-------------------------------------------------------------------------------
import           Protolude              hiding (catch, catchJust, handleJust)
-------------------------------------------------------------------------------
import           Control.Exception.Safe
import           Control.Monad.Logger
import           ObsExceptions
-------------------------------------------------------------------------------
import qualified Model.ETL.Fragment     as Model (len, toList)
import qualified Model.ETL.ObsETL       as Model
import qualified Model.ETL.TagRedExp    as Model
import qualified Model.ETL.Transformers as Trans
-------------------------------------------------------------------------------
import           Api.GQL.Schemas.ObsETL
import           Api.GQL.Schemas.Shared
import           Api.GQL.Types
import           WithAppContext
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- * ObsETL Model -> View
-- |
--
resolverObsEtl :: GraphQL o m => Model.ObsETL -> Object o m ObsETL
resolverObsEtl Model.ObsETL {..} =
  pure $
    ObsETL
      (resolverID obsID)
      (resolverSubject obsSubject)            -- :: Model.Subject -> Subject
      (resolverMeasurements obsMeasurements)  -- :: Model.Measurements -> [Measurement]

resolverID :: GraphQL o m => Model.ID -> Value o m Text
resolverID = pure . Model.unID

-------------------------------------------------------------------------------
-- ** Subject Model -> View
-- |
--
resolverSubject :: GraphQL o m => Model.Subject -> Object o m Subject
resolverSubject Model.Subject {..} =
  pure $
    Subject
      (resolverSubType subType)        -- :: Model.Key       -> String!
      (resolverQualities subQualities) -- :: Model.Qualities -> [Quality!]!

resolverSubType :: GraphQL o m => Model.SubKey -> Value o m Text
resolverSubType = pure . Model.unKey

--------------------------------------------------------------------------------
-- ** Measurements Model -> View
-- |
-- /Note/: The model does not host Measurement
--
resolverMeasurements :: GraphQL o m
                     => Model.Measurements -> ArrayObject o m Measurement
resolverMeasurements o'
  = traverse identity $ Trans.fromMeasurements resolverMeasurement o'
  where
    resolverMeasurement :: GraphQL o m
                        => Model.MeaKey -> Model.Components
                        -> Object o m Measurement
    resolverMeasurement key o'' =
      pure $
        Measurement
           (pure $ Model.unKey key)
           (traverse identity $ Trans.fromComponents resolverComponent o'')

--------------------------------------------------------------------------------
-- *** Components Model -> View
-- |
-- /Note/: The model does not host Component
--
-- > Components { components :: Map Key CompValues }
--
-- > type Component {
-- >   componentName: String!
-- >   componentValues: ComponentValues!
-- > }
--
resolverComponent :: GraphQL o m
                  => Model.CompKey -> Model.CompValues -> Object o m Component
resolverComponent key o' =
  pure $ Component
  { componentName = pure $ Model.unKey key
  , componentValues = resolverCompValues o'
  , count = pure $ Model.len o'
  }

--------------------------------------------------------------------------------
-- *** ComponentValues Model -> View
-- |
-- Model CompValues, GQL Union ComponentValues
--
-- > Model TxtSet (Set Text) | IntSet (Set Int) etc..
--
-- > GQL { txtValues :: [Text] } | { intValues :: [Int] }
--
-- Pattern match to delegate to one of the 3 value types
--
-- ⬜ Complete the use of Empty.  e.g., have it display null in GQL.
--
resolverCompValues :: GraphQL o m => Model.CompValues -> Object o m ComponentValues
resolverCompValues (Model.TxtSet o') =
  ComponentValuesTxtValues <$> resolverTxtValues (Model.TxtSet o')

resolverCompValues (Model.IntSet o') =
  ComponentValuesIntValues <$> resolverIntValues (Model.IntSet o')

resolverCompValues (Model.SpanSet o') =
  ComponentValuesSpanValues <$> resolverSpanValues (Model.SpanSet o')

resolverCompValues Model.Empty =
  ComponentValuesTxtValues <$> resolverTxtValues Model.Empty

--------------------------------------------------------------------------------
-- ** Qualities Model -> View
-- |
-- /Note/: The model does not host Quality
--
-- Model Key + TxtSet (Set Text) | IntSet (Set Text)
--
-- GQL Quality is an object with two fields
--     qualityName :: Text
--     qualityValues :: Union QualityValuesTxtValues ...
--
resolverQualities :: GraphQL o m => Model.Qualities -> ArrayObject o m Quality
resolverQualities o'
  = traverse identity $ Trans.fromQualities resolverQuality o'

resolverQuality :: GraphQL o m
                => Model.QualKey -> Model.QualValues -> Object o m Quality
resolverQuality key o' =
  pure $ Quality
  { qualityName = pure $ Model.unKey key
  , qualityValues = resolverQualValues o'
  , count = pure $ Model.len o'
  }

--------------------------------------------------------------------------------
-- *** QualValues Model -> View
-- |
-- Model QualValues, GQL Union QualityValues
--
-- > Model TxtSet (Set Text) | IntSet (Set Int)
--
-- > GQL { txtValues :: [Text] } | { intValues :: [Int] }
--
-- Pattern match to delegate to one of the 3 value types
--
resolverQualValues :: GraphQL o m => Model.FieldValues -> Object o m QualityValues
resolverQualValues (Model.TxtSet o') =
  QualityValuesTxtValues <$> resolverTxtValues (Model.TxtSet o')

resolverQualValues (Model.IntSet o') =
  QualityValuesIntValues <$> resolverIntValues (Model.IntSet o')

resolverQualValues  _ = panic "QualValues resolver: tried with the wrong type."


--------------------------------------------------------------------------------
-- *** ValuesReqEnum Model -> View
-- |
-- Model QualValues, GQL Union QualityValues
-- data ValuesReqEnum
-- = ExcludeRequest (ValuesReq 'Exclude)
-- | IncludeRequest (ValuesReq 'Include)
-- | NA QualValues
-- deriving (Show, Eq, Ord, Generic)
--
-- 🚧 Finish with SpSpanValues
-- 🔑 This is where we encode the Include Exclude!!
--
resolverValuesReqEnum :: GraphQL o m
                      => Model.ValuesReqEnum
                      -> (Object o m QualityValues, Bool)
resolverValuesReqEnum vs =
   let (vs', antiEnum) = Model.unwrapReqEnum vs
   in case antiEnum of
      Model.Include -> (resolverQualValues vs', False)
      Model.Exclude -> (resolverQualValues vs', True)

--------------------------------------------------------------------------------
-- **** FieldValues Model -> View
-- |
-- FieldValues :: TxtValues | IntValues | SpanValues
--
resolverTxtValues :: GraphQL o m => Model.FieldValues -> Object o m TxtValues
resolverTxtValues vs@(Model.TxtSet _) = pure $
  TxtValues { txtValues = pure $ Model.toList vs }
resolverTxtValues  _ = panic "Values resolver: tried with the wrong type."

resolverIntValues :: GraphQL o m => Model.FieldValues -> Object o m IntValues
resolverIntValues vs@(Model.IntSet _) = pure $
  IntValues { intValues = pure $ Model.toList vs }
resolverIntValues  _ = panic "Values resolver: tried with the wrong type."

resolverSpanValues :: GraphQL o m => Model.FieldValues -> Object o m SpanValues
resolverSpanValues vs@(Model.SpanSet _) = pure $
  SpanValues { spanValues = traverse resolverSpanValue (Model.toList vs) }
resolverSpanValues  _ = panic "Values resolver: tried with the wrong type."

--------------------------------------------------------------------------------
-- ***** SpanValue Model -> View
-- |
--
resolverSpanValue :: GraphQL o m => Model.Span -> Object o m Span
resolverSpanValue = \case
  (Model.Span (Model.Red Model.Range{..})) ->
    pure $ Span { rangeStart = pure rangeStart
                , rangeLength =  pure rangeLength
                , reduced = pure True
                }
  (Model.Span (Model.Exp Model.Range{..})) ->
    pure $ Span { rangeStart = pure rangeStart
                , rangeLength = pure rangeLength
                , reduced = pure False
                }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- * GQL Input
-- |
-- (GQL -> Model)
--
fromInputObsEtl :: WithAppContext m
                => ObsEtlInput -> m (Either ObsException Model.ObsETL)
fromInputObsEtl ObsEtlInput {..}
  = catch
    (do result <-
          Model.ObsETL (Model.mkID id) (fromInputSubject subject)  -- :: Input -> Model.Subject
          <$> fromInputMeasurements measurements                -- :: Input -> m Model.Measurements

        logInfoN  $ "Processing Input ObsETL for project: " <> show id
        logDebugF result  -- ⬜ create a summary debug view without levels

        pure $ pure result

    ) handler

    where
       handler :: MonadLogger m => ObsException
               -> m (Either ObsException Model.ObsETL)
       handler e = do logErrorN $ show e; pure $ Left e

--------------------------------------------------------------------------------
-- ** Subject
-- |
-- (GQL -> Model)
--
fromInputSubject :: SubjectInput -> Model.Subject
fromInputSubject SubjectInput {..}
  = Model.Subject
      (fromInputSubType subjectType)
      (fromInputQualities qualities)

fromInputSubType :: Text -> Model.SubKey
fromInputSubType = Model.SubKey
--------------------------------------------------------------------------------
-- *** List Quality
-- |
-- (GQL -> Model)
--
-- /Note/: The Model hosts qualities
--
fromInputQualities :: [QualityInput] -> Model.Qualities
fromInputQualities vs =
  Model.Qualities . Model.qualsFromList $
    bimap Model.QualKey fromInputQualValues . toTuple <$> vs
  where
    toTuple :: QualityInput -> (Name, QualValuesInput)
    toTuple QualityInput{..} = (qualityName, qualityValues)

type Name = Text

--------------------------------------------------------------------------------
-- **** QualValues
-- |
-- (GQL -> Model)
--
-- > QualValuesInput {
-- >   txtValues  :: [Text!]
-- >   intValues  :: [Int!]
-- > }
--
-- > data FieldValues
-- >     = TxtSet  (Set Text)
-- >     | IntSet  (Set Int)
-- >     | SpanSet (Set Span)
--
fromInputQualValues :: QualValuesInput -> Model.QualValues
fromInputQualValues QualValuesInput { txtValues = Just vs } = Model.toQualValues vs
fromInputQualValues QualValuesInput { intValues = Just vs } = Model.toQualValues vs
fromInputQualValues QualValuesInput {} = panic "The values type does not match FieldValues"

--------------------------------------------------------------------------------
-- ** List Measurement
-- |
-- (GQL -> Model)
--
-- /Note/: The Model does not host Measurement (but rather Measurements)
--
fromInputMeasurements :: MonadThrow m => [MeasurementInput] -> m Model.Measurements
fromInputMeasurements vs =
  fmap (Model.Measurements . Model.measFromList)
  <$> sequence
  $ (\(key, mValues) -> (key,) <$> mValues)
  . bimap Model.MeaKey fromInputComponents . toTuple
  <$> vs

  where
    toTuple :: MeasurementInput -> (Type', [ComponentInput])
    toTuple MeasurementInput{..} = (measurementType, components)

type Type' = Text

--------------------------------------------------------------------------------
-- *** List Component
-- |
-- (GQL -> Model)
--
-- /Note/: The Model does not host Component (but rather Components)
--
fromInputComponents :: MonadThrow m => [ComponentInput] -> m Model.Components
fromInputComponents vs =
  fmap (Model.Components . Model.compsFromList)
  <$> sequence
  $ (\(key, mValues) -> (key,) <$> mValues)
  . bimap Model.CompKey fromInputCompValues . toTuple -- :: [(Key, m CompValues)]
  <$> vs

  where
     toTuple :: ComponentInput -> (Name, CompValuesInput)
     toTuple ComponentInput{..} = (componentName, componentValues)

--------------------------------------------------------------------------------
-- **** CompValues
-- |
-- (GQL -> Model)
--
-- > CompValuesInput {
-- >   txtValues  :: [Text!]
-- >   intValues  :: [Int!]
-- >   spanValues :: [Span!]
-- > }
--
-- > data FieldValues
-- >     = TxtSet  (Set Text)
-- >     | IntSet  (Set Int)
-- >     | SpanSet (Set Span)
--
-- where...
-- > input SpanInput {
-- >   rangeStart: Int!
-- >   rangeLength: Int!
-- >   reduced: Boolean!
-- > }
--
-- > Span { span :: TagRedExp Range }
--
--
fromInputCompValues :: MonadThrow m => CompValuesInput -> m Model.CompValues
fromInputCompValues CompValuesInput { txtValues  = Just vs } = pure $ Model.toCompValues vs
fromInputCompValues CompValuesInput { intValues  = Just vs } = pure $ Model.toCompValues vs
fromInputCompValues CompValuesInput { spanValues = Just vs } =
  Model.toCompValues <$> traverse spanFromInput vs
      where
        -- | GraphQL -> Model
        spanFromInput :: MonadThrow m => SpanInput -> m Model.Span
        spanFromInput SpanInput{..}
          | reduced   = Model.mkSpanM Model.Red rangeStart rangeLength
          | otherwise = Model.mkSpanM Model.Exp rangeStart rangeLength

fromInputCompValues CompValuesInput {}
  = panic "The values type does not match that for CompValues"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
