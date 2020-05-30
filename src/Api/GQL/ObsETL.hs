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
-- Description: ObsEtl UI access point
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
  , resolverQualValues
  , resolverSpanValue
  , resolverSubType

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
import qualified Model.ETL.ObsETL       as Model hiding (fromList)
import qualified Model.ETL.Transformers as Trans
-------------------------------------------------------------------------------
import           Api.GqlHttp
-------------------------------------------------------------------------------
import           Api.GQL.Schemas.ObsETL
import           Api.GQL.Schemas.Shared
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- * ObsETL Model -> View
-- |
--
resolverObsEtl :: GraphQL o => Model.ObsETL -> Object o ObsETL
resolverObsEtl Model.ObsETL {..} =
  pure $
    ObsETL
      (resolverID obsID)
      (resolverSubject obsSubject)            -- :: Model.Subject -> Subject
      (resolverMeasurements obsMeasurements)  -- :: Model.Measurements -> [Measurement]

resolverID :: GraphQL o => Model.ID -> Value o Text
resolverID = pure . Model.unID

-------------------------------------------------------------------------------
-- ** Subject Model -> View
-- |
--
resolverSubject :: GraphQL o => Model.Subject -> Object o Subject
resolverSubject Model.Subject {..} =
  pure $
    Subject
      (resolverSubType subType)        -- :: Model.Key       -> String!
      (resolverQualities subQualities) -- :: Model.Qualities -> [Quality!]!

resolverSubType :: GraphQL o => Model.SubKey -> Value o Text
resolverSubType = pure . Model.unKey

--------------------------------------------------------------------------------
-- ** Measurements Model -> View
-- |
-- /Note/: The model does not host Measurement
--
resolverMeasurements :: GraphQL o
                     => Model.Measurements -> ArrayObject o Measurement
resolverMeasurements o'
  = traverse identity $ Trans.fromMeasurements resolverMeasurement o'
  where
    resolverMeasurement :: GraphQL o
                        => Model.MeaKey -> Model.Components -> Object o Measurement
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
resolverComponent :: GraphQL o
                  => Model.CompKey -> Model.CompValues -> Object o Component
resolverComponent key o' =
  pure $
    Component
       (pure $ Model.unKey key)
       (resolverCompValues o')

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
resolverCompValues :: GraphQL o => Model.CompValues -> Object o ComponentValues
resolverCompValues (Model.TxtSet o') =
  ComponentValuesTxtValues <$> resolverTxtValues (Model.TxtSet o')

resolverCompValues (Model.IntSet o') =
  ComponentValuesIntValues <$> resolverIntValues (Model.IntSet o')

resolverCompValues (Model.SpanSet o') =
  ComponentValuesSpanValues <$> resolverSpanValues (Model.SpanSet o')

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
resolverQualities :: GraphQL o => Model.Qualities -> ArrayObject o Quality
resolverQualities o'
  = traverse identity $ Trans.fromQualities resolverQuality o'

resolverQuality :: GraphQL o
                => Model.QualKey -> Model.QualValues -> Object o Quality
resolverQuality key o' =
  pure $
    Quality
       (pure $ Model.unKey key)
       (resolverQualValues o')

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
resolverQualValues :: GraphQL o => Model.QualValues -> Object o QualityValues
resolverQualValues (Model.TxtSet o') =
  QualityValuesTxtValues <$> resolverTxtValues (Model.TxtSet o')

resolverQualValues (Model.IntSet o') =
  QualityValuesIntValues <$> resolverIntValues (Model.IntSet o')

resolverQualValues  _ = panic "QualValues resolver: tried with the wrong type."

--------------------------------------------------------------------------------
-- **** FieldValues Model -> View
-- |
-- FieldValues :: TxtValues | IntValues | SpanValues
--
resolverTxtValues :: GraphQL o => Model.FieldValues -> Object o TxtValues
resolverTxtValues vs@(Model.TxtSet _) = pure $
  TxtValues { txtValues = pure $ Model.toList vs }
resolverTxtValues  _ = panic "Values resolver: tried with the wrong type."

resolverIntValues :: GraphQL o => Model.FieldValues -> Object o IntValues
resolverIntValues vs@(Model.IntSet _) = pure $
  IntValues { intValues = pure $ Model.toList vs }
resolverIntValues  _ = panic "Values resolver: tried with the wrong type."

resolverSpanValues :: GraphQL o => Model.FieldValues -> Object o SpanValues
resolverSpanValues vs@(Model.SpanSet _) = pure $
  SpanValues { spanValues = traverse resolverSpanValue (Model.toList vs) }
resolverSpanValues  _ = panic "Values resolver: tried with the wrong type."

--------------------------------------------------------------------------------
-- ***** SpanValue Model -> View
-- |
--
resolverSpanValue :: GraphQL o => Model.Span -> Object o Span
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
--------------------------------------------------------------------------------
-- * GQL Input
-- |
-- (GQL -> Model)
--
fromInputObsEtl :: (MonadLogger m, MonadCatch m, MonadThrow m)
                => ObsEtlInput -> m (Either ObsException Model.ObsETL)
fromInputObsEtl ObsEtlInput {..}
  = catch
    (do result <-
          Model.ObsETL Model.mkID (fromInputSubject subject)  -- :: Input -> Model.Subject
          <$> fromInputMeasurements measurements                -- :: Input -> m Model.Measurements
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
