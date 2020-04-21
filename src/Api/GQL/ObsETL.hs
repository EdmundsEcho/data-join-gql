{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

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
  -- * GQL Root
    resolverGetObsEtl
  , resolverGetStatus
  , resolverNewObsETL
  , Query (..)
  , Mutation (..)
  -- * Api.GQL Shared Views
  , fromInputQualities
  , Quality
  , Span
  , ComponentValues(..)
  , resolverCompValues
  , resolverQualities
  , resolverSpanValue
  , resolverSubType
  -- * Api.GQL Shared Inputs
  , FieldValuesInput(..)
  , QualityInput(..)
  , SpanInput(..)
  )
where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Morpheus.Document (importGQLDocument)
import           Data.Morpheus.Types
-------------------------------------------------------------------------------
import           Control.Concurrent.STM
-------------------------------------------------------------------------------
import qualified Model.ETL.ObsETL       as Model
import qualified Model.ETL.Transformers as Trans
-- AppObs types
import           Api.GqlHttp
import qualified AppTypes               as App
-------------------------------------------------------------------------------
importGQLDocument "src/Api/GQL/schema.shared.graphql"
importGQLDocument "src/Api/GQL/schema.obsetl.graphql"
-------------------------------------------------------------------------------
-- |
-- == GQL Query Resolvers
resolverGetObsEtl :: OptionalObject QUERY ObsETL
resolverGetObsEtl = do
  obsEtl' <- fmap App.db getDb
  case obsEtl' of
    App.DataObsETL o  -> do
      obsEtl <- resolverObsEtl o
      pure $ Just obsEtl
    _ -> pure Nothing

resolverGetStatus :: Value QUERY Text
resolverGetStatus = fmap App.status getDb

-------------------------------------------------------------------------------
-- |
-- == GQL Db
getDb :: GraphQL o => Value o App.Database
getDb = do
  dbTVar <- lift $ asks App.database
  liftIO . atomically $ readTVar dbTVar

--------------------------------------------------------------------------------
-- |
-- == GQL Mutation Resolvers
-- TODO: Return a Maybe ObsETL
resolverNewObsETL :: NewObsEtlArgs -> Object MUTATION ObsETL
resolverNewObsETL NewObsEtlArgs {value = newObs'} = do
  let newObs = fromInputObsEtl newObs'
  db <- getDb
  let newDb = db { App.db = App.DataObsETL newObs }
  dbTVar <- lift $ asks App.database
  liftIO . atomically $ writeTVar dbTVar newDb
  resolverObsEtl newObs


-------------------------------------------------------------------------------
-- |
-- === Model -> View resolvers
-- fromModelToView
resolverObsEtl :: GraphQL o => Model.ObsETL -> Object o ObsETL
resolverObsEtl Model.ObsETL {..} =
  pure $
    ObsETL
      (resolverID obsID)
      (resolverSubject obsSubject)            -- :: Model.Subject -> Subject
      (resolverMeasurements obsMeasurements)  -- :: Model.Measurements -> [Measurement]

resolverID :: GraphQL o => Model.ID -> Value o Text
resolverID = pure . Model.unID

-- |
-- ==== fromInput
fromInputObsEtl :: ObsEtlInput -> Model.ObsETL
fromInputObsEtl ObsEtlInput {..}
  = Model.ObsETL
      Model.mkID
      (fromInputSubject subject)            -- :: Input -> Model.Subject
      (fromInputMeasurements measurements)  -- :: Input -> Model.Measurements

-------------------------------------------------------------------------------
-- |
-- === Model -> View resolvers
-- fromModelToView
resolverSubject :: GraphQL o => Model.Subject -> Object o Subject
resolverSubject Model.Subject {..} =
  pure $
    Subject
      (resolverSubType subType)        -- :: Model.Key       -> String!
      (resolverQualities subQualities) -- :: Model.Qualities -> [Quality!]!

resolverSubType :: GraphQL o => Model.SubKey -> Value o Text
resolverSubType = pure . Model.unKey

-- |
-- ==== fromInput
fromInputSubject :: SubjectInput -> Model.Subject
fromInputSubject SubjectInput {..}
  = Model.Subject
      (fromInputSubType subjectType)
      (fromInputQualities qualities)

fromInputSubType :: Text -> Model.SubKey
fromInputSubType = Model.SubKey . Just
--------------------------------------------------------------------------------
-- |
-- === Measurements
--
resolverMeasurements :: GraphQL o
                     => Model.Measurements -> ArrayObject o Measurement
resolverMeasurements o'
  = traverse identity $ Trans.fromMeasurements resolverMeasurement o'

--------------------------------------------------------------------------------
-- |
-- === Measurement
-- /Note/: The model does not host Measurement
--
resolverMeasurement :: GraphQL o
                    => Model.MeaKey -> Model.Components -> Object o Measurement
resolverMeasurement key o' =
  pure $
    Measurement
       (pure $ Model.unKey key)
       (traverse identity $ Trans.fromComponents resolverComponent o')

--------------------------------------------------------------------------------
-- |
-- ==== Measurement (GQL -> Model)
-- /Note/: The Model does not host Measurement (but rather Measurements)
--
fromInputMeasurements :: [MeasurementInput] -> Model.Measurements
fromInputMeasurements vs =
  Model.Measurements . Model.measFromList $
    bimap Model.MeaKey fromInputComponents . toTuple <$> vs
  where
    toTuple :: MeasurementInput -> (Type', [ComponentInput])
    toTuple MeasurementInput{..} = (measurementType, components)

type Type' = Text
--------------------------------------------------------------------------------
-- |
-- === Component
-- Model -> GQL View
--
-- > Components { components :: Map Key CompValues }
--
-- > type Component {
-- >   componentName: String!
-- >   componentValues: ComponentValues!
-- > }
resolverComponent :: GraphQL o
                  => Model.CompKey -> Model.CompValues -> Object o Component
resolverComponent key o' =
  pure $
    Component
       (pure $ Model.unKey key)
       (resolverCompValues o')

--------------------------------------------------------------------------------
-- |
-- ==== Components (GQL -> Model)
-- /Note/: The Model does not host Component (but rather Components)
fromInputComponents :: [ComponentInput] -> Model.Components
fromInputComponents vs =
  Model.Components . Model.compsFromList $
    bimap Model.CompKey fromInputFieldValues . toTuple <$> vs
  where
    toTuple :: ComponentInput -> (Name, FieldValuesInput)
    toTuple ComponentInput{..} = (componentName, componentValues)

--------------------------------------------------------------------------------
-- |
-- === Model CompValues, GQL Union ComponentValues
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
-- |
-- === Quality
--
resolverQualities :: GraphQL o => Model.Qualities -> ArrayObject o Quality
resolverQualities o'
  = traverse identity $ Trans.fromQualities resolverQuality o'

-- | GQL Quality is an object with two fields
--      qualityName :: Text
--      qualityValues :: Union QualityValuesTxtValues ...
--   Model Key + TxtSet (Set Text) | IntSet (Set Text)
resolverQuality :: GraphQL o
                => Model.QualKey -> Model.QualValues -> Object o Quality
resolverQuality key o' =
  pure $
    Quality
       (pure $ Model.unKey key)
       (resolverQualValues o')

--------------------------------------------------------------------------------
-- |
-- ==== Quality (GQL -> Model)
-- /Note/: The Model hosts qualities
--
fromInputQualities :: [QualityInput] -> Model.Qualities
fromInputQualities vs =
  Model.Qualities . Model.qualsFromList $
    bimap Model.QualKey fromInputFieldValues . toTuple <$> vs
  where
    toTuple :: QualityInput -> (Name, FieldValuesInput)
    toTuple QualityInput{..} = (qualityName, qualityValues)

type Name = Text

--------------------------------------------------------------------------------
-- |
-- === Model CompValues, GQL Union ComponentValues
-- > GQL { txtValues :: [Text] } | { intValues :: [Int] }
-- > Model TxtSet (Set Text) | IntSet (Set Int)
-- Pattern match to delegate to one of the 3 value types
--
resolverQualValues :: GraphQL o => Model.QualValues -> Object o QualityValues
resolverQualValues (Model.TxtSet o') =
  QualityValuesTxtValues <$> resolverTxtValues (Model.TxtSet o')

resolverQualValues (Model.IntSet o') =
  QualityValuesIntValues <$> resolverIntValues (Model.IntSet o')

resolverQualValues  _ = panic "QualValues resolver: tried with the wrong type."

--------------------------------------------------------------------------------
-- |
-- === FieldValues :: TxtValues | IntValues | SpanValues
--
resolverTxtValues :: GraphQL o => Model.FieldValues -> Object o TxtValues
resolverTxtValues (Model.TxtSet o') = pure $
  TxtValues { txtValues = pure $ Trans.valuesToList o' }
resolverTxtValues  _ = panic "Values resolver: tried with the wrong type."

resolverIntValues :: GraphQL o => Model.FieldValues -> Object o IntValues
resolverIntValues (Model.IntSet o') = pure $
  IntValues { intValues = pure $ Trans.valuesToList o' }
resolverIntValues  _ = panic "Values resolver: tried with the wrong type."

resolverSpanValues :: GraphQL o => Model.FieldValues -> Object o SpanValues
resolverSpanValues (Model.SpanSet o') = pure $
  SpanValues { spanValues = traverse resolverSpanValue (Trans.valuesToList o') }

resolverSpanValues  _ = panic "Values resolver: tried with the wrong type."

--------------------------------------------------------------------------------
-- |
-- ==== FieldValuesInput (GQL -> Model)
-- > FieldValuesInput {
-- >   txtValues  :: Maybe [Text]
-- >   intValues  :: Maybe [Int]
-- >   spanValues :: Maybe [Span]
-- > }
--
fromInputFieldValues :: FieldValuesInput -> Model.FieldValues
fromInputFieldValues (FieldValuesInput (Just ts) _ _) = Model.TxtSet  (Model.fromList ts)
fromInputFieldValues (FieldValuesInput _ (Just is) _) = Model.IntSet  (Model.fromList is)
fromInputFieldValues (FieldValuesInput _ _ (Just ss)) = Model.SpanSet
                                                       (Model.fromList (fromInputSpan <$> ss))
fromInputFieldValues FieldValuesInput {} = panic "fromInput: Failed to provide values"
--------------------------------------------------------------------------------
-- |
-- === SpanValue
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
-- |
-- ==== SpanInput (GQL -> Model)
--
fromInputSpan :: SpanInput -> Model.Span
fromInputSpan SpanInput {..}
  | reduced   = Model.Span $ Model.Red (Model.Range rangeStart rangeLength)
  | otherwise = Model.Span $ Model.Exp (Model.Range rangeStart rangeLength)

--------------------------------------------------------------------------------
