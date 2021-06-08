{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Api.GQL.SelectValuesResolver
-- Description : Resolvers for the selectValues QUERY
-- Copyright   : (c) Lucivia LLC, 2021
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
module Api.GQL.SelectValuesResolver (
  resolverSelectValues
) where
---------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import qualified Model.ETL.ObsETL           as Model
import           Model.Search               (toEtlFromFragment, toFragmentETL, selectWithTerm)
---------------------------------------------------------------------------------
import           WithAppContext             (WithAppContext)
import qualified WithAppContext             as App hiding (WithAppContext)
---------------------------------------------------------------------------------
import           Api.GQL.ObsETL
import           Api.ETL                    (getComponentValues, getQualityValues)
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.ObsETL
import           Api.GQL.Types
import qualified Api.GQL.Schemas.ObsETL as GQL
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-- |
--
resolverSelectValues :: (GraphQL o m, WithAppContext m)
                  => Model.ObsETL
                  -> Maybe FromQuality
                  -> Maybe FromComponent
                  -> Object o m SelectedValues

resolverSelectValues obsEtl (Just FromQuality {..}) _ = do
  lift $ App.logInfoN ("Received the search request" <> show contains)
  let maybeEtl = getQualityValues qualityName obsEtl

  case (maybeEtl, contains, startsWith, endsWith ) of

    -- The search engine: selectWithTerm
    (Just values, Just contains', _, _ ) -> do
      result <- lift $ selectWithTerm contains' (toFragmentETL values)
      SelectedValuesTxtValues <$> resolverTxtValues (toEtlFromFragment result)

    (Just values, Nothing, Just startsWith', _) -> do
      result <- lift $ selectWithTerm "" (toFragmentETL values)
      SelectedValuesTxtValues <$> resolverTxtValues (toEtlFromFragment result)

    (Just values, Nothing, Nothing, Just endsWith') -> do
      result <- lift $ selectWithTerm "" (toFragmentETL values)
      SelectedValuesTxtValues <$> resolverTxtValues (toEtlFromFragment result)

    (Just values, Nothing, Nothing, Nothing) -> do
      result <- lift $ selectWithTerm "" (toFragmentETL values)
      SelectedValuesTxtValues <$> resolverTxtValues (toEtlFromFragment result)

    (Nothing, _, _, _ ) -> do
      lift $ App.logInfoN "There is no quality by that name"
      SelectedValuesEmpty <$> resolverEmpty


resolverSelectValues obsEtl _ (Just FromComponent {..}) = do
  let maybeEtl = getComponentValues measurementType componentName obsEtl

  case (maybeEtl, contains, startsWith, endsWith) of
    (Just values, Just contains', _, _) -> do
      result <- lift $ selectWithTerm contains' (toFragmentETL values)
      SelectedValuesTxtValues <$> resolverTxtValues (toEtlFromFragment result)

    (Just values, Nothing, Just startsWith', _) -> do
      result <- lift $ selectWithTerm "" (toFragmentETL values)
      SelectedValuesTxtValues <$> resolverTxtValues (toEtlFromFragment result)

    (Just values, Nothing, Nothing, Just endsWith') -> do
      result <- lift $ selectWithTerm "" (toFragmentETL values)
      SelectedValuesTxtValues <$> resolverTxtValues (toEtlFromFragment result)

    (Just values, Nothing, Nothing, Nothing) -> do
      result <- lift $ selectWithTerm "" (toFragmentETL values)
      SelectedValuesTxtValues <$> resolverTxtValues (toEtlFromFragment result)

    (Nothing, _, _, _ ) -> do
      lift $ App.logInfoN "There is no measurement component by that name"
      SelectedValuesEmpty <$> resolverEmpty


resolverSelectValues _ Nothing Nothing = do
      lift $ App.logInfoN "The select fields are empty"
      SelectedValuesEmpty <$> resolverEmpty



---------------------------------------------------------------------------------
-- |
-- Constant
--
--
resolverEmptyEnum :: Applicative f => f GQL.EmptySingleton
resolverEmptyEnum = pure GQL.EMPTY

resolverEmpty :: (GraphQL o m , WithAppContext m) => Object o m GQL.Empty
resolverEmpty = pure (GQL.Empty resolverEmptyEnum)
