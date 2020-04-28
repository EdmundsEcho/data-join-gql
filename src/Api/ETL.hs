{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{-
    Module that provides the functions required to
    retrieve values from ObsETL.
 -}
module Api.ETL where

---------------------------------------------------------------------------------
import           Protolude        hiding (Type, null)
---------------------------------------------------------------------------------
import qualified Data.Map.Strict  as Map (filterWithKey, fromList, lookup,
                                          member, singleton, toList, union)
import qualified Data.Set         as Set (fromList, intersection)
import           Model.ETL.ObsETL
import           Model.Request    (CompReqValues (..), ReqComponents (..),
                                   fromComponents, fromFieldsCompReqValues,
                                   fromListReqComponents, toTupleCompReqValues)
---------------------------------------------------------------------------------

-- | Functions to retrieve values throughout the Obs structure.
-- > There are five collections in the Obs data structure:
-- > 1. Qualities     parent :: Subject       Key :: Key (QualKey)
-- > 2. QualValues    parent :: Quality       NA
-- > 3. Measurements  parent :: Obs           Key :: Key (MeaKey)
-- > 4. Components    parent :: Measurement   Key :: Key (CompKey)
-- > 5. CompValues    parent :: Component     NA
--
-- QualValues :: FieldValues (wrap Text || Int)
-- CompValues :: FieldValues (wrap Text || Int || Span)
-- Note: The range of QualValue Types is not enforced by the Haskell Type system.
--
-- :: Key -> Node Collection -> Maybe Value
-- Not strictly a collection as there is only one Obs instance.

-- | Retrieve a reference to the one Subject associated with the Obs collection.
-- Not strictly a collection but is one of two branches in the Obs object.
-- The Subject node is ~ Measurements node
lookupSubject :: ObsETL -> Maybe Subject
lookupSubject o = Just (obsSubject o)

selectQualities :: [(Text, FieldValues)] -> Qualities -> Maybe Qualities
selectQualities selects qualities
  = let result = catMaybes $ f qualities <$> selects
     in case result of
       [] -> Nothing
       _  -> Just $ fromListQualities result
  where
    f :: Qualities -> (Text, FieldValues) -> Maybe (QualKey, QualValues)
    f qs (k, searchValues) = do
      let key = mkQualKey k
      values <- lookupQualValues key qs
      result <- filterValues searchValues values
      Just (key, result)

selectComponents :: [(CompKey, FieldValues)] -> Components -> Maybe Components
selectComponents selects components
  = let result = catMaybes $ f components <$> selects
     in case result of
       [] -> Nothing
       _  -> Just $ fromListComponents result
  where
    f :: Components -> (CompKey, FieldValues) -> Maybe (CompKey, CompValues)
    f cs (k, searchValues) = do
      values <- lookupCompValues k cs
      result <- filterValues searchValues values
      Just (k, result)

-- |
-- ComponentReqInput {
--   componentNames
--   componentName
--   componentValues: FieldValuesCompReqInput
-- }
selectReqComponents :: [(CompKey, CompReqValues)] -> Components -> Maybe ReqComponents
selectReqComponents selects components
  = let result = catMaybes $ f components <$> selects
     in case result of
       [] -> Nothing
       _  -> Just $ fromListReqComponents result
  where
    f :: Components -> (CompKey, CompReqValues)
      -> Maybe (CompKey, Maybe CompReqValues)
    -- Reduced case
    f cs (k, CompReqValues (Red searchValues)) = do
      values <- lookupCompValues k cs
      result <- filterValues searchValues values
      Just (k, Just $ CompReqValues (Red result))

    -- Expressed case
    f cs (k, CompReqValues (Exp searchValues)) = do
      values <- lookupCompValues k cs
      result <- filterValues searchValues values
      Just (k, Just $ CompReqValues (Exp result))

-- | Lookup all the qualities that describe the Subject.
-- Note: This is somewhat redundant with lookupSubject but provides symmetry.
-- GraphQL Subject = SubKey (aka: subject type) + Qualities
lookupQualities :: SubKey -> Subject -> Maybe Qualities
lookupQualitis (SubKey Nothing) c = Just (subQualities c)
lookupQualities key@(SubKey (Just _)) c
  | subType c == key = Just (subQualities c)
  | otherwise = Nothing
lookupQualities _ _ = panic "wrong key type"

-- |
-- A request to display a Quality field.
--
lookupQualityKey, lookupQualityName :: QualKey -> Qualities -> Maybe QualKey
lookupQualityKey key@(QualKey _) o =
  if Map.member key (qualities o) then Just key else Nothing
lookupQualityKey  _ _             = panic "wrong key type"
lookupQualityName = lookupQualityKey

-- | Lookup the values of a specific Quality.
lookupQualValues :: QualKey -> Qualities -> Maybe QualValues
lookupQualValues key@(QualKey _) c = Map.lookup key (qualities c)
lookupQualValues _ _               = panic "wrong key type"

-- | Retrieve a reference to the Measurements collection.
-- The collection is one of two branches in the Obs object.
lookupMeasurements :: ObsETL -> Maybe Measurements
lookupMeasurements o = Just (obsMeasurements o)

-- |
-- A request to display a Measurement field.
--
lookupMeasurementType :: MeaKey -> Measurements -> Maybe MeaKey
lookupMeasurementType key@(MeaKey _) o =
  if Map.member key (measurements o) then Just key else Nothing
lookupMeasurementType  _ _             = panic "wrong key type"

-- | Lookup a specific Measurement
-- Note: the API does not define a type for a single Measurement.
-- GraphQL Measurement = MeaKey (aka measurement type) + Components
lookupComponents, lookupMeasurement :: MeaKey -> Measurements -> Maybe Components
lookupComponents key@(MeaKey _) c = Map.lookup key (measurements c)
lookupComponents _ _              = panic "wrong key type"
lookupMeasurement = lookupComponents

-- | Lookup the values of a specific Component.
lookupCompValues :: CoSpKey -> Components -> Maybe CompValues
lookupCompValues key@(CompKey _) c = Map.lookup key (components c)
lookupCompValues key@SpanKey     c = Map.lookup key (components c)
lookupCompValues _ _               = panic "wrong key type"

-- | Subselect a collection :: [Key] -> Collection -> Collection
filterQualities :: [QualKey] -> Qualities -> Qualities
filterQualities ks o = Qualities $ Map.filterWithKey (mkMapFilter ks) (qualities o)

filterMeasurements :: [MeaKey] -> Measurements -> Measurements
filterMeasurements ks o = Measurements $ Map.filterWithKey (mkMapFilter ks) (measurements o)

filterComponents :: [CompKey] -> Components -> Components
filterComponents ks o = Components $ Map.filterWithKey (mkMapFilter ks) (components o)

-- |
-- /Note/: A user requesting all values of a component is isomorphic with
-- @Exp CompValues@. The request is for a series of the data (long view -> wide view)
filterReqComponents :: [CompKey] -> Components -> ReqComponents
filterReqComponents ks o =
  fromComponents Exp . Components $ Map.filterWithKey (mkMapFilter ks) (components o)

-- |
-- Unifying filter for FieldValues
filterValues, filterQualReqValues :: FieldValues -> FieldValues -> Maybe FieldValues
filterValues search values
  | null search = Just values
  | otherwise = go search values
  where
    go (TxtSet s) (TxtSet vs)   = Just . TxtSet $ Set.intersection s vs
    go (IntSet s) (IntSet vs)   = Just . IntSet $ Set.intersection s vs
    go (SpanSet s) (SpanSet vs) = Just . SpanSet $ Set.intersection s vs
    go _ _                      = Nothing
filterQualReqValues = filterValues

-- |
-- Unifying filter for CompReqValues
filterCompReqValues :: CompReqValues -> FieldValues -> Maybe CompReqValues
filterCompReqValues search values = do
  let (search', reduced) = toTupleCompReqValues search
  result <- filterValues search' values
  pure $ fromFieldsCompReqValues reduced result

-- fromFieldsCompReqValues :: Reduced -> CompValues -> CompReqValues
-- | Retrieve subset of values /Deprecated/
filterTxtValues :: [Text] -> FieldValues -> FieldValues
filterTxtValues search inSet =
  TxtSet $ Set.intersection (unTxtSet $ mkSearchTxt search) (unTxtSet inSet)
  where mkSearchTxt xs = TxtSet (Set.fromList xs)

-- | Retrieve subset of values @:: IntEtl@ /Deprecated/
filterIntValues :: [Int] -> FieldValues -> FieldValues
filterIntValues search inSet =
  IntSet $ Set.intersection (unIntSet $ mkSearchInt search) (unIntSet inSet)
  where mkSearchInt xs = IntSet (Set.fromList xs)

-- | Retrieve subset of values @:: Span@ /Deprecated/
-- Returns @[Span] -> FieldValues@ for every Span that is within the @ETL@
-- @FieldValues@ universe.
filterSpanValues :: [Span] -> FieldValues -> FieldValues
filterSpanValues search inSet = SpanSet . Set.fromList $ filterSpanReq search inSet
  where
    filterSpanReq :: [Span] -> SpanValues -> [Span]
    filterSpanReq reqs universe = filter (`isValidSpanReq` universe) reqs
      where
        isValidSpanReq :: Span -> SpanValues -> Bool
        isValidSpanReq req (SpanSet univ) = any (req `isWithin`) univ
        isValidSpanReq _ _ = panic "isValidSpanReq: Tried with the wrong FieldValue type"

-- | Internal. Pre-processes input for the filter node @:: Map@ functions
mkMapFilter :: [Key] -> Key -> a -> Bool
mkMapFilter search k _ = k `elem` search

-- |
-- == Validate and reconstruct
-- Utilized by functions that validate and reconstruct core types
-- e.g., "Schemas.Request.Types.RequestInput"
qualitiesToList :: Qualities -> [(Key,QualValues)]
qualitiesToList (Qualities qs) = mapToList qs

componentsToList :: Components -> [(Key,CompValues)]
componentsToList (Components cs) = mapToList cs

setFromList ::(Ord a) => [a] -> Set a
setFromList = Set.fromList

mapToList :: Map k vs -> [(k,vs)]
mapToList = Map.toList

mapFromList ::(Ord k) => [(k,vs)] -> Map k vs
mapFromList = Map.fromList

mapAppend :: (Ord k) => k -> a -> Map k a -> Map k a
mapAppend k a mp = Map.union mp $ singleton k a

singleton :: (Ord k) => k -> a -> Map k a
singleton = Map.singleton
