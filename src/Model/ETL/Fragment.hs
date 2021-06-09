{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections          #-}

-- |
-- Module      : Model.ETL.Fragment
-- Description : Type class definitions
-- Copyright   : (c) Lucivia LLC, 2020
-- Maintainer  : edmund.cape@lucivia.com
-- Stability   : experimental
-- Portability : POSIX
--
--
module Model.ETL.Fragment where
---------------------------------------------------------------------------------
import           Data.Coerce                    ( coerce )
import           Data.Text                      ( intercalate )
import           Protolude               hiding ( from
                                                , get
                                                , intercalate
                                                , null
                                                , toList
                                                )
---------------------------------------------------------------------------------
import           Model.ETL.Components    hiding ( lookup
                                                , null
                                                , size
                                                , toList
                                                )
import qualified Model.ETL.Components          as Components
                                                ( lookup )
import           Model.ETL.FieldValues          ( FieldValues(..)
                                                , FilterRange(..)
                                                , ValuesReqEnum(..)
                                                , ValuesReq(..)
                                                , AntiRequestEnum(..)
                                                , FieldValue(..)
                                                , unwrapReqEnum
                                                )
import           Model.ETL.Key                  ( Key(CompKey, MeaKey)
                                                , unKey
                                                )
import           Model.ETL.ObsETL               ( MeaKey
                                                , Measurements(..)
                                                , SubKey
                                                , Subject(..)
                                                , mkMeaKey
                                                )
import qualified Model.ETL.ObsETL              as Measurements
                                                ( lookup
                                                , null
                                                , size
                                                )
import           Model.ETL.Qualities     hiding ( lookup
                                                , null
                                                , size
                                                , toList
                                                )
import qualified Model.ETL.Qualities           as Qualities
                                                ( lookup )
import           Model.ETL.Span                 ( Span(Span) )
import qualified Model.ETL.Span                as Span
import           Model.ETL.TagRedExp
import           Model.SearchFragment
---------------------------------------------------------------------------------
import qualified Data.Set                      as Set
                                                ( fromList
                                                , intersection
                                                , null
                                                , size
                                                , toList
                                                , filter
                                                )
---------------------------------------------------------------------------------
import           Control.Monad.Logger
import           Control.Monad.Trans.Maybe
---------------------------------------------------------------------------------
import           Api.GQL.Schemas.Request        ( ComponentMixInput(..)
                                                , ComponentReqInput
                                                , QualityMixInput
                                                , QualityReqInput
                                                , RequestKey(..)
                                                , SubsetCompMixReq
                                                , SubsetCompReq
                                                , SubsetQualReq
                                                )
---------------------------------------------------------------------------------
--
-- |
-- Unifying classtype for a fragment of ETL data ~ collection
-- without a key.
--
class Fragment fragment where
  null     :: fragment -> Bool
  len      :: fragment -> Int
  showLen  :: fragment -> Text
  showLen f = "levels: " <> show (len f)

-- |
instance Fragment a => Fragment (Maybe a) where
  null Nothing   = True
  null (Just vs) = null vs
  len Nothing   = 0
  len (Just vs) = len vs

-- |
instance Fragment [a] where
  null [] = True
  null _  = False
  len [] = 0
  len xs = length xs


-- |
instance Fragment Measurements where
  null = Measurements.null
  len  = Measurements.size

-- |
instance Fragment FieldValues where
  null (TxtSet  set) = Set.null set
  null (IntSet  set) = Set.null set
  null (SpanSet set) = Set.null set
  null Empty         = True

  len (TxtSet  set) = Set.size set
  len (IntSet  set) = Set.size set
  len (SpanSet set) = Set.size set
  len Empty         = 0
-- |
--
instance Fragment ValuesReqEnum where
  null (ExcludeRequest vs) = null vs
  null (IncludeRequest vs) = null vs
  null (NA             vs) = null vs

  len (ExcludeRequest vs) = len vs
  len (IncludeRequest vs) = len vs
  len (NA             vs) = len vs

-- |
instance ToList ValuesReqEnum FieldValue where
  toList = toList . fst . unwrapReqEnum

-- |
-- 👍 The only time this version of toList is called
-- is when the return type is [Span]
instance ToList ValuesReqEnum Span where
  toList = toList . fst . unwrapReqEnum

-- |
--
instance Fragment (ValuesReq 'Include) where
  null (ValuesReq vs) = null vs
  len (ValuesReq vs) = len vs

-- |
--
instance Fragment (ValuesReq 'Exclude) where
  null (ValuesReq vs) = null vs
  len (ValuesReq vs) = len vs

  -- = ExcludeRequest (ValuesReq 'Exclude)
  -- | IncludeRequest (ValuesReq 'Include)
  -- | NA FieldValues

instance Fragment (SearchFragment FieldValues 'ETL) where
  null = (null @FieldValues) . coerce
  len  = (len @FieldValues) . coerce
instance Fragment (SearchFragment FieldValues 'Req) where
  null = (null @FieldValues) . coerce
  len  = (len @FieldValues) . coerce
instance Fragment (SearchFragment FieldValues 'ETLSubset) where
  null = (null @FieldValues) . coerce
  len  = (len @FieldValues) . coerce

-- |
-- 'Model.ETL.FieldValues.FilterRange' can equate to one or more values
-- otherwise captured in IntTxt and IntSet.
--
-- Utilized to determine the symbol value in a 'Relation'.
--
instance Fragment FilterRange where
  null v = len v == 0
  len FilterRange {..} = maximum [0, filterEnd - filterStart + 1]

---------------------------------------------------------------------------------
-- ** To and FromList
-- |
--
class ToList a item where
  toList :: a -> [item]

-- |
instance ToList FieldValues Text where
  toList (TxtSet vs) = Set.toList vs
  toList _           = panic $ message "toList" "Text"

-- |
instance ToList FieldValues Int where
  toList (IntSet vs) = Set.toList vs
  toList _           = panic $ message "toList" "Int"

-- |
instance ToList FieldValues Span where
  toList (SpanSet vs) = Set.toList vs
  toList _            = panic $ message "toList" "Span"

-- |
instance ToList FieldValues FieldValue where
  toList (TxtSet  vs) = TxtValue <$> Set.toList vs
  toList (IntSet  vs) = IntValue <$> Set.toList vs
  toList (SpanSet vs) = SpanValue <$> Set.toList vs
  toList Empty        = []

message :: Text -> Text -> Text
message func' type' =
  "ToList type mismatch:\n Called "
    <> type'
    <> " "
    <> func'
    <> " with the wrong input data type."

---------------------------------------------------------------------------------
-- |
class FromList fragment item | item -> fragment where
   fromList :: [item] -> fragment

-- |
instance FromList FieldValues Text where
  fromList = TxtSet . Set.fromList

-- |
instance FromList FieldValues Int where
  fromList = IntSet . Set.fromList

-- |
instance FromList FieldValues Span where
  fromList = SpanSet . Span.fromListEtl

---------------------------------------------------------------------------------
-- |
class Intersection a where
  intersection  :: a -> a -> a

-- |
instance Intersection FieldValues where
  intersection (TxtSet get) (TxtSet from) = TxtSet $ Set.intersection get from
  intersection (IntSet get) (IntSet from) = IntSet $ Set.intersection get from
  intersection (SpanSet get) (SpanSet from) =
    SpanSet $ Span.intersection get from
  intersection _ _ = panic "Type mismatch: intersection using different types"

---------------------------------------------------------------------------------
-- |
class HasFilterSearch i c where
  filterSearch  :: (i -> Bool) -> c -> c

-- |
instance HasFilterSearch Text FieldValues where
  filterSearch p (TxtSet values) = TxtSet $ Set.filter p values
  filterSearch _ _ =
    panic "Type mismatch:  using a Text filter on the wrong FieldValues"

-- |
instance HasFilterSearch Int FieldValues where
  filterSearch p (IntSet values) = IntSet $ Set.filter p values
  filterSearch _ _ =
    panic "Type mismatch:  using an Int filter on the wrong FieldValues"

-- |
instance HasFilterSearch Span FieldValues where
  filterSearch p (SpanSet values) = SpanSet $ Set.filter p values
  filterSearch _ _ =
    panic "Type mismatch:  using a Span filter on the wrong FieldValues"


---------------------------------------------------------------------------------
-- ** NameTag
-- |
-- === Overview
-- Text token that when combined generate a FieldName :: Text for each of the
-- 'Api.GQL.MatrixSpec.Filter' values. This format reflects the Long -> Wide
-- transformation.
--
-- /Note/: The smallest unit is CompKey :: FieldValues.  In a series of fields
-- (TagRedExp = Exp) FieldValues will be a singleton.
--
class NameTagC fragment where
  nameTag  :: fragment -> NameTag

type NameTag = Text

-- | LHS
instance NameTagC Key where
  nameTag key@(MeaKey  _) = "MeaType::" <> unKey key
  nameTag key@(CompKey _) = unKey key
  nameTag _               = mempty

-- |
-- NameTag does not need to interprit TagRedExp
--
-- RHS
--
instance NameTagC FieldValues where
  nameTag vs@(TxtSet _) = go_ (nameTag @Text) vs
  nameTag vs@(IntSet _) = go_ (nameTag @Int) vs
  nameTag Empty         = mempty
  nameTag (SpanSet _)   = panic "No name tag for SpanSet; use FilterRange"

-- private
go_ :: (ToList a b, Fragment a) => (b -> Text) -> a -> Text
go_ tagFn vs | len vs == 1 = intercalate "," (tagFn <$> toList vs)
             | otherwise   = "$" <> intercalate "," (tagFn <$> toList vs) <> "$"

-- | RHS
instance NameTagC Text where
  nameTag v = v

-- | RHS
instance NameTagC Int where
  nameTag = show

-- | RHS
instance NameTagC FilterRange where
  nameTag FilterRange {..} = if filterStart == filterEnd
    then show filterStart
    else show filterStart <> "_" <> show filterEnd

---------------------------------------------------------------------------------
-- ** FieldCount
-- |
-- === Overview
-- This is a /fullfilled/ 'Model.Request' concept. 'fieldCount'
-- reports the number of fields a fullfilled request generates
-- in the matrix. The count does not include derived fields.
--
class FieldCount fragment where

  fieldCount  :: fragment -> Int

  -- debugging capacity
  fieldCountM :: (MonadLogger m, Show fragment)
              => fragment -> m Int

  fieldCountM fragment = do
    let mess = ("fieldCount: "::Text) <> show (fieldCount fragment)
             <> "\nfragment:\n" <> show fragment
    logDebugN mess
    pure $ fieldCount fragment

-- |
-- ==== Contribution to the field count
-- 'Model.ETL.Span' is the only ETL construct that also needs implement this
-- typeclass.  This location for the implementation is to avoid the
-- orphan instance error.
--
-- Other implementations can be found in the 'Model.Request' module.
--
instance FieldCount Span where
  fieldCount Span { span = span' } = case span' of
    (Red _    ) -> 1
    (Exp range) -> Span.rangeLength range

-- ** FieldCounts
-- |
-- === Overview
--
-- A capacity to lookup "meta-data".
--
-- When the GQL constucts require item specific information where the @Model@
-- only represents the data in a @Map@ or other collection, the typeclass
-- provides access to the multiple field count values by way of @[(key, fieldCount)]@.
-- e.g., 'Model.Request.ReqComponents' implements
-- 'fieldCounts' @:: [(compName, fieldCount)]@. This may be
-- the only construct that benefits from implementing the typeclass.
--
class FieldCounts collection where
  fieldCounts :: collection -> [(Key, Int)]
  getCount    :: Key -> collection -> Int
  getCount key vs = maybe 0 snd (find (\ (k, _) -> key == k) (fieldCounts vs))


-- ** GetEtlFragment
-- |
-- === Overview
-- This is a ETL collection property that enables retrieving data from it.
-- This is an ETL concept. Not a Request concept /per se/.
--
-- ETL collection -> item
--
-- === Benefit
-- The 'getEtlFragment' function can be called in a request using a generic
-- 'Text' key. The ETL fragment will use the key depending on the specifics
-- of the data structure.
--
class GetEtlFragment collection k values | collection -> k values where

  -- |
  -- Minimum implementation: how to use a key from anywhere to pull
  -- a specific fragment (values) from a collection of ETL data fragment.
  --
  -- k :: matching the request input.
  --
  getValues :: Ord k => collection -> Text -> Maybe (k,values)

  -- |
  --
  member :: (Ord k, RequestKey request)
            => request
            -> collection
            -> Bool
  member req coll = isJust (getEtlFragment coll req)

  -- |
  -- collection of fragments -> fragment using a key from a request object.
  --
  getEtlFragment :: (Ord k, RequestKey request)
            => collection
            -> request
            -> Maybe (k, values)

  getEtlFragment coll req = do
     keyReq  <- requestKey req
     getValues coll keyReq

  -- |
  -- MaybeT version
  --
  getEtlFragmentT :: (Ord k, RequestKey request, Monad m)
            => collection
            -> request
            -> MaybeT m (k, values)

  getEtlFragmentT coll req = MaybeT . pure $ getEtlFragment coll req


---------------------------------------------------------------------------------
-- |
-- Instances align values extracted from GetEtlFragment with the request.
--
class (RequestKey request, GetEtlFragment etl k fragment)
  => IsSubset request etl k fragment | request -> etl k fragment where

  isSubset :: Ord k => request -> etl -> Bool
  isSubset req collection = case requestKey req of
     Nothing -> True
     Just _  -> member req collection

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-- |
instance GetEtlFragment Measurements MeaKey Components where
  getValues measurements k =
    let key = mkMeaKey k in (key, ) <$> Measurements.lookup measurements key

-- |
instance GetEtlFragment Subject SubKey Qualities where
  -- getValues :: Ord k => collection -> Text -> Maybe (k,values)
  getValues Subject {..} _ = Just (subType, subQualities)

-- |
instance GetEtlFragment Qualities QualKey (SearchFragment QualValues 'ETL) where
  getValues qualities k =
    let key = mkQualKey k
    in  coerce . (key, ) <$> Qualities.lookup qualities key

-- |
--
instance GetEtlFragment Components CompKey (SearchFragment CompValues 'ETL) where
  getValues components k =
    let key = mkCompKey k
    in  coerce . (key, ) <$> Components.lookup components key


instance IsSubset SubsetCompMixReq  Measurements MeaKey  Components
instance IsSubset ComponentMixInput Measurements MeaKey  Components
instance IsSubset SubsetCompReq     Components   CompKey (SearchFragment CompValues 'ETL)
instance IsSubset ComponentReqInput Components   CompKey (SearchFragment CompValues 'ETL)

instance IsSubset QualityMixInput   Subject      SubKey  Qualities
instance IsSubset SubsetQualReq     Subject      SubKey  Qualities
instance IsSubset QualityReqInput   Qualities    QualKey (SearchFragment QualValues 'ETL)

---------------------------------------------------------------------------------
  --
