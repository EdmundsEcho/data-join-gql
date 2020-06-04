{-# OPTIONS_HADDOCK prune #-}

{-|
   Expression captures all the information required to lookup the values
   required for the Matrix.

   There are two types of fields:

    (1) Fields in the Observation object

         * Requested QualityMix ~ Wide view (WV)
         * Requested ComponentMix ~ Long view (LV)

     2. Fields for the Matrix, WV

   This module is dedicated to capturing and enabling the following:

   @ ComponentMix -> [Matrix fields that lookup data from Long View sources] @

     ... where a source is associated with a Measurement Type

   @ Qualities    -> [Matrix fields that lookup data from Wide View source] @

     ... where the source is associated with the Subject

   @
   Relation :: FieldName   Relation   Values
                     LHS   IN | EQ    RHS

   Observation field -> Matrix field
   FieldName         -> filter: field to lookup (LHS)
   FieldValues       -> filter: search criteria (RHS)
   ComponentMix      -> matrix field name + filter
   Qualities         -> matrix field name + filter
   @

   == ComponentMix
   @

   Number of fields = product of every set size
   Number of filters for each field = number of components in the mix
   Record count = Set by the reduction Group By Subject ID
   UID = Subject ID

   -> matrix field names (a function of the filter)
   -> matrix filters, several for each field

   e.g.,
   payer  = { "Medicare", "Private" }
   rxType = { "Refills", "NRx" ]
   span   = { Exp (Range 1 12), Red (Range 3 10) }

   Number of fields = 2 x 2 x (12 + 1) = 52
   Note: span has 13 values in the set

   Number of filters for each field: 3

   ComponentMix -> filters :: [filter]
   filter = span (1,1)
   filter = payer  = "Medicare"
   filter = rxType = "Refills"

   filter = span (2,1)
   filter = payer  = "Medicare"
   filter = rxType = "Refills"
   ...

   filter = span (1,1)
   filter = payer  = "Private"
   filter = rxType = "Refills"
   ...

   filter = span (1,1)
   filter = payer  = "Medicare"
   filter = rxType = "NRx"
   ...

   filter = span (12,1)
   filter = payer  = "Private"
   filter = rxType = "NRx"
   @

   == Qualities
   @
   -> matrix field names (a function of the filter)
   -> matrix filters, several for each field

   Number of fields = Number of Qualities
   The one filter, number of criteria = Number of Qualities
   Record count = Determined by the filter
   UID = Subject ID

   Specialty = { "OBGYN", "ONC" }
   CalledOn  = { 0, 2 }

   filter =
     Specialty IN [ "OBGYN", "ONC" ]
     CalledOn  IN [ 0, 2 ]

   field names = Specialty, CalledOn
     ... criteria (LHS) of filters
   @
-}
module Model.Matrix.Expression
  ( module Model.Matrix.Expression
  , module Model.Matrix.Expression.FieldName
  , module Model.Matrix.Filter
  )
  where
---------------------------------------------------------------------------------
import           Protolude                         hiding (EQ, toList)
---------------------------------------------------------------------------------
import           Data.Aeson.Types
import           Data.Coerce
import           Data.Maybe                        (fromJust)
---------------------------------------------------------------------------------
import           Model.ETL.FieldValues
import           Model.ETL.ObsETL                  hiding (FieldName)
import           Model.ETL.TagRedExp               (TagRedExp)
import qualified Model.ETL.TagRedExp               as Tag
import           Model.ETL.Transformers            (fromReqComponents,
                                                    fromReqQualities)
import           Model.Matrix.Expression.FieldName
import           Model.Matrix.Filter
import           Model.Request                     (CompReqValues(..), ComponentMixes, QualityMix,
                                                    ReqComponents,
                                                    getReqQualityNames,
                                                    qualityMix, reqComponents,
                                                    reqQualities)
---------------------------------------------------------------------------------

{-| Exp defines how to retrieve the data from the ETL store.

   A unit is defined by the mechanics of a data pull to the @Matrix@.
   Start can be either Wide or Long view, both end as a Wide View data format.

   @
      Exp -> [Fields]  ... pulls from Wide-View data; so WV -> WV
      Exp -> Field     ... pulls from Long-View data; so LV -> WV
   @
-}
data Exp = Exp
  { source  :: !ETLTable
  , filter  :: !(Maybe Filter)    -- Nothing ~ no where entry
  , fields  :: ![FieldName]       -- subExp can generate several fields; not so for meaExp
  , reducer :: !(Maybe Reducer)   -- required for the GroupBy reduction
  } deriving (Show, Eq, Generic)

instance ToJSON Exp

{-|
  Construct the @Exp@ sourced by the requested Subject Qualities, returning
  the Matrix subExp value.

  The collection of @Relations@ that defines the filter:

   * The @LHS@ is a /product/ of quality names (field names)
   * The @RHS@ is ALL of the requested elements (field values)

-}
fromQualityMix :: QualityMix -> Exp
fromQualityMix mx = Exp SUB (mkFilter <$> quals) (fromJust fields) Nothing
  where
    quals = qualityMix mx
    mkFilter qs' = Filter $ fromReqQualities mkRel qs'
    mkRel k vs = Relation (mkLHS k) (RhsValues vs)
    fields = Just ["SUB"::Text] <|> getReqQualityNames <$> quals

-- TODO: Need to create a min Exp for QualityMix
-- qualityMix  :: !(Maybe ReqQualities)
-- reqQualities :: Map QualKey (Maybe QualValues)

{-| Construct the @Exp@ sourced by the requested CompMixes.
   Each @Measurement@ may have several @ComponentMixes@ values.
   The construct returns the Matrix meaExps value.

   The collection of @Relations@ that defines the filter:

    * The *LHS* is a /cartesian product/ of component names (field names)
    * The *RHS* is a single value from the requested elements

   /Note/: On the path to creating @[Exp]@ there is collection of @Relations@
   that are /related/, meaning they are generated from the same set, the same
   @Component@ in the @ComponentMixes@.

   TODO: Consider an explicit definition of rules for how to treat NULL values
   while joining the different @Exps@.

   The current default/starting points:

   @

   subExp Null -> Null (THIS IS A PROBLEM Null will never come up in a search)
   meaSub Null -> 0    (the neutral value for the SUM reducing operation)

   ComponentMix -> Related Relations -> Cartesian Relations -> Exp
   @
     (1) @ComponentMix -> Components@
      1. @Components   -> [[Relation]] :: [RelatedRelations]@
      1. @[[Relation]] -> [[Relation]] :: [Filter] ~ Filter :: CartesianRelations@
      1. @[Filter]     -> [Exp]@
-}
fromComponentMix :: (MeaType, ReqComponents) -> [Exp]
fromComponentMix mx = mkExp (etlTable mx) reducer (mkCartRelations (snd mx))

  where
    etlTable :: (MeaType, ReqComponents) -> MeaType
    etlTable = coerce . fst -- Key -> MeaType

    reducer :: Reducer
    reducer  = SUM

    mkCartRelations :: ReqComponents -> [Filter]
    mkCartRelations cs = undefined -- Filter <$> cartProd (fromReqComponents distSetNameOverValues cs)

    mkExp :: ETLTable -> Reducer -> [Filter] -> [Exp]
    mkExp tb red filters = fromFilter tb red <$> filters

    fromFilter :: ETLTable -> Reducer -> Filter -> Exp
    fromFilter tb' red' flt' = undefined -- Exp tb' flt' [fieldName flt'] (Just red')

-- * Long View -> Wide View transformation

-- | Distribute the Field Name over the @FieldValues@
--
-- e.g., @ payers {"CASH","PRIVATE"} -> payers "CASH", payers "PRIVATE" @
--
-- Each pair is a @Relation@. Number of @Relations@ generated = Number of Values
type RelatedCartesianFactors = [Relation]
-- |
-- TODO: These functions rely on access to the RHS value constructors.  These
-- constructors should be private.  Smart constructors that restrict the input
-- to FieldValues is required. Likely use PatternSynonym.
-- Update: FieldValues -> ReqCompValues, ExpComp computed as is; RedComp
-- computed using IN.
distSetNameOverValues :: Key -> CompReqValues -> RelatedCartesianFactors

distSetNameOverValues fldName (CompReqValues (Tag.Exp vs@(TxtSet _))) =
  mkRel <$> repeat fldName `zip` toList vs
    where mkRel (k,v) = Relation (mkLHS k) (RhsTxt v)

distSetNameOverValues fldName (CompReqValues (Tag.Exp (IntSet vs))) =
  undefined
  -- mkRel <$> repeat fldName `zip` toList (IntSet vs)
    -- where
      -- mkRel (k,v) = Relation (mkLHS k) (RhsInt v)

-- | Two tasks:
--   (1) Distribute the name over values
--    1. Transform @ Span Range -> SpanFilter @
distSetNameOverValues fldName (CompReqValues (Tag.Exp vs@(SpanSet _))) =
  mkRel <$> repeat fldName `zip` toListOfFilters vs
    where
      mkRel (k,v) = Relation (mkLHS k) (RhsSpan v)

      toListOfFilters :: FieldValues -> [FilterRange]
      toListOfFilters vs' = concat $ mkFilterRange <$> toList vs'

distSetNameOverValues _ (CompReqValues (Tag.Red _)) =
  panic "Match should not have reached here"

cartProd :: [[a]] -> [[a]]
cartProd [] = [[]]
cartProd (xs:xss) = [x:ys | x<- xs, ys <-yss]
                        where yss = cartProd xss

-- | Expression ~ 'From' clause in the 'SQL'
data ETLTable
  = SUB
  | MeaType Text deriving (Show, Eq, Generic)

instance FromJSON ETLTable
instance ToJSON ETLTable

-- | MeaType comes from MeaKey
type MeaType = ETLTable

-- | Smart constructor to limit the source of a MeaType
mkMeaType :: Key -> MeaType
mkMeaType (MeaKey k) = MeaType k
mkMeaType _ = panic "mkMeaType: Tried to make MeaType with the wrong Key value"

-- | Utilized by GraphQL to view Expression's source
showETLTable :: ETLTable -> Text
showETLTable SUB         = "SUB"
showETLTable (MeaType t) = t

-- | The GroupBy reduction has a Reducer (binary monoidal operation)
-- Used to reduce the many values -> one value for each SubjectID.
data Reducer = SUM | AVG
  deriving (Show, Eq, Generic)

instance FromJSON Reducer
instance ToJSON Reducer

-- | Used to generate Matrix Field name in the Exp from WV data
-- Quality filter
-- Location of FieldName = LHS of the Relations in the Filter
fieldNames :: Filter -> [FieldName]
fieldNames (Filter flt) = unLHS . lhs <$> flt

-- | Used to generate Matrix Field name in the Exp from LV data
-- Component filter
-- FieldName = Filter, a concatenation of the LHS:RHS in the Filter
fieldName :: Filter -> FieldName
fieldName = showFilter
