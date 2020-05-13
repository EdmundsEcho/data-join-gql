{-|
   Top-level ETL Model (where GQL is a view).  This object is instantiated
   using "Schemas.ETL.Types.ObsETLInput".
-}
module Model.ETL.ObsETL
  ( module Model.ETL.ObsETL
  -- * Re-exported types
  , module Model.ETL.ID       -- beginnings of a Node UID generator; now for root only.
  , module Model.ETL.Qualities
  , module Model.ETL.Key
  , module Model.ETL.Components
  , module Model.ETL.FieldValues
  , module Model.ETL.Fragment  -- type class
  , module Model.ETL.Span
  , module Model.ETL.TagRedExp
  )
  where
-------------------------------------------------------------------------------
import           Protolude
---------------------------------------------------------------------------------
import qualified Data.Map.Strict       as Map (fromList, lookup, null, size,
                                               union)
import           Data.Text             (append)
-------------------------------------------------------------------------------
import           Data.Aeson            (ToJSON)
---------------------------------------------------------------------------------
import           Lib.NodeManager
import           Model.ETL.Components  hiding (names)
import           Model.ETL.FieldValues
import           Model.ETL.Fragment
import           Model.ETL.ID
import           Model.ETL.Key
import           Model.ETL.Qualities
import           Model.ETL.Span
import           Model.ETL.TagRedExp
-------------------------------------------------------------------------------
-- = Observation model
-- |
data ObsETL = ObsETL
        { obsID           :: !ID            -- ^ Implemented, but not may not be useful
        , obsSubject      :: !Subject
        , obsMeasurements :: !Measurements
        } deriving (Show, Eq, Ord, Generic)

instance ToJSON ObsETL

-- |  documentation
obsDes :: Text
obsDes = "Describes the data available to construct\
        \ the matrix required for the analysis."


-- | Private smart constructor that utilizes an ID generator.
-- /Note/: The ID generator is implemented, but not exploited by the app.
mkObsETL :: Subject -> Measurements -> NodeManager ObsETL
mkObsETL s ms = do
  idx <- generateIdx   -- just give me state (moved to value channel)
  return $ ObsETL (mkID' idx) s ms -- return (a :: ObsETL)
    where
       mkID' :: Int32 -> ID
       mkID' n = NewID $
         ("OID"::Text) `append` show d3 `append` show d2 `append` show d1
           where { d3 = n`quot`100; d2 = n`quot`10; d1 = n`quot`1 }

-- == Subject
-- | The @Subject@ node is a record with
-- @ Key :: SubKey @ and
-- @ Value (child) :: Set of QualValues @
data Subject = Subject
        { subType      :: !Key
        , subQualities :: !Qualities
        } deriving (Show, Eq, Ord, Generic)

instance ToJSON Subject

instance GetEtlFragment Subject SubKey Qualities where
  getValues Subject { subQualities } _ = Just subQualities

-- | documentation
subDes :: Text
subDes = "Subject branch for which there is only one. The requested subset\
         \ determines the number of records included in the matrix."

-- == Measurements
-- | The @Measurements@ node is a @Map@ with
-- @ Key :: MeaKey @ and
-- @ Value (child) :: Map of Components @
newtype Measurements = Measurements
        { measurements  :: Map Key Components
        } deriving (Show, Eq, Ord, Generic)

instance ToJSON Measurements

instance Semigroup Measurements where
  (Measurements a) <> (Measurements b) = Measurements $ Map.union a b

instance Monoid Measurements where
  mempty = Measurements mempty
  Measurements a `mappend` Measurements b = Measurements $ Map.union a b

instance GetEtlFragment Measurements MeaKey Components where
  getValues Measurements { measurements } k
    = Map.lookup k measurements

instance Fragment Measurements where
  null (Measurements vs) = Map.null vs
  len  (Measurements vs) = Map.size vs

-- | As of yet, unused support function (perhaps for @Matrix@)
meaTypes :: Measurements -> [Text]
meaTypes = names . measurements

-- | documentation
measDes :: Text
measDes = "A Map collection of the available Measurements.\n\
          \ Key :: TypeKey that describes the Measurement Type\n\
          \ Value :: A Map to the Components of the Measurement."



-- | fromList constructors
measFromList :: Ord k => [(k,vs)] -> Map k vs
measFromList = mapFromList

compsFromList :: Ord k => [(k,vs)] -> Map k vs
compsFromList = mapFromList

qualsFromList :: Ord k => [(k,vs)] -> Map k vs
qualsFromList = mapFromList

mapFromList :: Ord k => [(k,vs)] -> Map k vs
mapFromList = Map.fromList
