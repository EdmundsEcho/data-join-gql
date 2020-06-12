{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module     : Model.ETL.Span
-- Description: Component for representing Time
--
-- This module is a separate version of the 'Model.ETL.Components' module.
-- It relates to 'Model.ETL.Components' in that it is a way to slice a
-- 'Model.ObsETL.Measurements' into parts. It is different in that it hosts
-- the unique qualities of time.
--
-- It defines the structure required both for the ETL Observation and
-- the requested version of time ('FilterRange'). In the 'Api.GQL.Matrix' it has
-- unique processing features:
--
-- > Span -> [FilterRange]
--
module Model.ETL.Span
  ( Range(..)
  , FilterRange(FilterRange) -- pattern synonym
  , mkFilterRange            -- smart constructor
  , filterStart              -- record
  , filterEnd                -- record
  , overlap                  -- typeclass version of intersection
  , filterSize
  , fromListEtl              -- utilized by Fragment
  , intersection             -- utilized by Search

  -- ** Span related computations
  , Span(Span, span)
  , mkSpan                  -- unsafe constructor
  , mkSpanM                 -- constructor :: m Span

  -- ** export for testing
  , incLen                  -- for testing
  , delayStart              -- for testing
  , disjoint
  , juxta
  , continuous
  , subset
  , union
  , unions
  , combine
  , spanEnd
  , spanLength
  , rangeEnd
  , mkRange
  , isExp                   -- calls isRed :: TagRedExp -> Bool
  , intersect               -- utilized by Search
  )
  where
-------------------------------------------------------------------------------
import           Protolude              hiding (compare, from, get, toList)
-------------------------------------------------------------------------------
import qualified Data.Set               as Set (fromList, toList)
-------------------------------------------------------------------------------
import           Control.Exception.Safe
import           ObsExceptions
-------------------------------------------------------------------------------
import           Data.Aeson             (ToJSON)
-------------------------------------------------------------------------------
import           Model.ETL.TagRedExp
-------------------------------------------------------------------------------

-- ** Set Span construction
-- |
-- ETL constructor for span to ensure the `Set`-like qualities enjoyed by
-- 'Model.ETL.FieldValues'.  The return values is a consolidated, non-redundant
-- representation of 'Model.ETL.Span' values.
--
fromListEtl :: [Span] -> Set Span
fromListEtl = Set.fromList . unions

-- * Request
-- |
-- A set-level concept that finds overlap in individual span values.
-- Calls 'Disjoint.overlap for each span in the search.
--
intersection :: Set Span -> Set Span -> Set Span
intersection search spans = Set.fromList $
  intersect (Set.toList search) (Set.toList spans)

-- * Instantiation
--
-- ** Individual span values
--
-- *** ETL Constructor
-- |
-- Throws an exception in the event is instantiated with a non-positive
-- rangeLength value.  TODO: In the future it may make sense to enable
-- length of non-zero value.
--
mkSpanM :: (MonadThrow m) => (Range -> TagRedExp Range) -> Int -> Int -> m Span
mkSpanM expRedTag rangeStart rangeLength
  | rangeLength < 1 = throw . ValueException $
      Just ("\nCannot instantiate Span with a non-positive range length: "
           <> show rangeLength)
  | otherwise = pure $ Span_ { span_ = expRedTag Range {..} }

-- *** Request Constructor
-- |
--
mkSpan :: (Range -> TagRedExp Range) -> Int -> Int -> Span
mkSpan expRedTag rangeStart rangeLength =
  Span_ { span_ = expRedTag Range { .. } }


-- ** Typeclasses - properties for Span and Range
-- |
-- Describes seperate and adjacent time periods
--
class (Monoid a, Ord a) => Disjoint a where
  disjoint :: a -> a -> Bool
  juxta    :: a -> a -> Bool  -- ^ used to consolidate etl span values
  subset   :: a -> a -> Bool
  overlap  :: a -> a -> a
  combine  :: a -> a -> a

  continuous :: a -> a -> Bool
  continuous v1 v2 = overlap v1 v2 /= mempty

  -- |
  -- not commutative b/c Red <> Exp is different than Exp <> Red
  -- this is not a combination of two lists, but rather to build a non-redundant
  -- list of time periods.
  unions :: [a] -> [a]
  unions items =
    let start = filter (/= mempty) items
        result = foldr union [] . sortOn Down $ start
        changed = length result /= length start
     in
        if changed then unions result else result

  union :: a -> [a] -> [a]
  union new items =
    -- try to find an item that combines with the new entry
    case find (tryCombine new) items of
       Nothing   -> new : items  -- add disjoint/unique data
       Just item -> (new <> item) : filter (/= item) items
    where
       -- is new a subset of new + try, if so, keep combo, else keep try
       tryCombine new' item' = new' `subset` ((new' <> item') <> item')

  -- | not commutative b/c Exp overlap Red /= Red overlap Exp
  intersect :: [a] -> [a] -> [a]
  intersect get from = mapMaybe (`getOverlap` sort from) get
    where getOverlap :: a -> [a] -> Maybe a
          getOverlap get' from' =
            let result = mconcat (overlap get' <$> from')
             in if result == mempty then Nothing else Just result


-- ** Span
-- |
--   [@Exp@]: A series of values within the expression, **Expressed**.
--   [@Red@]: A summary version of the computation, **Reduced**.
--
newtype Span = Span_ { span_ :: TagRedExp Range }
  deriving (Show, Generic)

instance ToJSON Span

isExp :: Span -> Bool
isExp (Span tag) = not (isRed tag)

-- |
-- Pattern match capacity for otherwise private constructor
--
-- /Note/ Unidirectional.
--
pattern Span :: TagRedExp Range -> Span
pattern Span { span } <- Span_ { span_ = span }
{-# COMPLETE Span :: Span  #-}

-- |
-- Construct to host start and length the measurement value.
-- /Note/: Range only has meaning inside a Span construct.  Use FilterRange
-- to host information outside of the Span context.
--
data Range = Range
  { rangeStart  :: !Int
  , rangeLength :: !Int
  } deriving (Show, Generic)

instance ToJSON Range

---------------------------------------------------------------------------------
-- ** Range properties
-- |
-- The start value has a zero index.  The min length is 1. A range with a start
-- and stop of 0 has length of one. So, start and stop are /inclusive/ at both
-- ends.
--
rangeEnd :: Range -> End
rangeEnd = compute <$> rangeStart <*> rangeLength
  where compute s l = s + l - 1

mkRange :: Start -> Len -> Range
mkRange = Range

-- |
-- All with length of zero are equivalent.
--
instance Eq Range where
  Range _ 0 == Range _ 0 = True
  Range s1 l1 == Range s2 l2 = s1 == s2 && l1 == l2

-- |
-- Outside of the @TagRedExp@ context,
-- Range is a subset of another when...
--
instance Ord Range where
  ra1@(Range s1 l1) <= ra2@(Range s2 l2)

    -- identity values
    | rangeLength ra1 == 0 = True
    | rangeLength ra2 == 0 = False

    -- No overlap; earlier in time < later
    | ra1 `disjoint` ra2 = s1 <= s2 && rangeEnd ra1 <= rangeEnd ra2

    -- Subset
    | s1 >= s2 && rangeEnd ra1 <= rangeEnd ra2 = True
    | s2 >= s1 && rangeEnd ra2 <= rangeEnd ra1 = False

    -- Overlap relation;
    -- arbitrary bias for start, then length (may change to end)
    | s1 < s2 = True
    | s1 > s2 = False
    | s1 == s2 = l1 < l2

    | otherwise = panic $ "range sort is flawed\n"
                        <> show ra1 <> show ra2

instance Disjoint Range where
  -- commutative (range only)
  disjoint ra1 ra2
    | ra1 `subset` ra2 || ra2 `subset` ra1 = False
    | ra1 `juxta` ra2       = True
    | rangeLength ra1 == 0   = False   -- empty set is not disjoint
    | rangeLength ra2 == 0   = False
    | rangeStart ra1 < rangeStart ra2 = rangeEnd ra1 < rangeStart ra2
    | rangeStart ra1 > rangeStart ra2 = rangeEnd ra2 < rangeStart ra1
    | otherwise = False

  -- commutative
  -- Note: Empty bias towards juxta AND subset. If want to be consistent
  -- may want to have Empty be (not juxta).
  juxta ra1 ra2
    | rangeLength ra1 == 0 = True  -- empty, bias to consume mempty
    | rangeLength ra2 == 0 = True
    | ra1 == ra2 = False          -- 3rd in line so empty == empty = True
    | otherwise =
        rangeEnd ra1 + 1 == rangeStart ra2 ||
        rangeEnd ra2 + 1 == rangeStart ra1

  -- not commutative
  subset ra1@(Range s1 _) ra2@(Range s2 _)
    | ra1 == ra2 = True
    | rangeLength ra1 == 0 = True
    | rangeLength ra2 == 0 = False
    | otherwise = s1 >= s2 && rangeEnd ra1 <= rangeEnd ra2

  overlap ra1 ra2
    | juxta ra1 ra2 || disjoint ra1 ra2 = mempty
    | ra1 `subset` ra2 = ra1
    | ra2 `subset` ra1 = ra2
    | otherwise =
          let interStart'  = maximum [rangeStart ra1, rangeStart ra2]
              interEnd'    = minimum [rangeEnd ra1, rangeEnd ra2]
              interLength' = interEnd' - interStart' + 1
           in Range { rangeStart = interStart'
                    , rangeLength = interLength' }

  combine ra1 ra2
    | ra1 `juxta` ra2 = go ra1 ra2
    | ra1 `disjoint` ra2 = mempty
    | ra1 == mempty = ra2
    | ra2 == mempty = ra1
    | otherwise = go ra1 ra2

      where
        go :: Range -> Range -> Range
        go (Range _ 0) ra = ra
        go ra (Range _ 0) = ra
        go r1@(Range s1 _) r2@(Range s2 _) =

          let unionStart  = minimum [s1, s2]
              unionEnd    = maximum [rangeEnd r1, rangeEnd r2]
              unionLength = unionEnd - unionStart + 1

           in Range { rangeStart  = unionStart
                    , rangeLength = unionLength
                    }

-- |
-- Returns empty when disjoint
--
instance Semigroup Range where (<>) = combine

instance Monoid Range where
  mempty = mkRange 0 0
  mappend = (<>)

---------------------------------------------------------------------------------
-- ** Span properties
-- |
instance Eq Span where
  (==) (Span spa1) (Span spa2)
    | rangeLength (unTag spa1) == 0 = rangeLength (unTag spa2) == 0
    | otherwise =
        case (spa1, spa2) of
           ((Exp ra1),          (Exp ra2))          -> ra1 == ra2
           ((Red ra1),          (Red ra2))          -> ra1 == ra2
           ((Exp (Range s1 1)), (Red (Range s2 1))) -> s1  == s2
           (_,_)                                    -> False

-- ** Ord - the sorting engine
-- |
-- Used to sequence how to reduce a list of items to the most succinct version.
--
--
instance Ord Span where
  (<=) spa1@(Span t1) spa2@(Span t2)

    -- neutral value is the smallest
    | rangeLength (unTag t1) == 0 = True
    | rangeLength (unTag t2) == 0 = False

    -- Eq Spans (could be a problem with mempty)
    | spa1 == spa2 = True

    -- Pair of Red || Exp => Range
    | matchingTags spa1 spa2 = unTag t1 <= unTag t2

    -- Mix of Red and Exp with Eq Ranges => Exp < Red
    -- ... process Red following Exp
    | not (matchingTags spa1 spa2) = isExp spa1

    | otherwise = panic $ "span sort is flawed\n"
                        <> show spa1 <> show spa2

-- local helper
matchingTags :: Span -> Span -> Bool
matchingTags spa1 spa2 = (isExp spa1 && isExp spa2) ||
                         not (isExp spa1) && not (isExp spa2)

-- |
--
instance Disjoint Span where
  -- commutative
  disjoint spa1 spa2
    | spa1 `juxta` spa2 = True
    | spa1 `subset` spa2 || spa2 `subset` spa1 = False
    | otherwise = compare' spa1 spa2
      where
         compare' (Span (Exp ra1)) (Span (Exp ra2)) = ra1 `disjoint` ra2
         compare' _ _                               = True

  -- commutative
  juxta spa1 spa2
    | spa1 == spa2 = False
    | otherwise = compare' spa1 spa2
      where
         compare' (Span (Exp ra1)) (Span (Exp ra2)) = ra1 `juxta` ra2
         compare' (Span (Red ra1@(Range _ 1))) (Span (Exp ra2)) = ra1 `juxta` ra2
         compare' (Span (Exp ra1)) (Span (Red ra2@(Range _ 1))) = ra2 `juxta` ra1
         compare' (Span (Red ra1@(Range _ 1)))
                  (Span (Red ra2@(Range _ 1))) = ra1 `juxta` ra2
         compare' _ _ = False

  -- not commutative
  subset spa1@(Span t1) spa2@(Span t2)
    | spa1 == spa2 = True
    | rangeLength (unTag t1) == 0 = True
    | rangeLength (unTag t2) == 0 = False
    | otherwise = compare spa1 spa2
      where
         -- | left Exp data can be found in right Exp data when...
         compare (Span (Exp ra1)) (Span (Exp ra2)) = ra1 `subset` ra2
         -- | left Red data can be found in right Exp data when...
         compare (Span (Red ra1)) (Span (Exp ra2)) = ra1 `subset` ra2
         -- | left Exp cannot otherwise be satisfied with Red data on the right
         compare _ _                               = False

  -- not commutative
  overlap s1 s2
    | s1 `subset` s2 = s1  -- not commutative
    | otherwise = s1 `intersect` s2  -- commutative, some portion of s1
      where
        -- overlap between a pair of Exp...
        Span(Exp r1) `intersect` Span(Exp r2) = Span_(Exp (overlap r1 r2))
        -- no overlap between a pair of Red or mixed not already captured...
        _ `intersect` _ = mempty

  combine s1' s2'
    | s1' `juxta` s2' = go s1' s2'
    | s1' `disjoint` s2' = mempty  -- Exp `subset` Red must be screened
    | s1' == mempty = s2'
    | s2' == mempty = s1'
    | otherwise = go s1' s2'
      where
         go (Span (Exp s1))
            (Span (Exp s2))             = Span_ . Exp $ s1 <> s2

         go (Span (Red s1@(Range _ 1)))
            (Span (Exp s2))             = Span_ . Exp $ s1 <> s2

         go (Span (Exp s1))
            (Span (Red s2@(Range _ 1))) = Span_ . Exp $ s1 <> s2

         -- Exp year <> Red year = Exp year
         -- Exp q1 <> Red year = mempty
         go s1@(Span (Exp r1)) s2@(Span (Red r2))
           | s2 `subset` s1 = Span_ . Exp $ r1 <> r2
           | otherwise = mempty

         -- Red year <> Exp year = Exp year ... generally RHS `subset` LHS
         -- Exp q1 <> Red year = mempty
         go s1@(Span (Red r1)) s2@(Span (Exp r2))
           | s1 `subset` s2 = Span_ . Exp $ r1 <> r2
           | otherwise = mempty

         go (Span (Red s1@(Range _ 1)))
            (Span (Red s2@(Range _ 1))) = Span_ . Exp $ s1 <> s2

         go (Span (Red _))
            (Span (Red _)) = mempty

         go s1 s2 = panic $ "instance of Semigroup should be unreachable\n"
                            <> show s1 <> show s2

-- |
-- A consolidate, joining of span values
--
instance Semigroup Span where (<>) = combine

instance Monoid Span where
  mempty = Span_ $ Exp mempty
  mappend = (<>)



-- |
-- For testing
incLen :: Span -> Span
incLen sp =
  let Span { span = span' } = sp
   in case span' of
      Exp Range {..} -> mkSpan Exp rangeStart (rangeLength + 1)
      Red Range {..} -> mkSpan Red rangeStart (rangeLength + 1)

-- |
-- For testing
delayStart :: Span -> Span
delayStart sp =
  let Span { span = span' } = sp
   in case span' of
      Exp Range {..} -> mkSpan Exp (rangeStart + 1) rangeLength
      Red Range {..} -> mkSpan Red (rangeStart + 1) rangeLength


-- |
-- Support function to access Range values
-- >>> spanEnd Range { rangeStart = 0, rangeLength = 1 }
-- 0
--
spanEnd :: Range -> Int
spanEnd (Range s l) = s + l - 1

-- |
-- Support function to derive length
-- /Note/ Disjoint data when the value is negative.
--
-- >>> spanLength 0 0
-- 1
--
spanLength :: Start -> End -> Int
spanLength start end = end - start + 1

-- |
-- Structure that expresses the inclusive min and max values of a filter
-- value.  e.g., FilterRange 0 3 = Where timeSpan BETWEEN 0 AND 3 (inclusive)
--
-- > Span -> FilterRange
--
-- > Red Range 0 6 = [FilterRange 0 5]
-- > Red Range 2 3 = [FilterRange 2 4]
-- > Exp Range 0 6 = [FilterRange 0 0, FilterRange 1 1, FilterRange 2 2]
--
data FilterRange = FilterRange_ !Start !End
  deriving (Show, Eq, Ord, Generic)

instance ToJSON FilterRange

-- |
-- Pattern match for FilterRange
--
pattern FilterRange :: Start -> End -> FilterRange
pattern FilterRange { filterStart, filterEnd } <- FilterRange_ filterStart filterEnd
{-# COMPLETE FilterRange :: FilterRange #-}

-- |
-- Utilized by Matrix to interpret the time-span related request.
--
mkFilterRange :: Span -> [FilterRange]
mkFilterRange (Span (Red r)) = [FilterRange_ (rangeStart r) (spanEnd r)]
mkFilterRange (Span (Exp r)) = expand
  where
    expand = (\s -> FilterRange_ s s) <$> [(rangeStart r)..(spanEnd r)]

-- |
-- Utilized by Models.Expression
filterSize :: FilterRange -> Size
filterSize (FilterRange s e) = e - s + 1

-- Private
type Start = Int -- inclusive
type Len   = Int -- useful to validate
type End   = Int -- inclusive
type Size  = Len

---------------------------------------------------------------------------------
