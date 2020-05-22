{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE RecordWildCards #-}
-- |
-- Module     : Model.ETL.Span
-- Description: Component for representing Time
--
-- This module is a separate version of the 'Components'. It relates to
-- 'Components' in that it is a way to slice a 'Measurements' into parts.
--
-- It defines the structure required both for the ETL Observation and
-- the requested version of time ('FilterRange'). In the 'Matrix' it has
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
  , showFilterRange
  , subset
  , intersection
  , filterSize

  -- ** Span related computations
  , Span(Span, span)
  , mkSpan                  -- constructor that panics
  , mkSpanM                 -- constructor :: m Span
  , consolidate             -- eliminates and combines redundant entries
  , incLen                  -- for testing
  , delayStart              -- for testing

  -- ** Other reporting capacity (TODO: move)
  , EitherM (..)
  , fromEitherM
  , toSet
  , request

  -- ** export for testing
  , interStart
  , interEnd
  , interLength
  , getRange
  , disjoint
  , spanEnd
  , spanLength
  , union
  , juxta
  , rangeEnd
  , mkRange

  )
  where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Data.Maybe             (fromJust)
-------------------------------------------------------------------------------
import           Control.Exception.Safe
import           ObsExceptions
-------------------------------------------------------------------------------
import           Data.Aeson             (ToJSON)
import           Data.Coerce
import qualified Data.Set               as Set
-------------------------------------------------------------------------------
import           Model.ETL.TagRedExp
-------------------------------------------------------------------------------

-- ** EitherM - TODO: Move
type Message = Text

data EitherM a
  = RightM a
  | LeftM  ([Message], Maybe a)
  deriving (Show, Eq, Functor, Monad)

fromEitherM :: EitherM a -> Maybe a
fromEitherM (RightM a)           = Just a
fromEitherM (LeftM (_, Just a))  = Just a
fromEitherM (LeftM (_, Nothing)) = Nothing

instance Applicative EitherM where
  pure = RightM
  RightM f <*> RightM a = RightM $ f a
  LeftM (mss, Just f)  <*> RightM a = LeftM (mss, Just $ f a)
  LeftM (mss, Nothing) <*> RightM _ = LeftM (mss, Nothing)
  RightM f <*> LeftM (mss, Just a) = LeftM (mss, Just $ f a)
  RightM _ <*> LeftM (mss, Nothing) = LeftM (mss, Nothing)
  LeftM (mss1, Just f) <*> LeftM (mss2, Just a) = LeftM (mss1 <> mss2, Just $ f a)
  LeftM (mss1, Just _) <*> LeftM (mss2, Nothing) = LeftM (mss1 <> mss2, Nothing)
  LeftM (mss1, Nothing) <*> LeftM (mss2, Just _) = LeftM (mss1 <> mss2, Nothing)
  LeftM (mss1, Nothing) <*> LeftM (mss2, Nothing) = LeftM (mss1 <> mss2, Nothing)

-- ** Typeclasses - properties for Span and Range
-- |
-- Describes seperate and adjacent time periods
--
class Ord a => Disjoint a where
  disjoint :: a -> a -> Bool
  juxta    :: a -> a -> Bool

-- |
-- Returns the first Span if disjoint
--
class Disjoint a => Union a where
  union :: a -> a -> a


-- ** Span
-- |
--   [@Exp@]: A series of values within the expression, **Expressed**.
--   [@Red@]: A summary version of the computation, **Reduced**.
--
newtype Span = Span_ { span_ :: TagRedExp Range }
  deriving (Show, Generic)

instance ToJSON Span

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
  ra1@(Range s1 _) <= ra2@(Range s2 _)
    | ra1 == ra2 = True
    | rangeLength ra1 == 0 = True
    | rangeLength ra2 == 0 = False
    | otherwise = s1 >= s2 && rangeEnd ra1 <= rangeEnd ra2


instance Disjoint Range where
  disjoint ra1 ra2
    | ra1 <= ra2 || ra2 <= ra1 = False
    | rangeLength ra1 == 0 = False   -- bias to merge to consume mempty
    | rangeLength ra2 == 0 = False
    | rangeStart ra1 /= rangeStart ra2 = rangeEnd ra1 < rangeStart ra2
    | otherwise = False

  juxta ra1 ra2
    | ra1 == ra2 = False
    | rangeLength ra1 == 0 = True  -- bias to merge to consume mempty
    | rangeLength ra2 == 0 = True
    | otherwise =
        rangeEnd ra1 + 1 == rangeStart ra2 ||
        rangeEnd ra2 + 1 == rangeStart ra1

instance Semigroup Range where
  ra1 <> ra2
    | ra1 `juxta` ra2 = combine ra1 ra2
    | ra1 `disjoint` ra2 = ra1
    | rangeLength ra1 == 0 = ra2
    | ra1 <= ra2 = ra2
    | ra2 <= ra1 = ra1
    | otherwise = combine ra1 ra2

      where
        combine (Range _ 0) ra = ra
        combine ra (Range _ 0) = ra
        combine ra1@(Range s1 _) ra2@(Range s2 _) =

          let unionStart  = minimum [s1, s2]
              unionEnd    = maximum [rangeEnd ra1, rangeEnd ra2]
              unionLength = unionEnd - unionStart + 1

           in Range { rangeStart  = unionStart
                    , rangeLength = unionLength
                    }

instance Monoid Range where
  mempty = mkRange 0 0
  mappend = (<>)

instance Union Range where
  union = (<>)

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

-- ** Ord as a proxy for subset
-- |
--
-- To serve a request, we need to determine the @intersection@.
--
-- Ord is used to test for a Subset relation between the Request Span
-- on the left, and the ETL Span on the right.
--
-- The objective: Does the ETL data contain what is being requested?
--
--  * When ETL Red 0 12, we don't have the data if don't match start and
--    length of the range
--
--  * When ETL Exp 0 12, we we can accomodate both Red and Exp within 0 12.
--
-- /Note/ non-commutative
--
instance Ord Span where
  (<=) spa1 spa2
    | spa1 == spa2 = True
    | otherwise = compare spa1 spa2
      where
         -- | left Exp data can be found in right Exp data when...
         compare (Span (Exp ra1)) (Span (Exp ra2)) = ra1 <= ra2
         -- | left Red data can be found in right Exp data when...
         compare (Span (Red ra1)) (Span (Exp ra2)) = ra1 <= ra2
         -- | left Exp cannot otherwise be satisfied with Red data on the right
         compare _ _                               = False

-- |
-- Ignores the Exp/Red tag
--
-- instance Disjoint Span where
   -- disjoint (Span ra1) (Span ra2) = interLength (unTag ra1) (unTag ra2) < 1
   -- juxta = undefined
   --
instance Disjoint Span where
  disjoint spa1 spa2
    | spa1 <= spa2 || spa2 <= spa1 = False
    | otherwise = compare spa1 spa2
      where
         compare (Span (Red _)) (Span (Red _))     = True
         compare (Span (Exp _)) (Span (Red _))     = True
         compare (Span (Exp ra1)) (Span (Exp ra2)) = ra1 `disjoint` ra2
         compare (Span (Red ra1)) (Span (Exp ra2)) = ra1 `disjoint` ra2

  juxta spa1 spa2
    | spa1 == spa2 = False
    | otherwise = compare spa1 spa2
      where
         compare (Span (Exp ra1)) (Span (Exp ra2)) = ra1 `juxta` ra2
         compare (Span (Red ra1@(Range _ 1))) (Span (Exp ra2)) = ra1 `juxta` ra2
         compare (Span (Exp ra1)) (Span (Red ra2@(Range _ 1))) = ra2 `juxta` ra1
         compare (Span (Red ra1@(Range _ 1)))
                 (Span (Red ra2@(Range _ 1))) = ra1 `juxta` ra2
         compare _ _ = False


instance Semigroup Span where
  spa1 <> spa2
    | spa1 `juxta` spa2    = combine spa1 spa2
    | spa1 `disjoint` spa2 = spa1
    | spa1 <= spa2          = spa2
    | spa2 <= spa1          = spa1
    | otherwise            = combine spa1 spa2
      where
         combine (Span (Exp ra1))
                 (Span (Exp ra2)) = Span_ . Exp $ ra1 <> ra2

         combine (Span (Red ra1@(Range _ 1)))
                 (Span (Exp ra2))             = Span_ . Exp $ ra1 <> ra2

         combine (Span (Exp ra1))
                 (Span (Red ra2@(Range _ 1))) = Span_ . Exp $ ra1 <> ra2

         combine (Span (Red ra1@(Range _ 1)))
                 (Span (Red ra2@(Range _ 1))) = Span_ . Exp $ ra1 <> ra2

         combine _ _ = spa1

instance Monoid Span where
  mempty = mkSpan Exp 0 0
  mappend = (<>)

instance Union Span where
  union = (<>)

-- |
-- Predicate that returns `True` when a subset relation exists between Spans.
--
-- Order of the arguments matter. This constraint is solved elsewhere.
--
-- Sequence: search -> etl -> subset
--
subset :: Span -> Span -> Bool
subset = (<=)
-- |
-- Pattern match capacity for otherwise private constructor
--
-- /Note/ Unidirectional.
--
pattern Span :: TagRedExp Range -> Span
pattern Span { span } <- Span_ { span_ = span }
{-# COMPLETE Span :: Span  #-}

-- |
-- ETL Constructor
--
-- Throws an exception in the event is instantiated with a non-positive
-- rangeLength value.
--
-- In the future it may make sense to enable length of non-zero value.
--
mkSpanM :: (MonadThrow m) => (Range -> TagRedExp Range) -> Int -> Int -> m Span
mkSpanM expRedTag rangeStart rangeLength
  | rangeLength < 1 = throw . ValueException $
      Just ("\nCannot instantiate Span with a non-positive range length: "
           <> show rangeLength)
  | otherwise = pure $ Span_ { span_ = expRedTag Range {..} }


-- |
-- Request constructor
--
mkSpan :: (Range -> TagRedExp Range) -> Int -> Int -> Span
mkSpan expRedTag rangeStart rangeLength =
  Span_ { span_ = expRedTag Range { .. } }
  -- rangeLength < 1 = panic "Cannot instantiate Span with a non-positive range length"
  -- otherwise       = Span_ { span_ = expRedTag Range { .. } }

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

consolidate :: Set Span -> Set Span
consolidate spans = undefined
-- Set.fromList
--                  $ foldr union [Span_ (Exp (Range 0 0))] (Set.toList spans)

request :: Set Span -> Set Span -> Set Span
request = undefined

-- |
-- Extract intersection result
toSet :: EitherM Span -> Set Span
toSet (RightM v)           = Set.fromList [v]
toSet (LeftM (_, Just v))  = Set.fromList [v]
toSet (LeftM (_, Nothing)) = Set.fromList []

-- |
-- Intersection
--
-- Sequence: search -> etl -> subset
--
intersection :: Span -> Span -> EitherM Span
intersection s1 s2
  | s1 `subset` s2   = RightM s1
  | s1 `disjoint` s2 = LeftM (["Error: Disjoint request"], Nothing)
  | s1 `juxta` s2    = LeftM (["Error: Juxta/disjoint request"], Nothing)

  | s2 `subset` s1 && not (isRed $ getSpan s2)
                     = LeftM (["Warning: Truncated request"], Just s2)

  | otherwise = case (s1,s2) of
       -- left Exp and right Exp data intersection (~minimax)
       (Span (Exp ra1), Span (Exp ra2)) ->
         LeftM (["Warning: Truncated span series"], Just $
            Span_ (Exp Range { rangeStart  = interStart ra1 ra2
                             , rangeLength = interLength ra1 ra2 }) )

       -- left Red and right Exp data intersection (~minimax)
       (Span (Red ra1), Span (Exp ra2)) ->
         LeftM (["Warning: Truncated summary span value"], Just $
            Span_ (Red Range { rangeStart  = interStart ra1 ra2
                             , rangeLength = interLength ra1 ra2 }) )

       (Span _, Span _) -> LeftM (["Error: No series of span"], Nothing)

-- |
-- Support for the intersection computation
--
interStart :: Range -> Range -> Start
interStart ra1 ra2 = maximum [spanStart ra1, spanStart ra2]

interLength :: Range -> Range -> Int
interLength ra1 ra2 = spanLength (interStart ra1 ra2) (interEnd ra1 ra2)

interEnd :: Range -> Range -> End
interEnd ra1 ra2 = minimum [spanEnd ra1, spanEnd ra2]

getRange :: Span -> Range
getRange (Span tagRange) = unTag tagRange

getSpan :: Span -> TagRedExp Range
getSpan = coerce


-- |
spanDes :: Text
spanDes = "A describes the time period over which the measurement value is relevant.\n\
          \ Each Span has a Range object that describes the start and length of the time.\n\
          \ Length is the number of units of time used to describe the data. Finally,\n\
          \ a measurement sample can be either a summary over time or taken on regular\n\
          \ intervals.  Span :: RED summary | EXP repeated measure."
{-# DEPRECATED spanDes "" #-}

-- |
-- Support function to access Range values
-- >>> spanEnd Range { rangeStart = 0, rangeLength = 1 }
-- 0
--
spanEnd :: Range -> Int
spanEnd (Range s l) = s + l - 1

-- |
-- Support function to access Range values
--
spanStart :: Range -> Int
spanStart (Range s _) = s

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
-- Structure that clearly defines the inclusive min and max values of a filter
-- value.  e.g., FilterRange 0 3 = Where timeSpan BETWEEN 0 AND 3 (inclusive)
-- Span -> FilterRange
-- Red Range 0 6 = [FilterRange 0 5]
-- Red Range 2 3 = [FilterRange 2 4]
-- Exp Range 0 6 = [FilterRange 0 0, FilterRange 1 1, FilterRange 2 2]
--
data FilterRange = FilterRange_ !Start !End
  deriving (Show, Eq, Ord, Generic)

-- |
-- Pattern match for FilterRange
--
pattern FilterRange :: Start -> End -> FilterRange
pattern FilterRange { filterStart, filterEnd } <- FilterRange_ filterStart filterEnd
{-# COMPLETE FilterRange :: FilterRange #-}

-- | FilterRange constructor
mkFilterRange :: Span -> [FilterRange]
mkFilterRange (Span (Red r)) = [FilterRange_ (rangeStart r) (spanEnd r)]
mkFilterRange (Span (Exp r)) = expand r
  where
    expand r' = go <$> [(rangeStart r')..(spanEnd r')]
    go s = FilterRange_ s s

-- | Utilized by Models.Expression
showFilterRange :: FilterRange -> Text
showFilterRange fr = show (filterStart fr) <> "_" <> show (filterEnd fr)

-- | Utilized by Models.Expression
filterSize :: FilterRange -> Size
filterSize (FilterRange s e) = e - s + 1

-- Private
type Start = Int -- inclusive
type Len   = Int -- useful to validate
type End   = Int -- inclusive
type Size  = Len

---------------------------------------------------------------------------------
