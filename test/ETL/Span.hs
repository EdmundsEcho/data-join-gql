module ETL.Span
  where
-------------------------------------------------------------------------------
import           Data.Coerce
import           Protolude             hiding (from, get, toList)
-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import qualified Data.Set              as Set
import           Model.ETL.FieldValues (FieldValues (..), toCompValues)
import           Model.ETL.Fragment
import           Model.ETL.Span        hiding (intersection)
import qualified Model.ETL.Span        as Span (subset)
import           Model.ETL.TagRedExp
-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Span request dynamics"
  [ spec_spanSubset
  , spec_range
  , spec_spanEq
  , spec_spanOrd
  , spec_spanSemigroup
  , spec_construction
  , spec_subset
  , spec_request
  , spec_spanProps
  , spec_idempotentProp
  ]

-- |
-- Generators
--
-- Arbitrary values for QuickCheck
--
rangeGen :: Gen Range
rangeGen = do
    start <- elements [0..99]
    len   <- elements [0..99]
    pure $ Range start len

instance Arbitrary Range where
  arbitrary = rangeGen

tagRedExpGen :: Arbitrary a => Gen (TagRedExp a)
tagRedExpGen = do
  a <- arbitrary
  oneof [pure $ Exp a, pure $ Red a]

instance Arbitrary a => Arbitrary (TagRedExp a) where
  arbitrary = tagRedExpGen

spanGen :: Gen Span
spanGen = do
  start <- elements [0..99]
  len   <- elements [0..99]
  oneof [ pure $ mkSpan Exp start len
        , pure $ mkSpan Red start len ]

instance Arbitrary Span where
  arbitrary = spanGen

flexList :: Arbitrary a => Gen [a]
flexList = sized $ \n ->
  frequency
    [ (1, return [])
    , (n, (:) <$> arbitrary <*> flexList)
    ]

spanList = generate (resize 1000 flexList :: Gen [Span])

bothExp :: Span -> Span -> Bool
bothExp spa1 spa2 = isExp spa1 && isExp spa2

leftExp :: Span -> Span -> Bool
leftExp spa1 spa2 = isExp spa1 && not (isExp spa2)

juxtaRangeGen :: Gen (Range, Range)
juxtaRangeGen = do
  r1 <- rangeGen
  len <- elements [0..99]
  pure (r1, mkRange (rangeEnd r1 + 1) len)

subsetRangeGen :: Gen (Range, Range)
subsetRangeGen = do
  r1 <- rangeGen
  pushStart <- elements [0,1]
  retractEnd <- elements [0,1]
  let len = maximum [rangeLength r1 - retractEnd, 0]
  pure (r1, mkRange (rangeStart r1 + pushStart) len)

subsetSpanGen :: Gen (Span, Span)
subsetSpanGen = do
  r1 <- rangeGen
  pushStart <- elements [0,1]
  retractEnd <- elements [0,1]
  let len = maximum [rangeLength r1 - retractEnd, 0]
  let r2  = mkRange (rangeStart r1 + pushStart) len
  s1 <- oneof [ pure $ mkSpan Exp (rangeStart r1) (rangeLength r1)
              , pure $ mkSpan Red (rangeStart r1) (rangeLength r1) ]
  s2 <- oneof [ pure $ mkSpan Exp (rangeStart r2) (rangeLength r2)
              , pure $ mkSpan Red (rangeStart r2) (rangeLength r2) ]

  pure (s1, s2)

-- |
-- Range properties
--
prop_commutativeSemi :: Range -> Range -> Bool
prop_commutativeSemi v1 v2 = v1 <> v2 == v2 <> v1

prop_commutativeDisjoint :: Range -> Range -> Bool
prop_commutativeDisjoint v1 v2 = v1 `disjoint` v2 == v2 `disjoint` v1

prop_commutativeJuxta :: Range -> Range -> Bool
prop_commutativeJuxta v1 v2 = v1 `juxta` v2 == v2 `juxta` v1

prop_commutativeEq :: Range -> Range -> Bool
prop_commutativeEq v1 v2 = (v1 == v2) == (v2 == v1)

prop_overlapDisjoint :: Range -> Range -> Property
prop_overlapDisjoint get from =
  from `disjoint` get ==> overlap get from == mempty

prop_isJuxta :: Property
prop_isJuxta = forAll juxtaRangeGen $ \tup ->
  uncurry juxta tup

prop_overlapJuxta :: Property
prop_overlapJuxta = forAll juxtaRangeGen $ \tup ->
  uncurry overlap tup == mempty

prop_overlapSubset :: Property
prop_overlapSubset = forAll subsetRangeGen $ \tup ->
  uncurry subset tup ==> uncurry overlap tup == fst tup ||
  uncurry subset tup ==> uncurry overlap tup == snd tup

prop_overlapNotDisjoint :: Range -> Range -> Property
prop_overlapNotDisjoint v1 v2 =
  v1 /= mempty && v2 /= mempty ==>
     (v1 `overlap` v2 == mempty) == (v1 `disjoint` v2) &&
     (v1 `overlap` v2 /= mempty) == not (v1 `disjoint` v2)

-- |
-- Span properties
--
prop_commutativeSemi' :: Span -> Span -> Bool
prop_commutativeSemi' v1 v2 = v1 <> v2 == v2 <> v1

prop_commutativeDisjoint' :: Span -> Span -> Bool
prop_commutativeDisjoint' v1 v2 = v1 `disjoint` v2 == v2 `disjoint` v1

prop_commutativeJuxta' :: Span -> Span -> Bool
prop_commutativeJuxta' v1 v2 = v1 `juxta` v2 == v2 `juxta` v1

prop_commutativeEq' :: Span -> Span -> Bool
prop_commutativeEq' v1 v2 = (v1 == v2) == (v2 == v1)

prop_overlapSubset' :: Property
prop_overlapSubset' = forAll subsetSpanGen $ \tup ->
  uncurry subset tup ==> uncurry overlap tup == fst tup

prop_overlapSubsetLeftExp' :: Property
prop_overlapSubsetLeftExp' = forAll subsetSpanGen $ \tup ->
  uncurry (==) tup ==> uncurry overlap tup == snd tup

prop_overlapNotDisjoint' :: Span -> Span -> Property
prop_overlapNotDisjoint' v1 v2 =
  v1 /= mempty && v2 /= mempty ==>
     (v1 `overlap` v2 == mempty) == (v1 `disjoint` v2) &&
     (v1 `overlap` v2 /= mempty) == not (v1 `disjoint` v2)

-- ** Idempotent property
-- |
-- Intersection
--
prop_intersectionIdempotent :: [Span] -> [Span] -> Bool
prop_intersectionIdempotent v1 v2 =
  let s1 = fromList v1
      s2 = fromList v2
   in
      s1 `intersection` s2 ==
        (s1 `intersection` s2) `intersection` s2

prop_intersectionRangeIdempotent :: [Range] -> [Range] -> Bool
prop_intersectionRangeIdempotent v1 v2 =
   v1 `intersect` v2 == (v1 `intersect` v2) `intersect` v2

-- |
-- Instantiation
--
prop_instantiationIdempotent :: [Span] -> Bool
prop_instantiationIdempotent v1 =
  fromList v1 == (fromList . fullCircle $ v1)

prop_instantiationRangeIdempotent :: [Range] -> Bool
prop_instantiationRangeIdempotent v1 =
  unions v1 == (unions . unions $ v1)

-- |
-- Sorting
--
prop_sortIdempotent :: [Span] -> Bool
prop_sortIdempotent v1 =
  sort v1 == (sort . sort $ v1)

prop_sortRangeIdempotent :: [Range] -> Bool
prop_sortRangeIdempotent v1 =
  sort v1 == (sort . sort $ v1)

---------------------------------------------------------------------------------
spec_spanProps :: TestTree
spec_spanProps = testGroup "Span Prop Tests"
---------------------------------------------------------------------------------
  [ testGroup "Commutative property"
     [ testGroup "Range props"
       [
         testProperty "Semigroup" prop_commutativeSemi
       , testProperty "Eq"        prop_commutativeEq
       , testProperty "juxta"     prop_commutativeJuxta
       , testProperty "disjoint"  prop_commutativeDisjoint
       ]
     , testGroup "Span props"
       [
         testProperty "Semigroup"
                     (withMaxSuccess 1000 prop_commutativeSemi)
       , testProperty "Eq"        prop_commutativeEq'
       , testProperty "juxta"     prop_commutativeJuxta'
       , testProperty "disjoint"
               (withMaxSuccess 1000 prop_commutativeDisjoint')
       ]
     ]
  , testGroup "Overlap => with other props "
     [ testGroup "Range props"
       [
         testProperty "overlap disjoint"    prop_overlapDisjoint
       , testProperty "is juxta?"           prop_isJuxta
       , testProperty "overlap juxta"       prop_overlapJuxta
       , testProperty "overlap subset"      prop_overlapSubset
       , testProperty "overlap /= disjoint true when no mempty"
            (withMaxSuccess 1000 prop_overlapNotDisjoint)

       ]
     , testGroup "Span props"
       [
         testProperty "overlap subset" prop_overlapSubset'
       , testProperty "overlap subset left Exp" prop_overlapSubsetLeftExp'
       , testProperty "overlap /= disjoint fails b/c overlap not commutative"
               (expectFailure (withMaxSuccess 1000 prop_overlapNotDisjoint'))

       , testCase "Red Exp overlap /= mempty " $
         mkSpan Red 74 18 `overlap` mkSpan Exp 44 59 /= mempty @?= True

       , testCase "...but Exp Red overlap == mempty" $
         mkSpan Red 44 59 `overlap` mkSpan Exp 74 18 @?= mempty

       ]
     ]
  ]

---------------------------------------------------------------------------------
spec_spanSubset :: TestTree
spec_spanSubset = testGroup "Subset" [g1, g2, g3]
---------------------------------------------------------------------------------
  --
g1, g2, g3 :: TestTree
g1 = testGroup "span'Request (left) subset span'ETL (right)"

  [ testCase "year subset q1 is False" $
    year `subset` q1 @?= False

  , testCase "q1 subset year is True" $
    q1 `subset` year @?= True

  , testCase "q1 subset q1 is True" $
    q1 `subset` q1 @?= True

  , testCase "q1 subset (q1 plus one period of length) is True" $
    q1 `subset` incLen q1 @?= True
  , testCase "q1 plus one period of length subset q1 is False" $
    incLen q1 `subset` q1 @?= False

  , testCase "...however, q1 subset (q1 plus a *delay*) is False" $
    q1 `subset` delayStart q1 @?= False

  , testCase "q1 plus a *delay* subset q1 is False" $
    delayStart q1 `subset` q1 @?= False

  , testCase "q1 subset q3 is False" $
    mkSpan Exp 0 3 `Span.subset` mkSpan Exp 6 3 @?= False

  , testCase "Range q1 subset q3 is False" $
    mkRange 0 3 `Span.subset` mkRange 6 3 @?= False

  , testCase "q1 subset q2 is False" $
    mkSpan Exp 0 3 `Span.subset` mkSpan Exp 3 3 @?= False

  , testCase "Range q1 subset q2 is False" $
    mkRange 0 3 `Span.subset` mkRange 3 3 @?= False

  , testCase "q1 <= q2 is True" $
    mkSpan Exp 0 3 <= mkSpan Exp 3 3 @?= True

  , testCase "Range q1 <= q2 is True" $
    mkRange 0 3 <= mkRange 3 3 @?= True

  , testCase "q1 `disjoint` q2 is True" $
    mkSpan Exp 0 3 `disjoint` mkSpan Exp 3 3 @?= True

  , testCase "Range q1 `disjoint` q2 is True" $
    mkRange 0 3 `disjoint` mkRange  3 3 @?= True

  , testCase "q1 `juxta` q2 is True" $
    mkSpan Exp 0 3 `juxta` mkSpan Exp 3 3 @?= True

  , testCase "q2 subset q1 is False" $
    q2 `subset` q1 @?= False

  , testCase "q2 subset year is True" $
    q2 `subset` year @?= True
  ]

g2 = testGroup "Left Reduced and Right ETL Expressed"

  [ testCase "q3R subset year is True" $
    q3R `subset` year @?= True

  , testCase "...but q4+q1 next year is not with this year" $
    q4q1R `subset` year @?= False

  , testCase "...nor the opposite; False" $
    year `subset` q4q1R @?= False


  , testCase "q1R subset year is True" $
    q1R `subset` year @?= True


  ]

g3 = testGroup "Left Exp, Right Exp is rarely True"

  [ testCase "year subset q3R is False" $
    year `subset` q3R @?= False

  , testCase "Only True when lengths are 1 and starts the same" $
    q11 `subset` q11R @?= True

  , testCase "...opposite order, also True" $
    q11R `subset` q11 @?= True

  , testCase "Exp 8 4 `subset` Red 8 4: False" $
    mkSpan Exp 8 4 `subset` mkSpan Red 8 4 @?= False

  ]

---------------------------------------------------------------------------------
spec_range :: TestTree
spec_range = testGroup "Range properties"
---------------------------------------------------------------------------------
  [ testGroup "Construction - deriving Length" [

       testCase "deriving rangeEnd for Range 0 1 is 0" $
       rangeEnd (mkRange 0 1) @?= 0

     , testCase "deriving rangeEnd for Range 1 1 is 1" $
       rangeEnd (mkRange 1 1) @?= 1

     , testCase "deriving rangeEnd for Range 0 3 is 2" $
       rangeEnd (mkRange 0 3) @?= 2

     , testCase "deriving rangeEnd for Range 9 3 is 11" $
       rangeEnd (mkRange 9 3) @?= 11
     ]

  , testGroup "Ord (<=)" [
       testCase "Range 0 1 <= Range 0 1 True" $
         mkRange 0 1 <= mkRange 0 1 @?= True

     , testCase "Range 0 1 <= Range 0 3 True" $
         mkRange 0 1 <= mkRange 0 3 @?= True

     , testCase "...is not commutative" $
         mkRange 0 3 <= mkRange 0 1 @?= False
     ]

  , testGroup "Disjoint - disjoint" [
       testCase "Range 0 1 and Range 0 1 False" $
         mkRange 0 1 `disjoint` mkRange 0 1 @?= False

     , testCase "Range 0 1 and Range 0 3 False" $
         mkRange 0 1 `disjoint` mkRange 0 3 @?= False

     , testCase "...is commutative" $
         mkRange 0 3 `disjoint` mkRange 0 1 @?= False

     , testCase "Range 1 3 and Range 0 3 False" $
         mkRange 1 3 `disjoint` mkRange 0 3 @?= False

     , testCase "...is commutative" $
         mkRange 0 3 `disjoint` mkRange 1 3 @?= False

     , testCase "Juxta-positioned Range 0 3 and Range 3 1 True" $
         mkRange 0 3 `disjoint` mkRange 3 1 @?= True

     , testCase "Range 0 3 and Range 8 3 True" $
         mkRange 0 3 `disjoint` mkRange 8 3 @?= True

     , testCase "Empty 3 0 and Range 8 3 False" $
         mkRange 3 0 `disjoint` mkRange 8 3 @?= False

     , testCase "...is commutative" $
         mkRange 8 3 `disjoint` mkRange 3 0 @?= False
     ]
  , testGroup "Disjoint - juxta" [
       testCase "Range 0 1 and Range 0 1 False" $
         mkRange 0 1 `juxta` mkRange 0 1 @?= False

     , testCase "Range 0 1 and Range 0 3 False" $
         mkRange 0 1 `juxta` mkRange 0 3 @?= False

     , testCase "...is commutative" $
         mkRange 0 3 `juxta` mkRange 0 1 @?= False

     , testCase "Range 1 3 and Range 0 3 False" $
         mkRange 1 3 `juxta` mkRange 0 3 @?= False

     , testCase "...is commutative" $
         mkRange 0 3 `juxta` mkRange 1 3 @?= False

     , testCase "Juxta-positioned Range 0 3 and Range 3 1 True" $
         mkRange 0 3 `juxta` mkRange 3 1 @?= True

     , testCase "...is commutative" $
         mkRange 3 1 `juxta` mkRange 0 3 @?= True

     , testCase "Range 0 3 and Range 8 3 False" $
         mkRange 0 3 `juxta` mkRange 8 3 @?= False

     , testCase "...is commutative" $
         mkRange 8 3 `juxta` mkRange 0 3 @?= False

     , testCase "Empty 3 0 and Range 8 3 True" $
         mkRange 3 0 `juxta` mkRange 4 3 @?= True

     , testCase "...is commutative" $
         mkRange 4 3 `juxta` mkRange 3 0 @?= True
     ]

  , testGroup "Semigroup <>"  [
       testCase "Range 0 1 + Range 0 1 = Range 0 1" $
         mkRange 0 1 <> mkRange 0 1 @?= mkRange 0 1

     , testCase "Range 0 3 and Range 1 3 = Range 0 4" $
         mkRange 0 3 <> mkRange 1 3 @?= mkRange 0 4

     , testCase "...is commutative" $
         mkRange 1 3 <> mkRange 0 3 @?= mkRange 0 4

     , testCase "Juxtaposed Range 0 3 and Range 3 1 = True" $
         mkRange 0 3 `juxta` mkRange 3 1 @?= True

     , testCase "...so will combine together" $
         mkRange 0 3 <> mkRange 3 1 @?= mkRange 0 4

     , testCase "Empty 3 0 and Range 8 3 = Range 8 3" $
         mkRange 3 0 <> mkRange 8 3 @?= mkRange 8 3


     ]
  ]

---------------------------------------------------------------------------------
spec_spanEq :: TestTree
spec_spanEq = testGroup "Span Eq"
---------------------------------------------------------------------------------
  [
    testGroup "Exp Exp" [
       testCase "Exp Exp with matching ranges 2 1 True" $
         mkSpan Exp 2 1 == mkSpan Exp 2 1 @?= True

     , testCase "Exp Exp with different ranges False" $
         mkSpan Exp 0 2 == mkSpan Exp 0 1 @?= False

     , testCase "Exp Exp with zero length always True" $
         mkSpan Exp 1 0 == mkSpan Exp 2 0 @?= True

     ]
  , testGroup "Red Red" [
       testCase "Red Red with both ranges 0 1 True" $
         mkSpan Red 0 1 == mkSpan Red 0 1 @?= True

     , testCase "Red Red with different ranges False" $
         mkSpan Red 0 2 == mkSpan Red 0 1 @?= False

     , testCase "Red Red with zero length always True" $
         mkSpan Red 1 0 == mkSpan Red 0 0 @?= True
     ]
  , testGroup "Exp Red" [
       testCase "Any pair of Spans with Range length 0 True" $
         mkSpan Exp 1 0 == mkSpan Red 3 0 @?= True

     , testCase "Any Spans with length 1 and the same start True" $
         mkSpan Exp 3 1 == mkSpan Red 3 1 @?= True

     , testCase "But nothing else; Exp 3 2, Red 3 2 False" $
         mkSpan Exp 3 2 == mkSpan Red 3 2 @?= False
     ]
  ]
---------------------------------------------------------------------------------
spec_spanOrd :: TestTree
spec_spanOrd = testGroup "Span Ord (<=)"
---------------------------------------------------------------------------------
  [
    testGroup "Exp Exp" [
       testCase "Exp Exp with matching ranges 2 1 True" $
         mkSpan Exp 2 1 <= mkSpan Exp 2 1 @?= True

     , testCase "Exp 0 2 <= Exp 0 1 False" $
         mkSpan Exp 0 2 <= mkSpan Exp 0 1 @?= False

     , testCase "Exp Exp with zero length always True" $
         mkSpan Exp 1 0 <= mkSpan Exp 2 0 @?= True

     , testCase "q2 <= 23 True" $
       q2 <= q23 @?= True

     , testCase "q3 <= 23 True" $
       q3 <= q23 @?= True

     , testCase "q23 <= q3 False" $
       q23 <= q3 @?= False

     , testCase "q4 <= 23 False" $
       q4 <= q23 @?= False

     , testCase "...does not commute" $
       q23 <= q4 @?= True
     ]

  , testGroup "Red Red" [
       testCase "Red Red with both ranges 0 1 True" $
         mkSpan Red 0 1 <= mkSpan Red 0 1 @?= True

     , testCase "Red 0 2 <= Red 0 1 False" $
         mkSpan Red 0 2 <= mkSpan Red 0 1 @?= False

     , testCase "Red Red with zero length always True" $
         mkSpan Red 1 0 <= mkSpan Red 0 0 @?= True
     ]

  , testGroup "Exp Red" [
       testCase "Any pair of Spans with Range length 0 True" $
         mkSpan Exp 1 0 <= mkSpan Red 3 0 @?= True

     , testCase "Any Spans with length 1 and the same start True" $
         mkSpan Exp 3 1 <= mkSpan Red 3 1 @?= True

     , testCase "Exp comes before Red (processing bias)" $
         mkSpan Exp 3 2 <= mkSpan Red 3 2 @?= True

     , testCase "...not the other way around" $
         mkSpan Red 3 2 <= mkSpan Exp 3 2 @?= False
     ]
  , testGroup "Sort list" [
      testCase "sort list Exp & Red" $
        sort [
            mkSpan Exp 22 44
          , mkSpan Exp 62 68
          , mkSpan Red 43 22
          , mkSpan Red 49 0
          , mkSpan Exp 12 34
             ] @?=
           [
             mkSpan Red 49 0
           , mkSpan Exp 12 34
           , mkSpan Exp 22 44
           , mkSpan Exp 62 68
           , mkSpan Red 43 22
           ]
    ]
  ]

---------------------------------------------------------------------------------
spec_spanSemigroup :: TestTree
spec_spanSemigroup = testGroup "Span Semigroup"
---------------------------------------------------------------------------------
  [
    testGroup "Exp <> Red with Eq Ranges = Exp year"

       [ testCase "Exp year trumps Red year" $
         year <> yearR @?= year

       , testCase "...commutes" $
         yearR <> year @?= year
       ]

  , testGroup "Exp <> Red with different Ranges = mempty"

       [ testCase "Exp quarter <> Red year = mempty" $
         q2 <> yearR @?= mempty

       , testCase "...always mempty" $
         yearR <> q2 @?= mempty
       ]

  , testGroup "Span <>" [

       testCase "q2 <> 23 = 23" $
       q2 <> q23 @?= q23

     , testCase "q3 <> 23 = 23" $
       q3 <> q23 @?= q23

     , testCase "q4 juxtaposes 23 True" $
       q4 `juxta` q23 @?= True

     , testCase "...so q4 <> 23 = 234" $
       q4 <> q23 @?= q234

     , testCase "...is commutative" $
       q23 <> q4 @?= q234

     , testCase "q1 juxtaposes 23 True" $
       q1 `juxta` q23 @?= True

     , testCase "...so q1 <> 23 = 123" $
       q1 <> q23 @?= q123

     , testCase "Combine adjacent periods" $
       incLen q1 <> q23 @?= q123

     , testCase " incLen q1, Juxta q23 = False" $
       incLen q1 `juxta` q23 @?= False

     , testCase " incLen q1, Disjoint q23 = False" $
       incLen q1 `disjoint` q23 @?= False

     , testCase " incLen q1 subset of q23 = False" $
       incLen q1 `subset` q23 @?= False

     , testCase "Combine adjacent periods q1 q2 = 12" $
       q1 <> q2 @?= q12

     ]
  ]

---------------------------------------------------------------------------------
spec_construction :: TestTree
spec_construction = testGroup "ETL data"
---------------------------------------------------------------------------------
  [
    testGroup "Instantiation" [
       testCase "single series" $
         [mkSpan Exp 2 1] @?= toList (fromList [mkSpan Exp 2 1])

      , testCase "two spans with a subset relation = single span" $
         toList (fromList [mkSpan Exp 0 4, mkSpan Exp 2 1]) @?=
         [mkSpan Exp 0 4]

      , testCase "spans with a subset relation plus mempty = single span" $
         [mkSpan Exp 0 4] @?=
         toList (fromList [mkSpan Exp 0 4, mkSpan Exp 2 1, mkSpan Red 9 0])

      , testCase "one span with mempty = single span " $
         [mkSpan Exp 0 4] @?=
         toList (fromList [mkSpan Exp 0 4, mkSpan Exp 5 0, mkSpan Red 9 0])

      , testCase "juxta spans with mempty = single, but larger span " $
         toList (fromList [mkSpan Exp 0 4, mkSpan Exp 4 1, mkSpan Red 9 0]) @?=
         [mkSpan Exp 0 5]

      , testCase "insert juxta span values = one value" $
        mkSpan Exp 0 1 `union` [mkSpan Exp 1 1] @?= [mkSpan Exp 0 2]

      , testCase "insert juxta span values = one value" $
        mkSpan Exp 0 2 `union` [mkSpan Exp 2 1] @?= [mkSpan Exp 0 3]

      , testCase "0 2 `juxta` 2 1 = True" $
         mkRange 0 2 `juxta` mkRange 2 1 @?= True

      , testCase "0 2 <> 2 1 = 0 3" $
         mkRange 0 2 <> mkRange 2 1 @?= mkRange 0 3

      , testCase "Exp 0 2 `juxta` Exp 2 1 = True" $
         mkSpan Exp 0 2 `juxta` mkSpan Exp 2 1 @?= True

      , testCase "Exp 0 2 <> Exp 2 1 = Exp 0 3" $
         mkSpan Exp 0 2 <> mkSpan Exp 2 1 @?= mkSpan Exp 0 3

      , testCase "mempty insert into list = list" $
        mkSpan Exp 1 0 `union` [mkSpan Exp 2 1] @?= [mkSpan Exp 2 1]

      , testCase "...is commutative" $
        mkSpan Exp 2 1 `union` [mkSpan Exp 1 0] @?= [mkSpan Exp 2 1]

      , testCase "two spans with a subset relation = single span" $
        mkSpan Exp 2 1 `union` [mkSpan Exp 0 4] @?= [mkSpan Exp 0 4]

      , testCase "...is commutative" $
        mkSpan Exp 0 4 `union` [mkSpan Exp 2 1] @?= [mkSpan Exp 0 4]

      , testCase "consolidate list -> FV Set" $
        fromList
           [ mkSpan Exp 22 44
           , mkSpan Exp 62 68
           , mkSpan Exp 43 22
           , mkSpan Exp 12 34
           ] @?=
        fromList [ mkSpan Exp 12 118 ]

      , testCase "consolidate list with mempty" $
        fromList
           [ mkSpan Exp 53 99
           , mkSpan Exp 29 32
           , mkSpan Red 99 0
           , mkSpan Red 37 96
           ] @?=
        fromList [ mkSpan Exp 29 123 ]

       ]
  ]

---------------------------------------------------------------------------------
spec_subset :: TestTree
spec_subset = testGroup "Span FieldValues subset prop"
---------------------------------------------------------------------------------
  [
    testGroup "Span FieldValues Subset" [

        testCase "FV [Exp 0 1, Exp 1 2] <= FV [Exp 3 1, Exp 4 1] = False" $
        fromList [mkSpan Exp 0 1, mkSpan Exp 0 2] <=
        fromList [mkSpan Exp 3 1, mkSpan Exp 4 1]
        @?= False

      , testCase "FV [Exp 0 1, Exp 1 2] <= FV [Exp 0 1, Exp 4 1] = False" $
        fromList [mkSpan Exp 0 1, mkSpan Exp 0 2] <=
        fromList [mkSpan Exp 3 1, mkSpan Exp 4 1]
        @?= False

      , testCase "FV [Exp 0 4, Red 0 7] <= [Exp 0 6, Red 8 4] = False" $
        fromList [mkSpan Exp 0 4, mkSpan Red 0 7] <=
        fromList [mkSpan Exp 0 6, mkSpan Red 8 4]
        @?= False

      , testCase "FV [Red 0 7, Exp 0 4] <= FV [Exp 0 6, Red 8 4] = False" $
        fromList [mkSpan Red 0 7, mkSpan Exp 0 4] <=
        fromList [mkSpan Exp 0 6, mkSpan Red 8 4]
        @?= False

      , testCase "FV [Red 0 7, Exp 0 4] <= FV [Red 8 4, Exp 0 6] = False" $
        fromList [mkSpan Red 0 7, mkSpan Exp 0 4] <=
        fromList [mkSpan Red 8 4, mkSpan Exp 0 6]
        @?= False

      , testCase "FV [Exp 2 1] <= FV [Exp 0 4] = True" $
        fromList [mkSpan Exp 2 1] <=
        fromList [mkSpan Exp 0 4]
        @?= True
      ]
  ]

---------------------------------------------------------------------------------
spec_request :: TestTree
spec_request = testGroup "Request from ETL data"
---------------------------------------------------------------------------------
  [
    testGroup "Intersection" [
       testCase "two Eq spans => span" $
         fromList [mkSpan Exp 2 1] `intersection`
         fromList [mkSpan Exp 2 1] @?=
         fromList [mkSpan Exp 2 1]

      , testCase "two spans with a subset relation = single span" $
         fromList [mkSpan Exp 2 1] `intersection`
         fromList [mkSpan Exp 0 4] @?=
         fromList [mkSpan Exp 2 1]

      , testCase "[Red 0 7] `intersection` [Exp 0 6, Red 8 4] = mempty" $
        fromList [mkSpan Red 0 7] `intersection`
        fromList [mkSpan Exp 0 6, mkSpan Red 8 4] @?= SpanSet (Set.fromList [])

      , testCase "overlap of different Red spans = mempty" $
         mconcat (overlap (mkSpan Red 2 2)
                  <$> [mkSpan Red 0 4, mkSpan Red 2 3]) @?= mempty

      , testCase "overlap Red 0 7 with [Exp 0 6, Red 8 4] = mempty" $
         mconcat (overlap (mkSpan Red 0 7)
                  <$> [mkSpan Exp 0 6, mkSpan Red 8 4]) @?= mempty

      , testCase "overlap Red 0 4 with [Exp 0 6, Red 8 4] = Red 0 4" $
         mconcat (overlap (mkSpan Red 0 4)
                  <$> [mkSpan Exp 0 6, mkSpan Red 8 4]) @?=
                    mkSpan Red 0 4

      , testCase "...intersection = Red 0 4" $
         toList  (intersection
                 (fromList [mkSpan Red 0 4, mkSpan Red 0 7])
                 (fromList [mkSpan Exp 0 6]))
                 @?= [mkSpan Red 0 4]

      , testCase "overlap with a zero length search = mempty" $
         mconcat (overlap (mkSpan Exp 2 0) <$> [mkSpan Exp 0 4]) @?= mempty

      , testCase "overlap search with a subset relation = search" $
         mconcat (overlap (mkSpan Exp 2 1) <$> [mkSpan Exp 0 4])
         @?= mkSpan Exp 2 1

      , testCase "Exp request that overlaps range of etl = overlap" $
         intersection
         (fromList [mkSpan Red 9 0, mkSpan Exp 0 8, mkSpan Exp 7 0])
         (fromList [mkSpan Exp 0 6, mkSpan Red 8 4])
         @?= fromList [mkSpan Exp 0 6]

      , testCase "overlap Exp 0 8 with Exp 0 6 = Exp 0 6" $
         mkSpan Exp 0 8 `overlap` mkSpan Exp 0 6
         @?= mkSpan Exp 0 6

      , testCase "Red request that overlaps range of etl = []" $
         fromList [mkSpan Red 0 8] `intersection`
         fromList [mkSpan Exp 0 6, mkSpan Red 8 4]
         @?= SpanSet (Set.fromList [])

     ]
  ]

---------------------------------------------------------------------------------
spec_idempotentProp :: TestTree
spec_idempotentProp = testGroup "Idempotent property"
---------------------------------------------------------------------------------
  [
    testProperty "intersection Spans"
      (withMaxSuccess 1000 prop_intersectionIdempotent)

  , testProperty "intersection Ranges"
      (withMaxSuccess 1000 prop_intersectionRangeIdempotent)

  , testProperty "instantiation Spans"  prop_instantiationIdempotent

  , testProperty "instantiation Ranges"
      (withMaxSuccess 1000 prop_instantiationRangeIdempotent)

  , testProperty "sort Spans"
      (withMaxSuccess 1000 prop_sortIdempotent)

  , testProperty "sort Ranges"
      (withMaxSuccess 1000 prop_sortRangeIdempotent)

  ]

fullCircle :: [Span] -> [Span]
fullCircle = toList . fromList

---------------------------------------------------------------------------------
-- Where each unit = 1 month
year, q1, q12, q23, q123, q123', q234, q2, q3, q4, q4q1, q11, q11R :: Span
yearR, q1R, q2R, q3R, q4R, q4q1R :: Span
year  = mkSpan Exp 0 12
q1    = mkSpan Exp 0 3
q12   = mkSpan Exp 0 6
q2    = mkSpan Exp 3 3
q23   = mkSpan Exp 3 6
q123  = mkSpan Exp 0 9
q123' = mkSpan Exp 1 9
q234  = mkSpan Exp 3 9
q3    = mkSpan Exp 6 3
q4    = mkSpan Exp 9 3
q4q1  = mkSpan Exp 9 6
yearR = mkSpan Red 0 12
q1R   = mkSpan Red 0 3
q2R   = mkSpan Red 3 3
q3R   = mkSpan Red 6 3
q4R   = mkSpan Red 9 3
q4q1R = mkSpan Red 9 6
q11   = mkSpan Exp 0 1
q11R  = mkSpan Red 0 1
