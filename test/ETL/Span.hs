module ETL.Span
  where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Model.ETL.Span
import           Model.ETL.TagRedExp
-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Span request dynamics"
  [ spec_spanSubset
  , spec_spanIntersection_1
  , spec_spanIntersection_2
  , spec_range
  , spec_spanEq
  , spec_spanOrd
  , spec_spanSemigroup
  , spec_spanProps
  ]

-- Arbitrary values for QuickCheck
data Trivial = Trivial deriving (Eq, Show)
instance Arbitrary Trivial

trivialGen :: Gen Trivial
trivialGen = return Trivial

---------------------------------------------------------------------------------
spec_spanProps :: TestTree
spec_spanProps = testGroup "Span Prop Tests"
---------------------------------------------------------------------------------
  [ testGroup "prop group"
    [ testProperty "prop" $
      \start len -> mkRange start len == mkRange start len
    , testProperty "prop" $
      \start len -> mkRange start len == mkRange start len

    ]
  ]
---------------------------------------------------------------------------------
spec_spanSubset :: TestTree
spec_spanSubset = testGroup "Subset" [g1, g2, g3]
---------------------------------------------------------------------------------
  --
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

  , testCase "q1 subset q2 is False" $
    q1 `subset` q2 @?= False

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
  ]

---------------------------------------------------------------------------------
spec_spanIntersection_1 :: TestTree
spec_spanIntersection_1 = testGroup "Intersection"
---------------------------------------------------------------------------------
  [ testGroup ">> support functions"

     [ testCase "spanLength start:0 end:0 is 1"
         $ spanLength 0 0 @?= 1
     , testCase "spanEnd Range { rangeStart=0, rangeLength=1 } is 0"
         $ spanEnd (Range 0 1) @?= 0
     ]

  , testCase "q1 intersection q1 is q1" $
    q1 `intersection` q1 @?= RightM q1

  , testCase "q1 intersection year is q1" $
    q1 `intersection` year @?= RightM q1

  , testCase "year intersection q1 is q1 **with a warning**" $
    year `intersection` q1
                 @?= LeftM (["Warning: Truncated request"], Just q1)

  , testCase "q4q1 intersection year is q4 **with a warning**" $
    q4q1 `intersection` year
                 @?= LeftM (["Warning: Truncated span series"], Just q4)

  , testCase "the two ranges are not disjoint" $ q4q1 `disjoint` year @?= False
  , testCase "commutative prop" $ year `disjoint` q4q1 @?= q4q1 `disjoint` year

  , testGroup "q4q1 intersection with year"

     [ testCase ("The combined START is set by @q4q1@: "
                 <> show (rangeStart $ getRange q4q1))
       $ interStart (getRange q4q1) (getRange year)
       @?= (rangeStart $ getRange q4q1)

     , testCase ("The combined END is set by @year@: "
                 <> show (spanEnd $ getRange year))
       $ interEnd (getRange q4q1) (getRange year)
       @?= (spanEnd $ getRange year)

     , testCase ("The combined inclusive LENGTH is therefore: "
                <> show ((spanEnd $ getRange year)
                      - (rangeStart $ getRange q4q1) + 1))
       $ interLength (getRange q4q1) (getRange year)
       @?= (spanEnd $ getRange year) - (rangeStart $ getRange q4q1) + 1

     ]

  , testCase "q4q1 intersection q4 is q4 **with a warning**" $
    q4q1 `intersection` q4
                 @?= LeftM (["Warning: Truncated request"], Just q4)

  , testCase "q4q1 intersection q3 returns Nothing" $
    q4q1 `intersection` q3
                 @?= LeftM (["Error: Juxta/disjoint request"], Nothing)
  ]

---------------------------------------------------------------------------------
spec_spanIntersection_2 :: TestTree
spec_spanIntersection_2 = testGroup "Intersection with Red"
---------------------------------------------------------------------------------
  [
    testCase "Red/summary of year intersects with Exp/series" $
    yearR `intersection` year @?= RightM yearR

  , testCase "...but not the other way around" $
    year `intersection` yearR @?= LeftM (["Error: No series of span"], Nothing)

  , testGroup "Deep-dive for Exp/series Red/summary" [

      testGroup "Subset is not commutative"

      [ testCase "Exp series is not a Subset of Red" $
        year `subset` yearR @?= False

      , testCase "...but Red year is a Subset of Exp" $
        yearR `subset` year @?= True
      ]

      , testGroup "Disjoint is commutative"

      [ testCase "Exp series is not Disjoint with Red" $
         year `disjoint` yearR @?= False

       , testCase "...but neither is Red year with Exp" $
         yearR `disjoint` year @?= False
       ]

  ]
  , testCase "q4q1 series does not intersect with yearR" $
    q4q1 `intersection` yearR
    @?= LeftM (["Error: Disjoint request"], Nothing)

  , testCase "...nor when both sides are Red/Summary" $
    q4q1R `intersection` yearR
    @?= LeftM (["Error: Disjoint request"], Nothing)

  , testCase "...but does when both are series (Exp)" $
    q4q1 `intersection` year
    @?= LeftM (["Warning: Truncated span series"], Just q4)

  , testCase "...and when the LHS is Red **with a warning**" $
    q4q1R `intersection` year
    @?= LeftM (["Warning: Truncated summary span value"], Just q4R)

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

  , testGroup "Semigroup <>, aka union"  [
       testCase "Range 0 1 + Range 0 1 = Range 0 1" $
         mkRange 0 1 `union` mkRange 0 1 @?= mkRange 0 1

     , testCase "Range 0 3 and Range 1 3 = Range 0 4" $
         mkRange 0 3 `union` mkRange 1 3 @?= mkRange 0 4

     , testCase "...is commutative" $
         mkRange 1 3 `union` mkRange 0 3 @?= mkRange 0 4

     , testCase "Juxtaposed Range 0 3 and Range 3 1 = True" $
         mkRange 0 3 `juxta` mkRange 3 1 @?= True

     , testCase "...so will combine together" $
         mkRange 0 3 `union` mkRange 3 1 @?= mkRange 0 4

     , testCase "Empty 3 0 and Range 8 3 = Range 8 3" $
         mkRange 3 0 `union` mkRange 8 3 @?= mkRange 8 3

     ]
  ]

---------------------------------------------------------------------------------
spec_spanEq :: TestTree
spec_spanEq = testGroup "Span Eq"
---------------------------------------------------------------------------------
  [
    testGroup "Exp Exp" [
       testCase "Exp Exp with matching ranges 2 1 True" $
         (mkSpan Exp 2 1) == (mkSpan Exp 2 1) @?= True

     , testCase "Exp Exp with different ranges False" $
         (mkSpan Exp 0 2) == (mkSpan Exp 0 1) @?= False

     , testCase "Exp Exp with zero length always True" $
         (mkSpan Exp 1 0) == (mkSpan Exp 2 0) @?= True

     ]
  , testGroup "Red Red" [
       testCase "Red Red with both ranges 0 1 True" $
         (mkSpan Red 0 1) == (mkSpan Red 0 1) @?= True

     , testCase "Red Red with different ranges False" $
         (mkSpan Red 0 2) == (mkSpan Red 0 1) @?= False

     , testCase "Red Red with zero length always True" $
         (mkSpan Red 1 0) == (mkSpan Red 0 0) @?= True
     ]
  , testGroup "Exp Red" [
       testCase "Any pair of Spans with Range length 0 True" $
         (mkSpan Exp 1 0) == (mkSpan Red 3 0) @?= True

     , testCase "Any Spans with length 1 and the same start True" $
         (mkSpan Exp 3 1) == (mkSpan Red 3 1) @?= True

     , testCase "But nothing else; Exp 3 2, Red 3 2 False" $
         (mkSpan Exp 3 2) == (mkSpan Red 3 2) @?= False
     ]
  ]
---------------------------------------------------------------------------------
spec_spanOrd :: TestTree
spec_spanOrd = testGroup "Span Ord (<=)"
---------------------------------------------------------------------------------
  [
    testGroup "Exp Exp" [
       testCase "Exp Exp with matching ranges 2 1 True" $
         (mkSpan Exp 2 1) <= (mkSpan Exp 2 1) @?= True

     , testCase "Exp Exp with different ranges False" $
         (mkSpan Exp 0 2) <= (mkSpan Exp 0 1) @?= False

     , testCase "Exp Exp with zero length always True" $
         (mkSpan Exp 1 0) <= (mkSpan Exp 2 0) @?= True

     ]
  , testGroup "Red Red" [
       testCase "Red Red with both ranges 0 1 True" $
         (mkSpan Red 0 1) <= (mkSpan Red 0 1) @?= True

     , testCase "Red Red with different ranges False" $
         (mkSpan Red 0 2) <= (mkSpan Red 0 1) @?= False

     , testCase "Red Red with zero length always True" $
         (mkSpan Red 1 0) <= (mkSpan Red 0 0) @?= True
     ]
  , testGroup "Exp Red" [
       testCase "Any pair of Spans with Range length 0 True" $
         (mkSpan Exp 1 0) <= (mkSpan Red 3 0) @?= True

     , testCase "Any Spans with length 1 and the same start True" $
         (mkSpan Exp 3 1) <= (mkSpan Red 3 1) @?= True

     , testCase "But nothing else; Exp 3 2, Red 3 2 False" $
         (mkSpan Exp 3 2) <= (mkSpan Red 3 2) @?= False
     ]
  ]

---------------------------------------------------------------------------------
spec_spanSemigroup :: TestTree
spec_spanSemigroup = testGroup "Semigroup"
---------------------------------------------------------------------------------
  [
    testGroup "Range" [
       testCase "Exp trumps Red" $
       yearR <> year @?= year

     , testCase "...commutates" $
       year <> yearR @?= year

     ]
  , testGroup "Span Ord" [
       testCase "q2 <= 23 True" $
       q2 <= q23 @?= True

     , testCase "q3 <= 23 True" $
       q3 <= q23 @?= True

     , testCase "q4 <= 23 False" $
       q3 <= q23 @?= True

     , testCase "...does not commute" $
       q23 <= q3 @?= False
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
       (incLen q1) <> q23 @?= q123

     , testCase " incLen q1, Juxta q23 = False" $
       (incLen q1) `juxta` q23 @?= False

     , testCase " incLen q1, Disjoint q23 = False" $
       (incLen q1) `disjoint` q23 @?= False

     , testCase " incLen q1 subset of q23 = False" $
       (incLen q1) `subset` q23 @?= False

     , testCase "Combine adjacent periods q1 q2 = 12" $
       q1 <> q2 @?= q12

     ]
  ]

---------------------------------------------------------------------------------
-- Where each unit = 1 month
year, q1, q2, q3, q4, q4q1, q11, q11R :: Span
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
