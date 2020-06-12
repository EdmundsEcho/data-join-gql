module Main
  ( main,
  )
  where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun
-------------------------------------------------------------------------------
import qualified ETL.Span                     (tests)
-------------------------------------------------------------------------------
import           TestFeature                  (testObs)
import qualified TestRoot                     as Test (api)
-------------------------------------------------------------------------------

main :: IO ()
main = do
  gqlTest   <- testObs Test.api     "GQLSpec"

  liftIO $ defaultMainWithRerun
    (
        testGroup
           "Lucivia ETL API Tests"
           [ tests         -- unit and prop tests
           , gqlTest
           ]
     )

tests :: TestTree
tests = testGroup "Lucivia ETL API Tests" [ unitTests ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ ETL.Span.tests ]
