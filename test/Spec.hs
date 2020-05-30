module Main
  where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun
-------------------------------------------------------------------------------
import qualified ETL.Span                     (tests)
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithRerun tests

tests :: TestTree
tests = testGroup "Lucivia ETL API Tests" [ unitTests ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ ETL.Span.tests ]
