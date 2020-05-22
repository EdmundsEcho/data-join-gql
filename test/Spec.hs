module Main
  where
-------------------------------------------------------------------------------
import           Protolude
-------------------------------------------------------------------------------
import           Test.Tasty
-- import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import qualified ETL.Span   (tests)
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lucivia ETL API Tests" [ unitTests ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ ETL.Span.tests ]
