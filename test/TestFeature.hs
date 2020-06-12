{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module TestFeature
  ( testFeature
  , testObs
  )
where

---------------------------------------------------------------------------------
import           Protolude                  hiding (ByteString, decodeUtf8)
---------------------------------------------------------------------------------
import           Data.Aeson                 (Value, decode, encode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import           Data.Morpheus.Types        (GQLRequest (..), GQLResponse (..))
import           Data.Text                  (unpack)
import qualified Data.Text.Lazy             as LT (toStrict)
import           Data.Text.Lazy.Encoding    (decodeUtf8)
---------------------------------------------------------------------------------
import           Test.Tasty                 (TestTree)
import           Test.Tasty.HUnit           (assertFailure, testCase)
---------------------------------------------------------------------------------
import           Lib                        (getGQLBody, getResponseBody,
                                             maybeVariables)
import           Types                      (Case (..), Name, testWith)
---------------------------------------------------------------------------------
import           TestTypes
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- ** Test
-- |
-- How to get the `ObsETL` app into the testing context.
--
-- 1. Cast/Wrap testFeature with TestObs version of the testFeature function
--    ... i.e., instantiate the TestObs app then...
--
-- 2. Unwrap/consume the TestObs monad using the natural transformation
--    TestObs a -> IO a
--
testObs :: (GQLRequest -> TestObs GQLResponse) -> Name -> IO TestTree
testObs api name = do
  db <- liftIO $ newTVarIO dbInit
  nat (Env db Nothing) (testFeature api name)

---------------------------------------------------------------------------------
-- ** Generic `m TestTree`
-- |
--
testFeature :: MonadIO m
            => (GQLRequest -> m GQLResponse) -> Name -> m TestTree
testFeature api = testWith (testByFiles api)

---------------------------------------------------------------------------------
-- ** Generic `m TestTree`
-- |
--
testByFiles :: MonadIO m
            => (GQLRequest -> m GQLResponse) -> Case -> m TestTree

testByFiles testApi Case { path, description } = do
  testCaseQuery     <- liftIO $ getGQLBody path
  testCaseVariables <- liftIO $ maybeVariables path
  expectedResponse  <- liftIO $ getResponseBody path
  actualResponse    <- encode
                       <$> testApi (packGQLRequest testCaseQuery
                                                 testCaseVariables)
  case decode actualResponse of
    Nothing -> liftIO $ assertFailure "Bad Response"
    Just response -> return . testCase (unpack path <> " | " <> description)
                       $ customTest expectedResponse response
      where
        customTest expected value
          | expected == value = return ()
          | otherwise =
            assertFailure . LB.unpack
                          $ "expected: \n " <> encode expected
                                            <> " \n but got: \n "
                                            <> actualResponse


packGQLRequest :: ByteString -> Maybe Value -> GQLRequest
packGQLRequest queryBS variables =
  GQLRequest
    { operationName = Nothing,
      query = LT.toStrict $ decodeUtf8 queryBS,
      variables
    }




---------------------------------------------------------------------------------
