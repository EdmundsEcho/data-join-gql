{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Case (..),
    Name,
    testWith,
  )
where

---------------------------------------------------------------------------------
import           Prelude    (String)
import           Protolude
---------------------------------------------------------------------------------
import           Data.Aeson (FromJSON)
import           Data.Text  (unpack)
import qualified Data.Text  as T (concat)
import           Test.Tasty (TestTree, testGroup)
---------------------------------------------------------------------------------
import           Lib        (getCases)
---------------------------------------------------------------------------------

type Name = Text

data Case = Case
  { path        :: Text,
    description :: String
  }
  deriving (Generic, FromJSON)

testWith :: MonadIO m => (Case -> m TestTree) -> Name -> m TestTree
testWith f dir = do
  cases <- liftIO $ getCases (unpack dir)
  test <- sequence $ f <$> map (\x -> x {path = T.concat [dir, "/", path x]}) cases
  return $ testGroup (unpack dir) test

---------------------------------------------------------------------------------
