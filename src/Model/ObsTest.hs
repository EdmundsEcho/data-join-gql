-- | Model ObsEtl
module Model.ObsTest where

import           Protolude

data ObsTest = ObsTest {
  subject :: !Text,
  quality :: !Quality
} deriving (Show)

data Quality = Quality {
  name   :: !Text,
  values :: ![Text]
} deriving (Show)

testInstance :: ObsTest
testInstance = ObsTest
  { subject = "test subject"
  , quality = testQuality
  }

testQuality :: Quality
testQuality = Quality
  { name = "color"
  , values = ["red", "blue"]
  }
