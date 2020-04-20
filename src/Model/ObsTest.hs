-- | Model ObsEtl
module Model.ObsTest where

import           Protolude

data ObsTest = ObsTest {
  subject :: !Text,
  quality :: !Quality
}

data Quality = Quality {
  name   :: !Text,
  values :: ![Text]
}

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
