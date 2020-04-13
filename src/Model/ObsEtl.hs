-- | Model ObsEtl
module Model.ObsEtl where

import           Protolude

data ObsEtl = ObsEtl {
  subject :: !Text,
  quality :: !Quality
}

data Quality = Quality {
  name   :: !Text,
  values :: ![Text]
}

testInstance :: ObsEtl
testInstance = ObsEtl
  { subject = "test subject"
  , quality = testQuality
  }

testQuality :: Quality
testQuality = Quality
  { name = "color"
  , values = ["red", "blud"]
  }
