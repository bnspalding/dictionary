module WiktionarySpec
  ( spec,
  )
where

import Test.Hspec
import Wiktionary

spec :: Spec
spec = do
  describe "fromJSON" $ do
    it "creates a Dictionary from a set of JSON entries" $ False
