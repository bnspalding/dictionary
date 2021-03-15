module DictionaryIOSpec
  ( spec,
  )
where

import DictionaryIO
import DictionaryTestData
import Test.Hspec

spec :: Spec
spec =
  describe "read-write" $
    it "writes a dictionary to file and reads back the same" $
      do
        writeDictionary "test/test.jsonl" caseMockDictC
        dict <- readDictionary "test/test.jsonl"
        dict `shouldBe` caseMockDictC
