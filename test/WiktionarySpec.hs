module WiktionarySpec
  ( spec,
  )
where

import qualified Data.ByteString.Lazy as B
import Test.Hspec
import Wiktionary

spec :: Spec
spec = do
  describe "readJSONL" $ do
    it "parses JSONL data into WiktData" $ False
  describe "readJSONSingle" $ do
    it "parses a JSON object int a WiktData object" $ False
  describe "makeDictionary" $ do
    it "constructs a Dictionary from a set of WiktData objects" $ False
  describe "wiktDataToEntry" $ do
    it "transforms a WiktData object into an Entry" $ False

mockSingle :: B.ByteString
mockSingle = "{\"word\": \"test\", \"pronunciations\": [{\"accent\":[\"RP\"], \"ipa\": [[\"en\", \"/tɛst\"]]}, \"senses\": [{\"glosses\": [\"this is a test with a tag\"], \"tags\": [\"testy\"]}, {\"glosses\": [\"this is a test with no tag\"]}], \"pos\":\"noun\"}"
