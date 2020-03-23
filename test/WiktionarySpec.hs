{-# LANGUAGE OverloadedStrings #-}

module WiktionarySpec
  ( spec,
  )
where

import qualified Data.ByteString.Lazy as B
import Data.Either (rights)
import qualified Data.Text.Lazy.Encoding as TE
import qualified Dictionary as D
import Test.Hspec
import Wiktionary

spec :: Spec
spec = do
  describe "readJSONL"
    $ it "parses JSONL data into WiktData"
    $ rights (readJSONL mockMulti) `shouldBe` mockWiktDMulti
  describe "readJSONSingle"
    $ it "parses a JSON object int a WiktData object"
    $ readJSONSingle mockSingle `shouldBe` Right mockWiktDSingle
  describe "makeDictionary"
    $ it "constructs a Dictionary from a set of WiktData objects"
    $ makeDictionary mockWiktDMulti `shouldBe` mockDict
  describe "wiktDataToEntry"
    $ it "transforms a WiktData object into an Entry"
    $ wiktDataToEntry mockWiktDSingle `shouldBe` mockEntry

mockSingle :: B.ByteString
mockSingle = TE.encodeUtf8 "{\"word\": \"test\", \"pronunciations\": [{\"accent\":[\"RP\"], \"ipa\": [[\"en\", \"tɛst\"]]}], \"senses\": [{\"glosses\": [\"this is a test with a tag\"], \"tags\": [\"testy\"]}, {\"glosses\": [\"this is a test with no tag\"]}], \"pos\":\"noun\"}"

-- The third line (testC) contains a pronunciation without an ipa key
-- (badSchema). This should not be present in the WiktData object.
mockMulti :: B.ByteString
mockMulti =
  TE.encodeUtf8
    "{\"word\": \"testA\", \"pronunciations\": [{\"accent\":[\"RP\"], \"ipa\": [[\"en\", \"tɛst\"]]}], \"senses\": [{\"glosses\": [\"A:this is a test with a tag\"], \"tags\": [\"A:testy\"]}, {\"glosses\": [\"A:this is a test with no tag\"]}], \"pos\":\"noun\"}\n{\"word\": \"testB\", \"pronunciations\": [{\"accent\":[\"GenAm\", \"Canadian\"], \"ipa\": [[\"en\", \"tɛst\"]]}], \"senses\": [{\"glosses\": [\"B:this is a test with a tag\"], \"tags\": [\"B:testy\"]}, {\"glosses\": [\"B:this is a test with no tag\"]}], \"pos\":\"verb\"}\n{\"word\": \"testC\", \"pronunciations\": [{\"badSchema\":\"some data\"}, {\"accent\":[\"RP\"], \"ipa\": [[\"en\", \"tɛst\"]]}], \"senses\": [{\"glosses\": [\"C:this is a test with a tag\"], \"tags\": [\"C:some testy\"]}, {\"glosses\": [\"C:this is a test with no tag\"]}], \"pos\":\"noun\"}"

mockWiktDSingle :: WiktData
mockWiktDSingle =
  WiktData
    { word = "test",
      pos = "noun",
      senses =
        [ WiktSense
            { gloss = "this is a test with a tag",
              tags = ["testy"]
            },
          WiktSense
            { gloss = "this is a test with no tag",
              tags = []
            }
        ],
      pronunciations =
        [ WiktPron
            { accent = ["RP"],
              ipa = "tɛst"
            }
        ]
    }

mockWiktDMulti :: [WiktData]
mockWiktDMulti =
  [ WiktData
      { word = "testA",
        pos = "noun",
        senses =
          [ WiktSense
              { gloss = "A:this is a test with a tag",
                tags = ["A:testy"]
              },
            WiktSense
              { gloss = "A:this is a test with no tag",
                tags = []
              }
          ],
        pronunciations =
          [ WiktPron
              { accent = ["RP"],
                ipa = "tɛst"
              }
          ]
      },
    WiktData
      { word = "testB",
        pos = "verb",
        senses =
          [ WiktSense
              { gloss = "B:this is a test with a tag",
                tags = ["B:testy"]
              },
            WiktSense
              { gloss = "B:this is a test with no tag",
                tags = []
              }
          ],
        pronunciations =
          [ WiktPron
              { accent = ["GenAm", "Canadian"],
                ipa = "tɛst"
              }
          ]
      },
    WiktData
      { word = "testC",
        pos = "noun",
        senses =
          [ WiktSense
              { gloss = "C:this is a test with a tag",
                tags = ["C:some testy"]
              },
            WiktSense
              { gloss = "C:this is a test with no tag",
                tags = []
              }
          ],
        pronunciations =
          [ WiktPron
              { accent = ["RP"],
                ipa = "tɛst"
              }
          ]
      }
  ]

mockEntry :: D.Entry
mockEntry =
  D.makeEntry
    "test"
    [ ("this is a test with a tag", "noun", ["testy"]),
      ("this is a test with no tag", "noun", [])
    ]
    "tɛst"

mockDict :: D.Dictionary
mockDict =
  D.fromList
    [ ( D.makeEntry
          "testA"
          [ ("A:this is a test with a tag", "noun", ["A:testy"]),
            ("A:this is a test with no tag", "noun", [])
          ]
          "tɛst"
      ),
      ( D.makeEntry
          "testB"
          [ ("B:this is a test with a tag", "verb", ["B:testy"]),
            ("B:this is a test with no tag", "verb", [])
          ]
          "tɛst"
      ),
      ( D.makeEntry
          "testC"
          [ ("C:this is a test with a tag", "noun", ["C:some testy"]),
            ("C:this is a test with no tag", "noun", [])
          ]
          "tɛst"
      )
    ]
