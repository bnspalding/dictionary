{-# LANGUAGE OverloadedStrings #-}

module DictionarySpec
  ( spec,
  )
where

import Dictionary
import Test.Hspec

spec :: Spec
spec = do
  describe "construction" $ do
    describe "makeEntry" $ do
      it "makes Entry from constituent components" $
        makeEntry
          "test"
          [ ("an evaluation", "n"),
            ("to evaluate", "v")
          ]
          ""
          `shouldBe` mockEntryMultiDef
      it "accepts empty arguments" $
        makeEntry
          ""
          []
          ""
          `shouldBe` Entry "" [] []
    describe "makeEntry1" $ do
      it "makes Entry from constituent components" $
        makeEntry1
          "test"
          "an evaluation"
          "n"
          ""
          `shouldBe` mockEntrySingleDef
      it "accepts empty arguments" $
        makeEntry1
          ""
          ""
          ""
          ""
          `shouldBe` Entry "" [] []
    describe "fromStringTuples" $ do
      it "creates a dictionary from text arguments" $
        size
          ( fromStringTuples
              [ ("read", "", "", ""),
                ("red", "", "", ""),
                ("reed", "", "", "")
              ]
          )
          `shouldBe` 3
      it "merges entries with the same representation" $
        size
          ( fromStringTuples
              [ ("test", "an evaluation", "n", ""),
                ("test", "to evaluate", "v", "")
              ]
          )
          `shouldBe` 1
    describe "fromList" $ do
      it "creates a dictionary from a list of Entries" $
        size
          ( fromList
              [ Entry "red" [] [],
                Entry "reed" [] [],
                Entry "read" [] []
              ]
          )
          `shouldBe` 3
      it "merges entries with the same representation" $
        size
          ( fromList
              [ mockEntrySingleDef,
                mockEntrySingleDef2
              ]
          )
          `shouldBe` 1
  describe "methods" $ do
    describe "first"
      $ it "provides first element of a dictionary"
      $ first mockDict `shouldBe` apple
    describe "last" $ it "has no tests" $ True `shouldBe` True
    describe "size" $ it "has no tests" $ True `shouldBe` True
    describe "next" $ it "has no tests" $ True `shouldBe` True
    describe "prev" $ it "has no tests" $ True `shouldBe` True
    describe "firstOfLetter" $ it "has no tests" $ True `shouldBe` True
    describe "contains" $ it "has no tests" $ True `shouldBe` True

mockEntrySingleDef :: Entry
mockEntrySingleDef = Entry "test" [mockDef1] []

mockEntrySingleDef2 :: Entry
mockEntrySingleDef2 = Entry "test" [mockDef2] []

mockEntryMultiDef :: Entry
mockEntryMultiDef = Entry "test" [mockDef1, mockDef2] []

mockDef1 :: Definition
mockDef1 = Definition "an evaluation" "n"

mockDef2 :: Definition
mockDef2 = Definition "to evaluate" "v"

mockDict :: Dictionary
mockDict =
  fromList
    [ cherry,
      banana,
      apple,
      durian,
      fig
    ]

apple :: Entry
apple = makeEntry1 "apple" "" "" ""

banana :: Entry
banana = makeEntry1 "banana" "" "" ""

cherry :: Entry
cherry = makeEntry1 "cherry" "" "" ""

durian :: Entry
durian = makeEntry1 "durian" "" "" ""

fig :: Entry
fig = makeEntry1 "fig" "" "" ""
