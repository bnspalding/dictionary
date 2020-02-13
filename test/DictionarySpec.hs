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
    describe "last"
      $ it "provides the last element of a dictionary"
      $ Dictionary.last mockDict `shouldBe` fig
    describe "size"
      $ it "provides the number of elements in a dictionary"
      $ size mockDict `shouldBe` 5
    describe "next" $ do
      it "provides the next entry (alphabetically) in a dictionary" $
        next mockDict apple `shouldBe` banana
      it "wraps around when given the last element" $
        next mockDict fig `shouldBe` apple
    describe "prev" $ do
      it "provides the previous entry (alphabetically) in a dictionary" $
        prev mockDict cherry `shouldBe` banana
      it "wraps around when given the first element" $
        prev mockDict apple `shouldBe` fig
    describe "firstOfLetter" $ do
      it "provides the first element of a given character (a-z)" $
        firstOfLetter mockDict 'd' `shouldBe` Just durian
      it "returns Nothing when no elements of the given character exist" $
        firstOfLetter mockDict 'z' `shouldBe` Nothing
    describe "contains" $ do
      it "returns True when an entry is present in the dictionary" $
        contains mockDict cherry `shouldBe` True
      it "returns False when an entry is not present in the dictionary" $
        contains mockDict (makeEntry1 "zebra" "" "" "") `shouldBe` False

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
      fig,
      apple,
      durian
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
