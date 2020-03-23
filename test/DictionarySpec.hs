{-# LANGUAGE OverloadedStrings #-}

module DictionarySpec
  ( spec,
  )
where

import qualified Data.Set as Set
import Dictionary
import Test.Hspec

spec :: Spec
spec = do
  describe "construction" $ do
    describe "makeEntry" $ do
      it "makes Entry from constituent components" $
        makeEntry
          "test"
          [ ("an evaluation", "n", []),
            ("to evaluate", "v", [])
          ]
          ""
          `shouldBe` mockEntryMultiDef
      it "accepts empty arguments" $
        makeEntry
          ""
          []
          ""
          `shouldBe` Entry "" Set.empty []
    describe "fromList" $ do
      it "creates a dictionary from a list of Entries" $
        size
          ( fromList
              [ Entry "red" Set.empty [],
                Entry "reed" Set.empty [],
                Entry "read" Set.empty []
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
      it "joins definitions by union for the same representation" $
        Set.size
          ( definitions $
              first
                ( fromList
                    [ mockEntrySingleDef,
                      mockEntrySingleDef2
                    ]
                )
          )
          `shouldBe` 2
      it "eliminates duplicate definitions within an entry" $
        Set.size
          ( definitions $
              first
                ( fromList
                    [ mockEntryMultiDef,
                      mockEntryMultiDef
                    ]
                )
          )
          `shouldBe` 2
  describe "methods" $ do
    describe "first"
      $ it "provides first element of a dictionary"
      $ first mockDict `shouldBe` apple
    describe "final"
      $ it "provides the final element of a dictionary"
      $ final mockDict `shouldBe` fig
    describe "size"
      $ it "provides the number of elements in a dictionary"
      $ size mockDict `shouldBe` 5
    describe "next" $ do
      it "provides the next entry (alphabetically) in a dictionary" $
        next mockDict apple `shouldBe` Just banana
      it "returns Nothing when given the last element" $
        next mockDict fig `shouldBe` Nothing
    describe "prev" $ do
      it "provides the previous entry (alphabetically) in a dictionary" $
        prev mockDict cherry `shouldBe` Just banana
      it "returns Nothing when given the first element" $
        prev mockDict apple `shouldBe` Nothing
    describe "firstOfLetter" $ do
      it "provides the first element of a given character (a-z)" $
        firstOfLetter mockDict 'd' `shouldBe` Just durian
      it "returns Nothing when no elements of the given character exist" $
        firstOfLetter mockDict 'z' `shouldBe` Nothing
    describe "contains" $ do
      it "returns True when an entry is present in the dictionary" $
        contains mockDict cherry `shouldBe` True
      it "returns False when an entry is not present in the dictionary" $
        contains mockDict zebra `shouldBe` False
    describe "lookupText" $ do
      it "provides a list of entries whose text match the given text" $
        lookupText mockDict "fig" `shouldBe` Just [fig]
      it "returns Nothing when no elements match" $
        lookupText mockDict "pig" `shouldBe` Nothing
    describe "lookupPron" $ do
      -- I don't want to import or mock Sound, so...
      it "provides a list of entries whose pronunciation match the given pron" $
        lookupPron mockDict [] `shouldBe` Just (toList mockDict)
      it "returns Nothing when no elements match" $
        lookupPron (fromList []) [] `shouldBe` Nothing
    describe "toList"
      $ it "converts a dictionary to a list"
      $ toList mockDict `shouldBe` [apple, banana, cherry, durian, fig]
  describe "sub-dictionaries" $ do
    describe "subDict" $ do
      it "provides a sub-dictionary based on some entry predicate" $
        subDict mockDict (\e -> text e == "apple") `shouldBe` fromList [apple]
      it "provides the entire dictionary when given a tautology" $
        subDict mockDict (const True) `shouldBe` mockDict
      it "provides an empty dictionary when given a contradiction" $
        subDict mockDict (const False) `shouldBe` fromList []
    describe "subPOS" $ do
      it "provides a sub-dictionary based on a given part of speech" $
        subPOS (fromList [apple, mockEntrySingleDef]) "n"
          `shouldBe` fromList [mockEntrySingleDef]
      it "provides an empty dictionary when no matches are present" $
        subPOS mockDict "not a part of speech" `shouldBe` fromList []
    describe "subXTags" $ do
      it "provides a sub-dictionary where entries are filtered on tags" $
        subXTags (fromList [zebra, trumpet]) ["animal", "other tag"]
          `shouldBe` fromList [trumpet]
      it "provides an empty dictionary when no matches are present" $
        subXTags (fromList [zebra]) ["animal"] `shouldBe` fromList []

mockEntrySingleDef :: Entry
mockEntrySingleDef = Entry "test" (Set.singleton mockDef1) []

mockEntrySingleDef2 :: Entry
mockEntrySingleDef2 = Entry "test" (Set.singleton mockDef2) []

mockEntryMultiDef :: Entry
mockEntryMultiDef = Entry "test" (Set.fromList [mockDef1, mockDef2]) []

mockDef1 :: Definition
mockDef1 = Definition "an evaluation" "n" []

mockDef2 :: Definition
mockDef2 = Definition "to evaluate" "v" []

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
apple = Entry "apple" Set.empty []

banana :: Entry
banana = Entry "banana" Set.empty []

cherry :: Entry
cherry = Entry "cherry" Set.empty []

durian :: Entry
durian = Entry "durian" Set.empty []

fig :: Entry
fig = Entry "fig" Set.empty []

zebra :: Entry
zebra = Entry "zebra" (Set.singleton (Definition "a test zebra" "n" ["animal"])) []

trumpet :: Entry
trumpet = Entry "trumpet" (Set.singleton (Definition "a test trumpet" "n" [])) []
