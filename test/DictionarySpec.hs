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
        makeEntry "test" [("an evaluation", "n"), ("to evaluate", "v")] ""
          `shouldBe` mockEntryMultiDef
      it "accepts empty arguments" $
        makeEntry "" [] "" `shouldBe` Entry "" [] []
    describe "makeEntry1" $ do
      it "makes Entry from constituent components" $
        makeEntry1 "test" "an evaluation" "n" "" `shouldBe` mockEntrySingleDef
      it "accepts empty arguments" $
        makeEntry1 "" "" "" "" `shouldBe` Entry "" [] []
    describe "fromStringTuples" $ it "has no tests" $ True `shouldBe` True
    describe "fromList" $ it "has no tests" $ True `shouldBe` True
  describe "methods" $ do
    describe "first" $ it "has no tests" $ True `shouldBe` True
    describe "last" $ it "has no tests" $ True `shouldBe` True
    describe "size" $ it "has no tests" $ True `shouldBe` True
    describe "next" $ it "has no tests" $ True `shouldBe` True
    describe "prev" $ it "has no tests" $ True `shouldBe` True
    describe "firstOfLetter" $ it "has no tests" $ True `shouldBe` True
    describe "contains" $ it "has no tests" $ True `shouldBe` True

mockEntrySingleDef :: Entry
mockEntrySingleDef = Entry "test" [mockDef1] []

mockEntryMultiDef :: Entry
mockEntryMultiDef = Entry "test" [mockDef1, mockDef2] []

mockDef1 :: Definition
mockDef1 = Definition "an evaluation" "n"

mockDef2 :: Definition
mockDef2 = Definition "to evaluate" "v"
