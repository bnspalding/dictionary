{-# LANGUAGE OverloadedStrings #-}

module DictionaryTestData where

import qualified Data.Set as Set
import Dictionary

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

caseMockDictA :: Dictionary
caseMockDictA =
  fromList
    [ apple,
      bigAcorn,
      banana
    ]

caseMockDictB :: Dictionary
caseMockDictB =
  fromList
    [ apple,
      bigArrow,
      banana
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

bigAcorn :: Entry
bigAcorn = Entry "Acorn" Set.empty []

bigArrow :: Entry
bigArrow = Entry "Arrow" Set.empty []
