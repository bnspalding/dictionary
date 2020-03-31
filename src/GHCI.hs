{-# LANGUAGE OverloadedStrings #-}

module GHCI where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either (rights)
import Data.List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dictionary
import DictionaryIO
import System.Environment
import Wiktionary

-- These are just for testing in ghci right now ------------------------

-- | wiktData is the JSONL formatted wiktionary data, read as a ByteString. The
-- data is read from the WIKTDATA_UTF8 environment variable.
wiktData :: IO B.ByteString
wiktData = B.readFile =<< getEnv "WIKTDATA_UTF8"

-- | wFirst is the first entry from wiktData, parsed
wFirst :: IO (Either String WiktData)
wFirst = readJSONSingle . head . C.lines <$> wiktData

-- | printPron prints the pronunciation for wFirst, demonstrating it handles
-- weird ipa unicode characters correctly.
printPron :: IO ()
printPron =
  TIO.putStrLn . either T.pack (ipa . head . pronunciations) =<< wFirst

wiktdict :: IO Dictionary
wiktdict =
  -- let source = makeDictionary . rights . readJSONL <$> wiktData
  let source = readDictionary "dict.jsonl"
   in filterWordsAndDefs =<< subXPOS . flip subXTags filterTags <$> source

testIPA :: IO [Entry]
testIPA = toList <$> wiktdict

filterTags :: [T.Text]
filterTags =
  [ "offensive",
    "derogatory",
    "informal",
    "spoken",
    "imitating Irish accent",
    "Singlish",
    "Braille",
    "humorous",
    "vulgar",
    "colloquial",
    "ethnic slur",
    "religious slur",
    "informal",
    "slang",
    "archaic",
    "rare",
    "obsolete",
    "Singapore",
    "medical",
    "legal",
    "plural",
    "pharmaceutical drug",
    "zoology",
    "medicine",
    "anatomy",
    "biochemistry",
    "law",
    "biology",
    "surgery",
    "dialect",
    "evolutionary theory",
    "past",
    "misspelling",
    "comparative",
    "superlative",
    "plural",
    "3",
    "dialectical",
    "grammar",
    "sciences",
    "geology",
    "nautical",
    "Scotland",
    "dated",
    "pharmaceutical effect",
    "ornithology",
    "electricity",
    "dental",
    "sometimes",
    "phonetics",
    "geography",
    "in Spanish contexts",
    "organizational theory",
    "business",
    "Islam",
    "organic chemistry",
    "organic compound",
    "chemistry",
    "physiology",
    "AAVE",
    "rhyming slang",
    "psychoanalysis",
    "UK",
    "British",
    "Australia",
    "mathematics",
    "linguistics",
    "Christianity",
    "philosophy",
    "political science",
    "physics",
    "optics",
    "person",
    "surname",
    "psychiatry",
    "historical",
    "outdated",
    "psychology",
    "abbreviation",
    "angling",
    "botany",
    "rhetoric",
    "pejorative",
    "pathology",
    "hematology",
    "alchemy",
    "astronomy",
    "cartography",
    "surveying",
    "nonstandard",
    "programming",
    "neologism",
    "immunology",
    "phonology",
    "dialectal",
    "Hawaii",
    "Indian cooking",
    "inorganic chemistry",
    "mycology",
    "oncology",
    "Jainism",
    "marine biology",
    "particle",
    "Liverpool",
    "possessive form",
    "Philippines",
    "religion",
    "biblical",
    "theology",
    "geometry",
    "fencing",
    "jargon",
    "Classical Philosophy",
    "enzyme",
    "computing",
    "rocketry",
    "game of go",
    "Hinduism",
    "Buddhism",
    "Vedanta",
    "science fiction",
    "music",
    "genetics"
  ]

filterPOSs :: [T.Text]
filterPOSs = ["name", "infix", "prefix", "suffix", "phrase"]

subXPOS :: Dictionary -> Dictionary
subXPOS = flip subDict $ \e ->
  not $
    all (\s -> Dictionary.pos s `elem` filterPOSs) (definitions e)

subStartsWith :: Dictionary -> Char -> Dictionary
subStartsWith d c = subDict d $ (== c) . T.head . text

filterWordsAndDefs :: Dictionary -> IO Dictionary
filterWordsAndDefs d = do
  filterFile <- getEnv "FILTERWORDS"
  filterList <- T.lines <$> TIO.readFile filterFile
  let filterDefs =
        flip
          subDict
          ( any
              (null . flip intersect filterList . T.words . Dictionary.gloss)
              . Set.toList
              . definitions
          )
      filterText = flip subDict (not . (`elem` filterList) . text)
  return . filterDefs . filterText $ d
