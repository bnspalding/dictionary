{-# LANGUAGE OverloadedStrings #-}

module GHCI where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either (rights)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dictionary
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

testIPA :: IO [Entry]
testIPA = toList . flip subXTags filterTags . makeDictionary . rights . readJSONL <$> wiktData

filterTags :: [T.Text]
filterTags = ["offensive", "derogatory", "informal", "Singlish", "humorous"]
--NOTE: There are approximately 860,000 entries in wiktionary that do not have
--pronunciation fields (around 70,000 that do). Consider pulling in something
--like CMUinIPA to fill in some of those cases where there's no pronunciation.
