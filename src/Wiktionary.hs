{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Wiktionary
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Wiktionary provides tools for working with dictionary entries parsed from
-- Wiktionary <https://www.wiktionary.org>. It expects data files that have
-- already been parsed from the wiktionary xml format (clean, filter, and
-- process your data in advance), and it converts the set of entries into a
-- "Dictionary". This module expects JSON data generated from something like
-- <https://github.com/tatuylonen/wiktextract>
--
-- A dictionary (in this package) maps a unique (text, pronunciation) pair with
-- a set of meanings. This is slightly different from the mappings between
-- representation and meaning used by Wiktionary, which means that the object
-- set that comprises the dictionary will be slightly different.
--
-- Pronunciations are interpreted using the General American English accent (See
-- "Dictionary" for more information. When processing data, be sure to pre-select
-- a single pronunciation for each word in your data.
module Wiktionary
  ( WiktData,
    readJSONSingle,
  )
where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dictionary
import GHC.Generics
import System.Environment

-- TODO: write WiktData record type to actually fit with JSONL format
data WiktData
  = WiktData
      { word :: T.Text,
        pos :: T.Text,
        senses :: [WiktSense],
        pronunciations :: [WiktPron]
      }
  deriving (Show)

-- Note: I have not yet figured out why glosses is a list. There seems to be one
-- gloss per sense in the small subset of entries I have examined thus far.
newtype WiktSense = WiktSense {glosses :: [T.Text]} deriving (Generic, Show)

instance FromJSON WiktSense

-- Note: ipa is a double list because it sometimes looks like
-- "[["en", "/pɔːɹtˈmæntoʊ/"]]". This will need to be cleaned up.
newtype WiktPron = WiktPron {ipa :: [[T.Text]]} deriving (Show)

-- Note: this probably won't work without thinking about UTF8 and char ecodings
readJSONSingle :: B.ByteString -> Either String WiktData
readJSONSingle input = do
  result <- eitherDecode input
  flip parseEither result $ \o -> do
    word <- o .: "word"
    pos <- o .: "pos"
    senses <- o .: "senses"
    _pronunciations <- o .: "pronunciations" :: Parser [Value]
    let pronunciations = flip fmap _pronunciations $ \p ->
          flip parseEither p $ \pVal ->
            flip (withObject "WiktPron") pVal $ \pObj -> do
              ipa <- pObj .: "ipa"
              return $ WiktPron ipa
    return $ WiktData word pos senses (rights pronunciations)

-- JSONL needs to be split into lines for Aeson to parse it
readJSONL :: B.ByteString -> [Either String WiktData]
readJSONL = fmap readJSONSingle . C.lines

-- These are just for testing in ghci right now
wiktData :: IO B.ByteString
wiktData = B.readFile =<< getEnv "WIKTDATA_UTF8"

wFirst :: IO (Either String WiktData)
wFirst = readJSONSingle . head . C.lines <$> wiktData

-- Note: unicode encoding is working fine. You just need to use TIO for it to
-- display properly (as opposed to the way it works in show)
printPron :: IO ()
printPron =
  TIO.putStrLn . either T.pack ((!! 1) . head . ipa . head . pronunciations) =<< wFirst
