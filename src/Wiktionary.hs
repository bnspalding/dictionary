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

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as TIO -- testing only
import Dictionary
import System.Environment -- testing only

--Note: rather than defining fromJSON, we parse in readJSONSingle because
--there's some weird parsing for senses and pronunciations, so it's just easier
--to parse there rather than use two different methods (parse vs fromJSON)
data WiktData
  = WiktData
      { word :: T.Text,
        pos :: T.Text,
        senses :: [WiktSense],
        pronunciations :: [WiktPron]
      }
  deriving (Show)

data WiktSense
  = WiktSense
      { gloss :: T.Text,
        tags :: [T.Text]
      }
  deriving (Show)

data WiktPron
  = WiktPron
      { accent :: [T.Text],
        ipa :: T.Text
      }
  deriving (Show)

readJSONSingle :: B.ByteString -> Either String WiktData
readJSONSingle input = do
  result <- eitherDecode input
  flip parseEither result $ \o -> do
    word <- o .: "word"
    pos <- o .: "pos"
    _senses <- o .: "senses" :: Parser [Value]
    let senses = flip fmap _senses $ \s ->
          flip parseEither s $ \sVal ->
            flip (withObject "WiktSense") sVal $ \sObj -> do
              glosses <- sObj .: "glosses"
              -- tags are an optional field. An empty list is used when none are
              -- present
              tags <- sObj .:? "tags" .!= []
              -- glosses are "usually only one" (wiktextract)
              return $ WiktSense (head glosses) tags
    _pronunciations <- o .: "pronunciations" :: Parser [Value]
    let pronunciations = flip fmap _pronunciations $ \p ->
          flip parseEither p $ \pVal ->
            flip (withObject "WiktPron") pVal $ \pObj -> do
              _ipa <- pObj .: "ipa"
              accent <- pObj .: "accent"
              -- ipa in JSON has structure [["en", "ipa"]] (weird)
              -- using (!!) because I want to see failures when they happen
              let ipa = (!! 1) . head $ _ipa
              return $ WiktPron accent ipa
    -- Note: we filter out senses and pronunciations that fail to parse using
    -- Data.Either.rights
    return $ WiktData word pos (rights senses) (rights pronunciations)

-- JSONL needs to be split into lines for Aeson to parse it
readJSONL :: B.ByteString -> [Either String WiktData]
readJSONL = fmap readJSONSingle . C.lines

-- These are just for testing in ghci right now ------------------------
wiktData :: IO B.ByteString
wiktData = B.readFile =<< getEnv "WIKTDATA_UTF8"

wFirst :: IO (Either String WiktData)
wFirst = readJSONSingle . head . C.lines <$> wiktData

-- Note: unicode encoding is working fine. You just need to use TIO for it to
-- display properly (as opposed to the way it works in show)
printPron :: IO ()
printPron =
  TIO.putStrLn . either T.pack (ipa . head . pronunciations) =<< wFirst
