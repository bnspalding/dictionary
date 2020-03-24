{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Wiktionary
-- Description: extracted JSONL to Dictionary
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
-- "Dictionary" for more information. When there are multiple pronunciations for
-- a word, the General American accent pronunciation (if it exists) will be
-- preferred.
module Wiktionary
  ( -- * Types
    WiktData (..),
    WiktSense (..),
    WiktPron (..),

    -- * From JSONL
    readJSONSingle,
    readJSONL,

    -- * To Dictionary
    makeDictionary,
    wiktDataToEntry,
  )
where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either
import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import qualified Dictionary as D (Dictionary, Entry, fromList, makeEntry)

-- | WiktData represents a useful subset of the information that describes a word on
-- Wiktionary. An entry in Wiktionary is a single word (written form) and part
-- of speech together with a set of meanings (senses) and a set of
-- pronunciations.
--
-- Rather than make WiktData an instance of FromJSON, parsing happens in
-- "readJSONSingle". This is because of some weird parsing that has to happen
-- for pronunciations (which lack a consistent schema), and it just made sense
-- to do the rest of the parsing there as well (for consistency).
--
-- NOTE: many wiktionary entries lack pronunciations. It may be better to make
-- pronunciations optional
data WiktData
  = WiktData
      { word :: T.Text,
        pos :: T.Text,
        senses :: [WiktSense],
        pronunciations :: [WiktPron]
      }
  deriving (Eq, Show)

-- | WiktSense represents a meaning for a word. The fields here are a selective
-- subset of what comes with the original data. The gloss is the written meaning
-- or definition that describes the sense. Tags are a list of qualifiers and
-- categorical descriptors for the sense (such as \'colloqial\', \'present\', or
-- \'chemistry\').
data WiktSense
  = WiktSense
      { gloss :: T.Text,
        tags :: [T.Text]
      }
  deriving (Eq, Show)

-- | WiktPron represents a pronunciation for a word. While there are several
-- different representations used on Wiktionary for a pronunciation (IPA, enPR),
-- the parsed results here are limited to IPA representations. Each
-- pronunciation is associated with one or more accents (GenAm, Canadian,
-- Received Pronunciation).
--
-- Note: not all wiktionary entries have an accent associated with the
-- pronunciation. When an accent is not present, it is assumed to be "GenAm"
-- (this choice is convenience-based).
data WiktPron
  = WiktPron
      { accent :: [T.Text],
        ipa :: T.Text
      }
  deriving (Eq, Show)

-- | readJSONSingle expects a single JSON object (as a ByteString) and parses it
-- as "WiktData" (wrapped in an Either, where Left reports on a failed parse)
readJSONSingle :: B.ByteString -> Either String WiktData
readJSONSingle input = do
  result <- eitherDecode input
  flip parseEither result $ \o -> do
    _word <- o .: "word"
    _pos <- o .: "pos"
    _senses' <- o .: "senses" :: Parser [Value]
    let _senses = flip fmap _senses' $ \s ->
          flip parseEither s $ \sVal ->
            flip (withObject "WiktSense") sVal $ \sObj -> do
              _glosses <- sObj .: "glosses"
              -- tags are an optional field. An empty list is used when none are
              -- present
              _tags <- sObj .:? "tags" .!= []
              -- glosses are "usually only one" (wiktextract)
              return $ WiktSense (head _glosses) _tags
    _pronunciations' <- o .: "pronunciations" :: Parser [Value]
    let _pronunciations = flip fmap _pronunciations' $ \p ->
          flip parseEither p $ \pVal ->
            flip (withObject "WiktPron") pVal $ \pObj -> do
              _ipa' <- pObj .: "ipa" :: Parser [[T.Text]]
              _accent <- pObj .:? "accent" .!= ["GenAm"]
              -- ipa in JSON has structure [["en", "ipa"]] (see [1])
              -- using (!!) because I want to see failures when they happen
              let _ipaMaybe = find ((== "en") . head) _ipa'
              if isNothing _ipaMaybe
                then fail "no en pronunciations"
                else return $ WiktPron _accent ((!! 1) (fromJust _ipaMaybe))
    -- Note: we filter out senses and pronunciations that fail to parse using
    -- Data.Either.rights
    let checkedProns = rights _pronunciations
    if null checkedProns
      then fail $ "no appropriate pronunciations for " ++ T.unpack _word
      else return $ WiktData _word _pos (rights _senses) (rights _pronunciations)

-- Note [1]: the "en" distinguishes an anglicised pronunciation from, for
-- example, Irish Gaelic ("ga"). See https://en.wiktionary.org/wiki/Ceann_Comhairle

-- | readJSONL expects a set of line-separated JSON objects (as a ByteString)
-- and parses each one into a WiktData object. See "readJSONSingle".
readJSONL :: B.ByteString -> [Either String WiktData]
readJSONL = fmap readJSONSingle . C.lines

-- | makeDictionary constructs a "Dictionary" out of list of WiktData objects
makeDictionary :: [WiktData] -> D.Dictionary
makeDictionary = D.fromList . fmap wiktDataToEntry

-- | wiktDataToEntry transforms a WiktData object into an Entry.
--
-- Because an Entry takes a single text, pronunciation pair and a set of
-- definitions (gloss, pos), a single pronunciation is chosen (GenAm if it
-- exists) and the pos is mapped across all glosses.
wiktDataToEntry :: WiktData -> D.Entry
wiktDataToEntry w = D.makeEntry (word w) defs (selectPron $ pronunciations w)
  where
    defs = (\s -> (gloss s, pos w, tags s)) <$> senses w

-- TODO: rewrite this. It's not selecting as expected, because pronunciations
-- are both {accent: RP, ipa: [[en, pron]]} and {accent [genam, canadian], ipa
-- [[en, genam_ipa], [en, canadian_ipa]]} (also remember that non "en"
-- pronunciations are being filtered out in parsing. However, other entries
-- separate US and UK (for example) as different objects (see "aircraft"). Be
-- prepared to handle both formats.
-- use (ipas !!) <$> findIndex (Genam ... to get either Genam pron or nothing
-- from index associated lists
selectPron :: [WiktPron] -> T.Text
selectPron ps = ipa $ head $ catMaybes [genAm, genAm', us, def]
  where
    genAm = find (elem "GenAm" . accent) ps
    genAm' = find (elem "GA" . accent) ps
    us = find (elem "US" . accent) ps
    -- add more "find ..." as needed
    def = Just (head ps)
--NOTE: There are approximately 860,000 entries in wiktionary that do not have
--pronunciation fields (around 70,000 that do). Consider pulling in something
--like CMUinIPA to fill in some of those cases where there's no pronunciation.
--
--NOTE: Wiktionary pronunciations are a mess. See words with multiple prons, ex.
--"dictionary", where a single pronunciation object has {accents:
--["uk", "us"], ipa: [["en", "uk_pronunciation"], ["en", "us_pronunciation"]]},
--where the index of the accent corresponds to the index of the ipa
--pronunciation. This differs from "RP", which comes as a separate object in the
--pronunciation list.
-- TODO: more symbols need to be added to Sound ipa symbol parsing (such as '/')
