{-# LANGUAGE DeriveGeneric #-}

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
    fromJSON,
  )
where

import qualified Data.Text as T
import Dictionary
import GHC.Generics

-- TODO: write WiktData record type
data WiktData
  = WiktData
      { word :: T.Text,
        pos :: T.Text,
        senses :: WiktSense,
        pronunciations :: [WiktPron]
      }
  deriving (Generic, Show)

data WiktSense = WiktSense {glosses :: [T.Text]} deriving (Generic, Show)

data WiktPron = WiktPron {ipa :: T.Text} deriving (Generic, Show)

fromJSON :: [WiktData] -> Dictionary
fromJSON = undefined
