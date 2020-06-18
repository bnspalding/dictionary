{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: DictionaryIO
-- Description: reading and writing Dictionaries
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- DictionaryIO contains operations for writing a Dictionary to a file and
-- reading it at a later time. This is useful for filtering a dictionary to a
-- sub-dictionary once as a pre-process, and then being able to work with the
-- already reduced dictionary in the future. The Dictionary is written in [JSON
-- Lines format](http://jsonlines.org/), so it can also be hand-edited or worked over by some other
-- program.
module DictionaryIO
  ( readDictionary,
    writeDictionary,
  )
where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dictionary
import Sound.Pronunciation (makePronunciation)
import Sound.Word (symbols)

-- | readDictionary reads a Dictionary stored in JSON Line format at the given
-- filepath.
readDictionary :: FilePath -> IO Dictionary
readDictionary filepath = do
  jsonEntries <- C.lines <$> B.readFile filepath
  return $ fromList $ catMaybes $ readSingleEntry <$> jsonEntries

readSingleEntry :: B.ByteString -> Maybe Entry
readSingleEntry jsonEntry = do
  result <- decode jsonEntry
  flip parseMaybe result $ \o -> do
    _text <- o .: "text"
    _pron <- o .: "pronunciation"
    _defs' <- o .: "definitions" :: Parser [Value]
    let _defs = flip fmap _defs' $ \d ->
          flip parseMaybe d $ \dVal ->
            flip (withObject "Definition") dVal $ \dObj -> do
              _gloss <- dObj .: "gloss"
              _pos <- dObj .: "pos"
              _tags <- dObj .: "tags"
              return $ Definition _gloss _pos _tags
        _pronAsSounds = fromMaybe [] (makePronunciation _pron)
    return $ Entry _text (Set.fromList (catMaybes _defs)) _pronAsSounds

-- | writeDictionary writes a Dictionary in JSON Line format at the given
-- filepath.
writeDictionary :: FilePath -> Dictionary -> IO ()
writeDictionary filepath d = do
  let jsonEntries = entryToJSON <$> toList d
  TIO.writeFile filepath $ T.unlines jsonEntries

-- I cannot get Data.Aeson to do what I want, so we're just doing this ourselves
-- for now
entryToJSON :: Entry -> T.Text
entryToJSON e =
  mkObj
    [ ("text", quote $ text e),
      ("pronunciation", quote pronString),
      ("definitions", mkArr (definitionToJSON <$> Set.toList (definitions e)))
    ]
  where
    enclose a b t = a <> t <> b
    mkObj ps = enclose "{" "}" $ T.intercalate ", " $ mkPair <$> ps
    quote = enclose "\"" "\""
    mkPair (k, v) = quote k <> ": " <> v
    mkArr objs = enclose "[" "]" $ T.intercalate ", " objs
    pronString = symbols (pronunciation e)
    definitionToJSON d =
      mkObj
        [ ("gloss", quote . escapeQuotes $ gloss d),
          ("pos", quote $ pos d),
          ("tags", mkArr (quote . escapeQuotes <$> tags d))
        ]

escapeQuotes :: T.Text -> T.Text
escapeQuotes = T.replace "\"" "\\\""
