-- |
-- Module: Dictionary
-- Description: a set of lexical entries
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- A Dictionary is a set of lexical entries. It associates a ('text', 'pronunciation')
-- pair with set of 'Definition's, where each association is an 'Entry'. A
-- Dictionary is ordered alphabetically by text.
module Dictionary
  ( -- * Types
    Dictionary,
    Entry (..),
    Definition (..),

    -- * Construction
    makeEntry,
    fromList,

    -- * Get Entries
    first,
    firstOfLetter,
    final,
    lookupText,
    lookupPron,
    next,
    prev,

    -- * Other
    contains,
    size,
    toList,

    -- * Sub-Dictionaries

    --
    -- $filtering
    subDict,
    subDictTrim,
    subPOS,
    subXTags,
  )
where

import qualified Data.Char as C
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Sound.Pronunciation (Pronunciation, makePronunciation)

-- | A Dictionary is a Map of Representations to Definitions. Use Entries when
-- working with a dictionary, not Reps.
--
-- A map is used to make sense of merging definitions to a representation. All
-- dictionary methods wrap inputs and outputs in Entries, so there should never
-- be a need to work with the internal Rep, {Definition} map representation.
type Dictionary = Map.Map Rep (Set.Set Definition)

-- | Rep (short for Repesentation) is a (text, pronunciation) pair. Together
-- with a set of definitions, it forms a dictionary entry. Most of the time, you
-- should be working with Entries, rather than Reps.
--
-- For example,
-- (\"read\", \"ɹid\") is a unique pair separate from (\"read\", \"ɹɛd\"), (\"reed\",
-- \"ɹid\"), and (\"red\", \"ɹɛd\").
data Rep = Rep
  { _text :: T.Text,
    _pron :: Pronunciation
  }
  deriving (Eq, Show)

-- | Rep is sorted by its text component (alphabetically, like a dictionary)
instance Ord Rep where
  compare r1 r2 = compare (T.toLower (_text r1)) (T.toLower (_text r2))

-- | A Definition is a meaning, an ontological unit of a dictionary.
data Definition = Definition
  { -- | gloss gives the textual definition for a word
    gloss :: T.Text,
    -- | pos gives the part of speech tied to the definition
    pos :: T.Text,
    tags :: [T.Text]
  }
  deriving (Eq, Ord, Show)

-- | an Entry associates a (text, pronunciation) pair with a set of unique
-- definitions. The structure of an Entry is how we should think about the data,
-- even though the underlying representation in the map is slightly different.
-- All Dictionary methods return Entries, not (rep, def) pairs.
--
-- Entries are compared based on their text, and they are evaluated for equality
-- (uniqueness) based on their (text, pronunciation) pair.
data Entry = Entry
  { text :: T.Text,
    definitions :: Set.Set Definition, -- gloss, pos
    pronunciation :: Pronunciation -- same as Sound.Word, i.e. [Syl]
  }
  deriving (Show)

-- | Entries are ordered based on their text (alphabetically)
instance Ord Entry where
  compare e1 e2 = compare (text e1) (text e2)

-- | Entries are unique based on their (text, pronunciation) pair
instance Eq Entry where
  e1 == e2 = text e1 == text e2 && pronunciation e1 == pronunciation e2

-- | fromList generates a Dictionary from a list of Entries. Definitions are
-- merged under unique (text, pronunciation) pairs.
fromList :: [Entry] -> Dictionary
fromList es = Map.fromListWith Set.union (_toInternal <$> es)

-- | first provides the first element of the Dictionary (by text, ascending)
first :: Dictionary -> Entry
first = _entry . Map.elemAt 0

-- | final provides the final element of the Dictionary (by text, ascending)
final :: Dictionary -> Entry
final d = _entry $ Map.elemAt (size d - 1) d

-- | the size of the dictionary. See "Data.Map".'Map.size'
size :: Dictionary -> Int
size = Map.size

-- | next gives the next element of the Dictionary (alphabetically).
-- next returns Nothing when it is the last entry in the dicitionary.
next :: Entry -> Dictionary -> Maybe Entry
next entry d =
  let mockR = Rep {_text = text entry, _pron = []}
   in _entry <$> Map.lookupGT mockR d

-- | prev gives the previous element of the Dictionary (alphabetically).
-- prev returns Nothing when it is the first entry in the dictionary.
prev :: Entry -> Dictionary -> Maybe Entry
prev entry d =
  let mockR = Rep {_text = text entry, _pron = []}
   in _entry <$> Map.lookupLT mockR d

-- | contains reports whether the Dictionary contains a (text, pronunciation)
-- entry. Note that because equality for Entries is based only on (text,
-- pronunciation), you can construct a mock entry containing just these
-- elements.
contains :: Entry -> Dictionary -> Bool
contains entry = Map.member (_toRep entry)

-- | lookupText provides a list of Entries whose text element matches the
-- provided text. If there are no matches, Nothing is returned.
lookupText :: T.Text -> Dictionary -> Maybe [Entry]
lookupText target = _maybe . toList . subDict (\e -> text e == target)

-- | lookupPron provides a list of Entries whose pronunciation element matches
-- the provided pronunciation. If there are no matches, Nothing is returned.
lookupPron :: Pronunciation -> Dictionary -> Maybe [Entry]
lookupPron target = _maybe . toList . subDict (\e -> pronunciation e == target)

_maybe :: [a] -> Maybe [a]
_maybe xs =
  if null xs
    then Nothing
    else Just xs

-- $filtering
-- Sub-dictionaries can be constructed to filter a set of entries according to
-- some predicate. 'subDict' can produce arbitrary sub-dictionaries according to
-- a given predicate for entries to satisfy. 'subXTags' and 'subPOS' are
-- convenience functions built on top of 'subDict'.
--
-- To filter a dictionary by word:
--
-- > subDict d (`elem` ["my filter words"] . text)
--
-- By Definitions: (at least one definition is clear of filter words)
--
-- > subDict d ( any (null . `intersect` ["my filter words"] . words) . Set.toList . definitions)
--
-- alternatively, use all instead of any or isInfixOf instead of intersect .
-- words to make this more strict

-- | subDict filters the entries of a dictionary and returns the sub-dictionary
-- of matching entries
subDict :: (Entry -> Bool) -> Dictionary -> Dictionary
subDict f = Map.filterWithKey (\k v -> f (_entry (k, v)))

-- | subDictTrim removes definitions from entries in a dictionary. An entry that
-- has had all of its definitions removed is not returned as part of the
-- sub-dictionary. For example,
--
-- > subDictTrim d ((== "n") . pos)
--
-- will produce a subdictionary of d where all definitions with parts of speech
-- other than "n" will be removed, and all entries without definitions with part
-- of speech "n" will be removed.
subDictTrim :: (Definition -> Bool) -> Dictionary -> Dictionary
subDictTrim f =
  fromList
    . Map.foldrWithKey -- using foldr as a map + filter
      ( \k v acc ->
          let e = _entry (k, trim v)
           in if not (empty e) then e : acc else acc
      )
      []
  where
    empty = Set.null . definitions
    trim = Set.filter f

-- | subPOS provides the sub-dictionary where at least one definition for each
-- entry is the given POS. For example,
--
-- > subPOS d "n"
--
-- will produce a subdictionary of d where each word has at least one definition with
-- part of speech "n"
--
-- Note: this does not filter out definitions that do not match the given part
-- of speech from each entry in the sub-dictionary. So an entry with multiple
-- definitions will retain all of its definitions when it is reproduced in the
-- sub-dictionary, even if only a subset of those definitions match the given
-- part of speech. Use 'subDictTrim' to actually remove definitions from an
-- entry while keeping the entry.
subPOS :: T.Text -> Dictionary -> Dictionary
subPOS target =
  subDict (\e -> any (\def -> pos def == target) (Set.toList $ definitions e))

-- | subXTags provides a sub-dictionary where an entry has been removed if all
-- of its definitions contain at least one of the given tags. For example,
--
--  > subXTags d ["informal", "humorous", "offensive"]
--
--  will produce a subdictionary of d where each word has at least one
--  definition that does not contain any of the supplied tags.
--
--  Note: this does not filter out definitions that contain the tags, so long as
--  there is at least one other definition for the word that does not contain
--  the tags. Use 'subDictTrim' to actually remove definitions from an entry
--  while keeping the entry.
subXTags :: [T.Text] -> Dictionary -> Dictionary
subXTags xTags =
  subDict
    ( \e ->
        not $
          all
            ( \def ->
                any (`elem` tags def) xTags
            )
            (Set.toList $ definitions e)
    )

-- | toList converts a Dictionary to a list of Entries.
toList :: Dictionary -> [Entry]
toList = fmap _entry . Map.toList

-- becase lookupGE returns the first entry GE an entry with text "c"
-- it is necessary to ensure the entry actually starts with c
-- otherwise, firstOfLetter d 's' could return entry with text "t..."
-- for a dictionary with no entries beginning with 's'

-- | firstOfLetter provides the first entry whose text begins with a given
-- letter. If there are no entries in the Dictionary that begin with the given
-- letter, Nothing is returned.
firstOfLetter :: Char -> Dictionary -> Maybe Entry
firstOfLetter c d =
  let mockE = Rep {_text = T.singleton (C.toLower c), _pron = []}
      maybeE = _entry <$> Map.lookupGE mockE d
   in case maybeE of
        Nothing -> Nothing
        Just e ->
          if T.head (T.toLower (text e)) == C.toLower c
            then Just e
            else Nothing

-- | makeEntry constructs an Entry out of its constituent components. makeEntry
-- takes a list of definitions as its second argument.
makeEntry :: T.Text -> [(T.Text, T.Text, [T.Text])] -> T.Text -> Entry
makeEntry _text _defs _pronString =
  Entry _text defs pron
  where
    defs = Set.fromList $ (\(g, p, ts) -> Definition g p ts) <$> _defs
    pron = fromMaybe [] (makePronunciation _pronString)

_entry :: (Rep, Set.Set Definition) -> Entry
_entry (r, ds) = Entry (_text r) ds (_pron r)

_toInternal :: Entry -> (Rep, Set.Set Definition)
_toInternal e = (_toRep e, definitions e)

_toRep :: Entry -> Rep
_toRep e = Rep {_text = text e, _pron = pronunciation e}
