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
--
-- A special note about this representation of a dictionary: The 'next' and
-- 'prev' functions wrap around (such that the final element points to the first,
-- and the first element points to the final when using 'next' and 'prev'). This
-- decision comes from the way that Dictionary is used in Sequence, Selection
-- libraries farther up the ladder.
module Dictionary
  ( -- * Types
    Dictionary,
    Entry (..),
    Definition (..),

    -- * Construction
    makeEntry,
    fromList,

    -- * Methods
    first,
    final,
    size,
    next,
    prev,
    firstOfLetter,
    contains,
    lookupText,
    lookupPron,
    toList,

    -- * Sub-Dictionaries
    --
    -- $filtering
    subDict,
    subPOS,
    subXTags,
  )
where

import qualified Data.Map as Map
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
data Rep
  = Rep
      { _text :: T.Text,
        _pron :: Pronunciation
      }
  deriving (Eq, Show)

-- | Rep is sorted by its text component (alphabetically, like a dictionary)
-- TODO: rewrite Rep's compare to work on lower case versions of text (so that 'A' is
-- not separate from 'a'
instance Ord Rep where
  compare r1 r2 = compare (_text r1) (_text r2)

-- | A Definition is a meaning, an ontological unit of a dictionary.
data Definition
  = Definition
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
data Entry
  = Entry
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
next :: Dictionary -> Entry -> Maybe Entry
next d entry =
  let mockR = Rep {_text = text entry, _pron = []}
   in _entry <$> Map.lookupGT mockR d

-- | prev gives the previous element of the Dictionary (alphabetically).
-- prev returns Nothing when it is the first entry in the dictionary.
prev :: Dictionary -> Entry -> Maybe Entry
prev d entry =
  let mockR = Rep {_text = text entry, _pron = []}
   in _entry <$> Map.lookupLT mockR d

-- | contains reports whether the Dictionary contains a (text, pronunciation)
-- entry. Note that because equality for Entries is based only on (text,
-- pronunciation), you can construct a mock entry containing just these
-- elements.
contains :: Dictionary -> Entry -> Bool
contains d e = Map.member (_toRep e) d

-- | lookupText provides a list of Entries whose text element matches the
-- provided text. If there are no matches, Nothing is returned.
lookupText :: Dictionary -> T.Text -> Maybe [Entry]
lookupText d target = _maybe $ toList $ subDict d (\e -> text e == target)

-- | lookupPron provides a list of Entries whose pronunciation element matches
-- the provided pronunciation. If there are no matches, Nothing is returned.
lookupPron :: Dictionary -> Pronunciation -> Maybe [Entry]
lookupPron d target = _maybe $ toList $ subDict d (\e -> pronunciation e == target)

_maybe :: [a] -> Maybe [a]
_maybe xs =
  if null xs
    then Nothing
    else Just xs

-- TODO: make subDictStrict that strips entries of non-matching definitions

-- $filtering
-- Sub-dictionaries can be constructed to filter a set of entries according to
-- some predicate. 'subDict' can produce arbitrary sub-dictionaries according to
-- a given predicate for entries to satisfy. 'subXTags' and 'subPOS' are
-- convenience functions built on top of 'subDict'.
--
-- In my work with Wiktionary, here are some of the filters that I apply to the
-- data:
--
-- By Tag:
--
-- > subXTags d ["offensive", "derogatory", "informal", "spoken",
-- > "imitating Irish accent", "Singlish", "Braille", "humorous", "vulgar",
-- > "colloquial", "ethnic slur", "religious slur", "informal", "slang",
-- > "archaic", "rare", "obsolete", "Singapore"]
--
-- By Word:
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
subDict :: Dictionary -> (Entry -> Bool) -> Dictionary
subDict d f = Map.filterWithKey (\k v -> f (_entry (k, v))) d

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
-- part of speech.
subPOS :: Dictionary -> T.Text -> Dictionary
subPOS d target =
  subDict d (\e -> any (\def -> pos def == target) (Set.toList $ definitions e))

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
--  the tags.
subXTags :: Dictionary -> [T.Text] -> Dictionary
subXTags d xTags =
  subDict d (\e -> not $ all (\def -> any (`elem` tags def) xTags) (Set.toList $ definitions e))

-- | toList converts a Dictionary to a list of Entries.
toList :: Dictionary -> [Entry]
toList d = _entry <$> Map.toList d

-- becase lookupGE returns the first entry GE an entry with text "c"
-- it is necessary to ensure the entry actually starts with c
-- otherwise, firstOfLetter d 's' could return entry with text "t..."
-- for a dictionary with no entries beginning with 's'

-- | firstOfLetter provides the first entry whose text begins with a given
-- letter. If there are no entries in the Dictionary that begin with the given
-- letter, Nothing is returned.
firstOfLetter :: Dictionary -> Char -> Maybe Entry
firstOfLetter d c =
  let mockE = Rep {_text = T.singleton c, _pron = []}
      maybeE = _entry <$> Map.lookupGE mockE d
   in case maybeE of
        Nothing -> Nothing
        Just e ->
          if T.head (text e) == c
            then Just e
            else Nothing

-- | makeEntry constructs an Entry out of its constituent components. makeEntry
-- takes a list of definitions as its second argument. Use makeEntry1 to
-- construct entries from single-definition, flat argument sets.
makeEntry :: T.Text -> [(T.Text, T.Text, [T.Text])] -> T.Text -> Entry
makeEntry _text _defs _pronString =
  Entry _text defs (makePronunciation _pronString)
  where
    defs = Set.fromList $ (\(g, p, ts) -> Definition g p ts) <$> _defs

_entry :: (Rep, Set.Set Definition) -> Entry
_entry (r, ds) = Entry (_text r) ds (_pron r)

_toInternal :: Entry -> (Rep, Set.Set Definition)
_toInternal e = (_toRep e, definitions e)

_toRep :: Entry -> Rep
_toRep e = Rep {_text = text e, _pron = pronunciation e}
