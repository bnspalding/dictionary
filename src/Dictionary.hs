-- |
-- Module: Dictionary
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- A Dictionary is a set of lexical entries. It associates a ('text', 'pronunciation')
-- pair with list of 'Definition's, where each association is an 'Entry'. A
-- Dictionary is ordered alphabetically by text.
--
-- A special note about this representation of a dictionary: The 'next' and
-- 'prev' functions wrap around (such that the last element points to the first,
-- and the first element points to the last when using 'next' and 'prev'). This
-- decision comes from the way that Dictionary is used in Sequence, Selection
-- libraries farther up the ladder.
module Dictionary
  ( -- * Types
    Dictionary,
    Entry (..),
    Definition (..),

    -- * Construction
    makeEntry,
    fromStringTuples,
    fromList,

    -- * Methods
    first,
    Dictionary.last,
    size,
    next,
    prev,
    firstOfLetter,
    contains,
  )
where

import qualified Data.Set as Set
import qualified Data.Text as T
import Sound.Pronunciation (Pronunciation, makePronunciation)

-- | A Dictionary is a Set of Entries
type Dictionary = Set.Set Entry

-- | A Definition is a meaning, an ontological unit of a dictionary.
data Definition
  = Definition
      { -- | gloss gives the textual definition for a word
        gloss :: T.Text,
        -- | pos gives the part of speech tied to the definition
        pos :: T.Text
      }
  deriving (Eq, Show)

-- | an Entry associates a (text, pronunciation) pair with a list of
-- definitions.
--
-- Entries are compared based on their text, and they are evaluated for equality
-- (uniqueness) based on their (text, pronunciation) pair.
data Entry
  = Entry
      { text :: T.Text,
        definitions :: [Definition], -- gloss, pos
        pronunciation :: Pronunciation -- same as Sound.Word, i.e. [Syl]
      }
  deriving (Show)

-- | Entries are ordered based on their text (alphabetically)
instance Ord Entry where
  compare e1 e2 = compare (text e1) (text e2)

-- | Entries are unique based on their (text, pronunciation) pair
instance Eq Entry where
  e1 == e2 = text e1 == text e2 && pronunciation e1 == pronunciation e2

-- | fromList generates a Dictionary from a list of Entries. See
-- "Data.Set".'Set.fromList'
fromList :: [Entry] -> Dictionary
fromList = Set.fromList

-- TODO: fix fromStringTuples to use a flat structure.
-- Use the equality of entries to merge multiple definitions onto entries

-- | fromStringTuples constructs a dictionary from a set of string tuples
-- that are generated, for example, from parsing a file
fromStringTuples :: [(T.Text, [(T.Text, T.Text)], T.Text)] -> Dictionary
fromStringTuples ts = fromList $ uncurriedMakeEntry <$> ts
  where
    uncurriedMakeEntry (t, ds, pr) = makeEntry t ds pr

-- | first provides the first element of the Dictionary (by text, ascending)
first :: Dictionary -> Entry
first = Set.elemAt 0

-- | last provides the last element of the Dictionary (by text, ascending)
last :: Dictionary -> Entry
last d = Set.elemAt (size d - 1) d

-- | the size of the dictionary. See "Data.Set".'Set.size'
size :: Dictionary -> Int
size = Set.size

-- | next gives the next element of the Dictionary (alphabetically).
-- Next wraps around the dictionary when given the last entry (it gives the
-- first)
next :: Dictionary -> Entry -> Entry
next d entry = Set.elemAt i d
  where
    prevI = Set.findIndex entry d
    i =
      if prevI == size d - 1
        then 0
        else prevI + 1

-- | prev gives the previous element of the Dictionary (alphabetically).
-- Prev wraps around the dictionary when given the first entry (it gives the
-- last)
prev :: Dictionary -> Entry -> Entry
prev d entry = Set.elemAt i d
  where
    nextI = Set.findIndex entry d
    i =
      if nextI == 0
        then size d - 1
        else nextI - 1

-- | contains reports whether the Dictionary contains a (text, pronunciation)
-- entry. Note that because equality for Entries is based only on (text,
-- pronunciation), you can construct a mock entry containing just these
-- elements.
contains :: Dictionary -> Entry -> Bool
contains d entry = Set.member entry d

-- TODO: add containsText and containsPron

-- becase lookupGE returns the first entry GE an entry with text "c"
-- it is necessary to ensure the entry actually starts with c
-- otherwise, firstOfLetter d 's' could return entry with text "t..."
-- for a dictionary with no entries beginning with 's'

-- | firstOfLetter provides the first entry whose text begins with a given
-- letter. If there are no entries in the Dictionary that begin with the given
-- letter, Nothing is returned.
firstOfLetter :: Dictionary -> Char -> Maybe Entry
firstOfLetter d c =
  let maybeE = Set.lookupGE (Entry (T.singleton c) [] []) d
   in case maybeE of
        Nothing -> Nothing
        Just e ->
          if T.head (text e) == c
            then Just e
            else Nothing

-- | makeEntry constructs an Entry out of its constituent components. makeEntry
-- takes a set of definitions as its second argument. Use makeEntry' to
-- construct entries from single-definition, flat argument sets.
makeEntry :: T.Text -> [(T.Text, T.Text)] -> T.Text -> Entry
makeEntry _text _defs _pronString =
  Entry _text defs (makePronunciation _pronString)
  where
    defs = uncurry Definition <$> _defs
-- TODO: add makeEntry' that just takes 4 flat arguments
