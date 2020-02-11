module Dictionary
  ( Dictionary,
    Entry (..),
    Definition (..),
    makeEntry,
    fromStringTuples,
    fromList,
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

type Dictionary = Set.Set Entry

data Definition
  = Definition
      { gloss :: T.Text,
        pos :: T.Text
      }
  deriving (Eq, Show)

data Entry
  = Entry
      { text :: T.Text,
        definitions :: [Definition], -- gloss, pos
        pronunciation :: Pronunciation -- same as Sound.Word, i.e. [Syl]
      }
  deriving (Eq, Show)

instance Ord Entry where
  compare e1 e2 = compare (text e1) (text e2)

fromList :: [Entry] -> Dictionary
fromList = Set.fromList

fromStringTuples :: [(T.Text, [(T.Text, T.Text)], T.Text)] -> Dictionary
fromStringTuples ts = fromList $ uncurriedMakeEntry <$> ts
  where
    uncurriedMakeEntry (t, ds, pr) = makeEntry t ds pr

first :: Dictionary -> Entry
first = Set.elemAt 0

last :: Dictionary -> Entry
last d = Set.elemAt (size d - 1) d

size :: Dictionary -> Int
size = Set.size

-- next wraps around the dictionary when given the last entry
next :: Dictionary -> Entry -> Entry
next d entry = Set.elemAt i d
  where
    prevI = Set.findIndex entry d
    i =
      if prevI == size d - 1
        then 0
        else prevI + 1

-- prev wraps around the dictionary when given the first entry
prev :: Dictionary -> Entry -> Entry
prev d entry = Set.elemAt i d
  where
    nextI = Set.findIndex entry d
    i =
      if nextI == 0
        then size d - 1
        else nextI - 1

contains :: Dictionary -> Entry -> Bool
contains d entry = Set.member entry d

-- becase lookupGE returns the first entry GE an entry with text "c"
-- it is necessary to ensure the entry actually starts with c
-- otherwise, firstOfLetter d 's' could return entry with text "t..."
-- for a dictionary with no entries beginning with 's'
firstOfLetter :: Dictionary -> Char -> Maybe Entry
firstOfLetter d c =
  let maybeE = Set.lookupGE (Entry (T.singleton c) [] []) d
   in case maybeE of
        Nothing -> Nothing
        Just e ->
          if T.head (text e) == c
            then Just e
            else Nothing

makeEntry :: T.Text -> [(T.Text, T.Text)] -> T.Text -> Entry
makeEntry _text _defs _pronString =
  Entry _text defs (makePronunciation _pronString)
  where
    defs = uncurry Definition <$> _defs
