# Dictionary

A Dictionary is a set of lexical entries. It associates a (text, pronunciation)
pair with a set of definitions, where each association forms an Entry. A
dictionary, like the one you might find on your bookshelf, is ordered
alphabetically by text.

An Entry is a data structure that contains:

- text - the textual representation of the entry
- pronunciation - the spoken representation of the entry
- definitions: - a set of (meaning, part of speech, tag) structs for the entry.

The uniqueness of entries is based on their representation, which is their
textual representation and spoken representation together. Some example
representations that would each have a unique entry might be:

- "read", "ɹid" (_I read the book_; present tense)
- "read", "rɛd" (_I read the book_; past tense)
- "reed", "ɹid" (_the reeds grow in the water_)
- "red",  "ɹɛd" (_the apple is red_)

Each representational pair (text, pronunciation) points to a set of definitions.

Pronunciation and sound-related information are represented using
[bnspalding/sound](https://github.com/bnspalding/sound). Pronunciations parse
IPA symbols and represent sounds based on an approximation of the General 
American English accent.


## Usage

### create a dictionary

Create a dictionary out of its entries' constituent components: text, definitions,
pronunciation. A definition has a meaning, a part of speech, and zero or more
tags associated with the entry.

```haskell
import Dictionary

let dict = fromList $
  [ (makeEntry "opal" [("a gemstone", "n", [])] "ˈo͡ʊ.pəl"),
    (makeEntry "stack" [("a vertical pile", "n", []), ("to place vertically", "v", [])] "stæk"),
    (makeEntry "aardvark" [("a very cute animal"), "n", ["animal"])] "ˈɑɹd.vɑɹk")
  ]
-- generate a dictionary with three entries

size dict
-- 3
```

### find items in the dictionary

There are a variety of functions for finding items within a dictionary. Entries
are ordered according to their textual representation, alphabetically. The
dictionary is ordered the same way you might expect a paper dictionary on your
bookshelf to be ordered.

```haskell
-- using the dictionary defined above ...

first dict
-- Entry "aardvark"

firstOfLetter 'o' dict
-- Just Entry "opal"

firstOfLetter 'b' dict
-- Nothing

final dict
-- Entry "stack"
```

You can search for entries by either text or pronunciation. Because entries are
unique only by their text-pronunciation pairs, these lookups return a list. The
list is wrapped in a `Maybe`, because an empty list of results is instead a
`Nothing`.

When searching by pronunciation, stress information must be accurate for the
pronunciation in order for it to return a match.

```haskell
import Sound.Pronunciation
import Data.Maybe (fromJust)

lookupText "opal" dict
-- Just [Entry "opal"]

lookupText "archway" dict
-- Nothing

lookupPron (fromJust (makePronunciation "ˈo͡ʊ.pəl")) dict
-- Just [Entry "opal"]
-- we use fromJust because we are confident our pronunciation text is valid

lookupPron (fromJust (makePronunciation "opal")) dict
-- Nothing
-- The pronunciation is valid, but there is no matching entry in the dictionary
```

Entries are ordered alphabetically within the dictionary. You can move from one
entry to its adjacent entries with `next` and `prev`. The result is wrapped in a
`Maybe` because the first entry has no previous entry and the last entry has no
next entry.

```haskell
let aardvark = first dict

next aardvark dict
-- Just Entry "opal"

prev aardvark dict
-- Nothing
```

### create a sub-dictionary

Dictionary provides several tools for producing sub-dictionaries. `subDict`
filters a dictionary by whatever predicate on entries is provided.

```haskell
subDict (`elem` ["my", "filter", "words", "opal"] . text) dict
-- filter entries by their text. 
-- The example dictionary would no longer contain the Entry "opal"

subDict 
  ( any (null . `intersect` ["my", "filter", "words", "cute"] . words) 
    . Set.toList
    . definitions
  ) dict
-- filter entries by their definitions. At least one is clear of filter words.
-- The example dictionary would no longer contain the Entry "aardvark"
```

Convenience filters are provided for part of speech filtering and tag filtering.

```haskell
subPOS "v" dict
-- filter to only entries with at least one pos="v" definition
-- The example dictionary would only contain the Entry "stack"

subXTags ["animal"] dict
-- filter to only entries with at least one definition free of the given tags
-- The example dictionary would no longer contain the Entry "aardvark"
```

## More Documentation

Module documentation for the package can be generated using haddock. Run
`stack haddock --open dictionary` to view the package once it has been included
in your project.

## LICENSE

See the [LICENSE](https://github.com/bnspalding/dictionary/blob/master/LICENSE)
file in the repository.
