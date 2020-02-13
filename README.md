# Dictionary

## Introduction
A Dictionary is a set of lexical entries. It associates a (text, pronunciation)
pair with a set of definitions, where each association forms an Entry. A
dictionary, like the one you might find on your bookshelf, is ordered
alphabetically by text.

An Entry is a data structure that contains:

- text - the textual representation of the entry
- pronunciation - the spoken representation of the entry
- definitions: - a set of (meaning, part of speech) pairs for the entry.

The uniqueness of entries is based on their representation, which is their
textual representation and spoken representation together. Some example entries
might be:

- "read", "ɹid" (_I read the book_; present tense)
- "read", "rɛd" (_I read the book_; past tense)
- "reed", "ɹid" (_the reeds grow in the water_)
- "red",  "ɹɛd" (_the apple is red_)

Each representational pair (text, pronunciation) points to a set of definitions.

Pronunciation and sound-related information are represented using
[bnspalding/sound](https://github.com/bnspalding/sound). Pronunciations parse
IPA symbols and represent sounds based on an approximation of the General 
American English accent.

Technical documentation for the library can be generated using haddock.

## Usage

The following methods are defined for working with dictionaries:

- first :: Dictionary -> Entry
- final :: Dictionary -> Entry
- size :: Dictionary -> Int
- next :: Dictionary -> Entry -> Entry
- prev :: Dictionary -> Entry -> Entry
- firstOfLetter :: Dictionary -> Char -> Maybe Entry
- contains :: Dictionary -> Entry -> Bool
- lookupText :: Dictionary -> Text -> Maybe [Entry]
- lookupPron :: Dictionary -> Pronunciation -> Maybe [Entry]
- subDict :: Dictionary -> (Entry -> Bool) -> Dictionary
- subPOS :: Dictionary -> Text -> Dictionary

## LICENSE

See the [LICENSE](https://github.com/bnspalding/dictionary/blob/master/LICENSE)
file in the repository.

TODO: add hooks to wiktionary data

