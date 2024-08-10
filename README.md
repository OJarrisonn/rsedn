# rsedn

`rsedn` is a crate that implements a subset(atm) of [Extensible Data Notation](https://github.com/edn-format/edn)

## Supported Syntax

- [x] `( )` lists
- [x] `[ ]` vectors
- [x] `{ }` maps
- [x] `#{ }` sets
- [x] `symbols` (the full edn symbol specification)
- [x] `:keywords`
- [x] `#user/tags`
- [ ] `#_discard` (not being discarted, just parsed)
- [ ] Integers (unsupported arbitrary precision integers)
- [x] Floats
- [x] Boolean
- [x] `nil`
- [ ] Strings (unsupported `\uNNNN` unicode sequences)
- [x] Characters
- [x] Built-in tagged elements (`#inst` and `#uuid`)
- [x] Comments

## Usage

`rsedn` usage is aplit into 4 steps:

1. Build a `Source` from a `&str` (use `rsedn::source_from_str`)
2. Lex `Source` to produce a `Vec<Lexeme>` (use `rsedn::lex_source`)
3. Parse each `Lexeme` to produce a `Token` (use `rsedn::parse_lexeme`)
4. Create a `TokenStream` (use `LinkedList::iter`) and consume it to produce a `Form` (use `rsedn::consume_token_stream`)

## Concepts

### `Source`

A wrapper around the source code, we always refer to source code as `&'source str`. It can be latter used to get the span (the actual text) of some `Lexeme`

### `Lexeme` 

Stores the coordinates of a piece of meaningful source code (just coordinates, no text), it doens't classifies it, just knows that the given piece of text may have a meaning.

For instance: `(println)` has 3 lexemes: `(`, `println`, and `)`, `(def var 5)` has 5 lexemes: `(`, `def`, `var`, `5` and `)`

### `Token`

A wrapper around a lexeme that stores the span and what kind of token it is. It classifies by reading the span and checking the syntax of the corresponding piece of source code.

Producing a `Token` may produce a `TokenizationError` when the lexeme isn't syntatically right.

### `Form`

The final step, it's built by one or more tokens and represents an `edn` form: a list, a vector, a symbol, etc.

Almost no manipulation is done with the source text, except the parsing of text into values like: `i64`, `f64`, `bool` and `String` for the corresponding `edn` forms.

Forms are what you may use out of this library.

Producing forms may produce a `ParsingError` when the tokens in the token stream aren't the expected ones.