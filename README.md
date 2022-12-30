# Cassava Megaparsec

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)
[![Hackage](https://img.shields.io/hackage/v/cassava-megaparsec.svg?style=flat)](https://hackage.haskell.org/package/cassava-megaparsec)
[![Stackage Nightly](http://stackage.org/package/cassava-megaparsec/badge/nightly)](http://stackage.org/nightly/package/cassava-megaparsec)
[![Stackage LTS](http://stackage.org/package/cassava-megaparsec/badge/lts)](http://stackage.org/lts/package/cassava-megaparsec)
![Build Status](https://github.com/stackbuilders/cassava-megaparsec/actions/workflows/build.yml/badge.svg)

The package provides alternative parser for the
[Cassava](https://hackage.haskell.org/package/cassava) package written with
[Megaparsec](https://hackage.haskell.org/package/megaparsec) so you can get
better error messages at expense of some speed.

## Quick start

The package works seamlessly with Cassava by replacing the following
functions:

* `decode`
* `decodeWith`
* `decodeByName`
* `decodeByNameWith`

The functions work just the same as Cassava's equivalents, but also take
name of file they parse (to include into error messages) and return typed
high-quality error messages produced by
[Megaparsec](https://hackage.haskell.org/package/megaparsec).

The import section typically looks like this:

```haskell
import Data.Csv hiding (decode, decodeWith, decodeByName, decodeByNameWith)
import Data.Csv.Parser.Megaparsec (decode, decodeWith, decodeByName, decodeByNameWith)
```

Next you call appropriate function and get either result of parsing
identical to that of Cassava or error message. The error message is
well-typed so you can examine it in Haskell code easily. Conversion error
are considered parsing errors by the `cassava-megaparsec` package and are
reported via custom error message component `Cec` supported by Megaparsec 5.
Since Cassava's conversion errors are plain strings, we have no choice but
to represent them as strings too:

```haskell
-- | Custom error component for CSV parsing. It allows typed reporting of
-- conversion errors.

data Cec
  = CecFail String
  | CecIndentation Ordering Pos Pos
  | CecConversionError String
  deriving (Eq, Data, Typeable, Ord, Read, Show)
```

To pretty print a error message use the `parseErrorPretty` function from
`Text.Megaparsec`.

This should be enough to start using the package, please consult Haddocks
for detailed per-function documentation.

## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](/docs/CONTRIBUTING.md) to know how you can help us build it.

---
<img src="https://cdn.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>  
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)
