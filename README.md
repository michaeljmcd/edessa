# Edessa

Edessa is a parser combinator library for Clojure. Rather than drawing its
inspiration primarily from common libraries in other languages, like Haskell's
Parsec (https://hackage.haskell.org/package/parsec), it was written by reading
the common papers on the subject and trying to build a new implementation from
scratch.

## Usage

No artifacts are currently published (though this will change eventually), but a
git dependency can be indicated in `deps.edn` like so:

  {:deps {github-michaeljmcd/edessa {:git/url "https://github.com/michaeljmcd/edessa"
                                     :sha "b3a139cf6cd7667358b3d5f28ffed8ab099decb9"}}

From there, all code is under `edessa.parser`. See the docs for more
introductory examples.

## Development

Edessa uses [Clojure Deps](https://clojure.org/guides/deps_and_cli) for
dependency management. To run the test suite, run

  clj -M:kaocha

The main code all lives in `edessa.parser`. Similarly, documentation can be
generated with Codox by running

  clj -X:codox

Which will generate HTML documentation in `target/doc`.

## License

This code is [licences](LICENSE.md) under the MIT license.
