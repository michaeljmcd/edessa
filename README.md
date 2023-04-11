# Edessa

Edessa is a parser combinator library for Clojure. Rather than drawing its
inspiration primarily from common libraries in other languages, like Haskell's
[Parsec](https://hackage.haskell.org/package/parsec), it was written by reading
the common papers on the subject and trying to build a new implementation from
scratch.

## Usage

### Dependency

No artifacts are currently published (though this will change eventually), but a
git dependency can be indicated in `deps.edn` like so:

```
  {:deps {github-michaeljmcd/edessa {:git/url "https://github.com/michaeljmcd/edessa"
                                     :sha "5610465d68dfbfc82b412abe1a142d871a52d17e"}}
```

From there, all code is under `edessa.parser`.

### Text Parsing Quickstart

In Edessa, parsers are functions that accept a parser input map and produce
whatever output is relevant to that particular parser. A parser input object can
be created from a sequence of characters or objects can be created with the
`make-input` function.

```clojure
  (make-input "some very long text...")
```

From there, parser functions can be applied to input. The simplest parser is
`match` which matches against the next object in the input stream.

```clojure
  ((match \a) (make-input  "aaaa"))
```

See the docs for more introductory examples.

## Development

Edessa uses [Clojure Deps](https://clojure.org/guides/deps_and_cli) for
dependency management. To run the test suite, run

```
  clj -M:kaocha
```

The main code all lives in `edessa.parser`. Similarly, documentation can be
generated with Codox by running

```
  clj -X:codox
```

Which will generate HTML documentation in `target/doc`.

## License

This code is [licensed](LICENSE.md) under the MIT license.
