## next [????.??.??]
* Achieve forward compatibility with
  [GHC proposal 229](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst).

## 0.1.2 [2019.05.02]
* Add a unit test suite.

## 0.1.1
* Add a library dependency in the `doctests` test suite

## 0.1
* Add compare-and-swap support for struct slots
* Add `Data.Struct.TH`, which provides Template Haskell support for
  generating structs
* Remove unneeded proxy argument to `struct`
* Add a type parameter to `Order`
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

## 0
* Repository initialized
* Added structures for list labeling, order-maintenance, and link-cut trees.
