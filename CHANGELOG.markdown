## next [????.??.??]
* Avoid a particularly dodgy use of `unsafeCoerce#` in the implementation of
  `isNil` when building with GHC 9.4 or later. This is necessary to make the
  `isNil` function behave properly on GHC 9.6, as changes to GHC's optimizer in
  9.6 make that use of `unsafeCoerce#` produce unexpected results at runtime.

## 0.1.6 [2021.04.30]
* Make the test suite compile on recent GHCs.

## 0.1.5 [2021.02.17]
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

## 0.1.4 [2020.10.02]
* Allow building with `template-haskell-2.17.0.0` (GHC 9.0).

## 0.1.3 [2020.01.29]
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
