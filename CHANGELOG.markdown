## next
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
