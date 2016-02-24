# repa-utils

Utilities for working with [`repa`](https://www.stackage.org/package/repa). Including:

* `Data.Array.Repa.Shape.Int`: Orphan instance of `Int` for `Shape` class. Enables `repa` 1-D arrays like `Vector` which are easier to use.
* `Data.Array.Repa.Operators.Filtering`: A `filterP` function which is lacking from standard `repa` library.
* `Data.Array.Repa.Tree`: A multi-way tree as a nested `repa` array.
* `Data.Array.Repa.Instances`: Orphan instances of `repa` arrays for `Prelude` classes.
