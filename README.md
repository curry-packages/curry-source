curry-source: Libraries to check Curry source programs
======================================================

This package contains libraries to deal with the source code level
of Curry programs.

There are some libraries to check the intended usage
of some Curry features.

* `Language.Curry.CheckDetUsage`: check the correct use of `DET`
  annotations to mark deterministic operations as described in
  <http://dx.doi.org/10.1007/978-3-319-51676-9_1>

* `Language.Curry.CheckOperations`: check the correct use of
  [set functions](http://doi.acm.org/10.1145/1599410.1599420)
  and whether some auxiliary operations are not used, mainly
  the non-strict equality operation `Prelude.=:<=` which used
  to implement [functional patterns](https://doi.org/10.1007/11680093_2).

Furthermore, there is a library `Language.Curry.StringClassifier`
to process strings containing Curry source code.


Examples
--------

The directory `examples` of this package contains
example programs showing unintended uses of
Curry features which are detected by the libraries of this package.
Note that these libraries are integerated in CurryCheck
so that these unintended uses are detected when processing
these examples with CurryCheck.

