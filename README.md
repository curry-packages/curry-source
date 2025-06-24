curry-source: Libraries to check Curry source programs
======================================================

This package contains libraries to deal with the source code level
of Curry programs.

Source code checks
------------------

The following package libraries provide operations to check the intended
usage of some Curry features.

* `Language.Curry.CheckDetUsage`: check the correct use of `DET`
  annotations to mark deterministic operations as described in
  <http://dx.doi.org/10.1007/978-3-319-51676-9_1>

* `Language.Curry.CheckOperations`: check the correct use of
  [set functions](http://doi.acm.org/10.1145/1599410.1599420)
  and whether some auxiliary operations are not used, mainly
  the non-strict equality operation `Prelude.=:<=` which used
  to implement [functional patterns](https://doi.org/10.1007/11680093_2).

Source code classification
--------------------------

The following package libraries provide operations to classify the
source code of Curry modules into various classes of entities.

* `Language.Curry.SourceCodeClassifier`: This library provides
  operations to extract from a given Curry module all defined operations,
  types, and classes and returns lists containing the entity names and
  span information of the comments and source code of these entities.

* `Language.Curry.StringClassifier`: classify a string
  containing Curry source code into various categories,
  like module head, comments, code, etc.

Auxiliary libraries for matching AbstractCurry/FlatCurry expressions
--------------------------------------------------------------------

In addition to the libraries listed above, this package contains also
the libraries `AbstractCurry.Match` and `FlatCurry.Match`
which defines operations intended to be used
as functional patterns in order to match subexpressions
in `AbstractCurry` or `FlatCurry` programs at arbitrary depth.
They are used in the module `Language.Curry.CheckOperations`.

Examples
--------

The directory `examples` of this package contains
example programs showing unintended uses of Curry features
which are detected by the `Check` libraries of this package.
Note that these libraries are integrated in CurryCheck
so that these unintended uses are detected when processing
these examples with CurryCheck.
