---------------------------------------------------------------------------
-- Some examples for unintended uses of internal operations which
-- are detected by CurryCheck (based on the libraries of this package).
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- The direct use primitives to implement functional patterns is
-- not allowed since their intended use is not ensured.

testNonStrictUniv x | 3 =:<= x = True  -- not allowed!

---------------------------------------------------------------------------
