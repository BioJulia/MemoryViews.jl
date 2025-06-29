# Changelog
This project follows semantic versioning (semver) 2.0.
Any new features, or breaking changes, will be written in this file.
Bugfixes, internal refactors, documentation improvements and style changes will
not be mentioned here, because they do not impact how the package is to be used.

## 0.3.2
* Support FixedSizeArrays
* Various bugfixes and optimisations
* Format with Runic


## 0.3.1
* Fix a bug in parentindices, where it returned the wrong type
* Make `similar` return a mutable array always
* Various bugfixes and optimisations

## 0.3.0
### Breaking changes
* Change the bounds checking behaviour of the find* functions to match those of
  `Vector`. In particular, previously, `findnext(pred, mem, -5)` would be
  equivalent to searching from index 1, and similarly, `findprev(pred, mem,
  lastindex(mem) + 10)` would be equialent to searching from `lastindex(mem)`.
  Now, searching from an index before the first valid index throws a `BoundsError`.
  Findfirst searching from `i > lastindex(mem)`, and findlast searching from
  `i < 1` will still simply return `nothing`, just like searching vectors.

### Other changes
* Add optimised versions of `findprev` and `findlast`, searching bytes
* Add optimised version of `find*(iszero, bytes)` methods
* Add optimised generic `find*` methods
* Add functions `split_first`, `split_last`, `split_at` and `split_unaligned`
* Add a more correct implementation of `Base.mightalias` for memory views and
  some types of arrays



