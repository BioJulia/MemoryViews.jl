## Why `MemViews`
* `SubArray` is too complex.
    - Quick, what are its FIVE parameters?
    - Can you name the subset of a `SubArray` which is backed by dense memory so you can use ccalls etc?
* `SubArrays` tends to rely too much on abstract types.
    - E.g. <: AbstractUnitRange. For low-level computing that means you rely on all implementers to be correct
* Methods for abstract arrays tend to miss important optimisations