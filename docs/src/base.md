# MemoryViews.jl
It is my hope that MemoryViews, or something like MemoryViews, will eventually
be moved into Base Julia.
This is because Base Julia, too, includes code that uses the concept of a memory-backed array.
However, Base currently lacks any kind of interface and internal API to handle memory-backed objects.

See [the related issue on JuliaLang/julia](https://github.com/JuliaLang/julia/issues/54581).

## What's wrong with `SubArray`s of `Memory` as memory views?
`SubArray` is generic over too much, and is therefore too hard to reason about,
and to uphold its guarantees.

First, it's generic over the array type, meaning it may be backed by `Memory` or `Vector`, but also `UnitRange` or `Base.LogRange` (bitstypes, so not backed by memory), `BitMatrix` (memory-backed, but elements are stored packed), `OffsetArrays`, `CodeUnits` (memory-backed but immutable) and many more.
What can you do with the underlying array, generally speaking? Take a pointer to it? No. Assume one-based indexing? No. Assume a stride of one? No. Assume mutability? No.

Second, it's generic over the index type. It may be `UnitRange{Int}`, of course, but also `Base.OneTo{UInt16}`, or `StepRange{BigInteger}`, `CartesianIndices` (which it itself generic over the indexes), `Colon`.
Can you define the subset of these types which indicate dense indices? I can't.

Third, it's multidimensional. It may `collect` to a `Vector` or `Matrix`.

This is not a design flaw of `SubArray` - it's a perfectly fine design choice, which enables `SubArray` to be extremely flexible and broadly useful.
Unfortunately, it also makes it nearly impossible to write robust, low-level code using `SubArray`, because it's almost imopssible not to violate the assumptions of a subset of `SubArray`s many concrete types.
Practically speaking, what happens is that methods taking `SubArray` fall back to only assuming what can be assumed about `AbstractArray` - which may be inefficient, and buggy (as the recurring bugs due to assumption of one-based indexing has taught us).

In contrast, a `MemoryView{T}` is _always_ represented by exactly a `MemoryRef{T}` and an `Int` as length.
You know exactly what you get.

## Design decisions
#### Mutability
Mutable and immutable memory views are statically distinguished, such that users
can write methods that only take mutable memory views.
This will statically prevent users from accidentally mutating e.g. strings.

#### MemoryKind
The MemoryKind trait is used because constructing a MemoryView only for dispatch purposes
may not be able to be optimised away by the compiler for some types (currently, strings).

MemoryKind could be replaced with a function that returned `nothing`, or the correct
MemoryView type directly, but it's nicer to dispatch on `::MemoryKind` than on `::Union{Nothing, Type{<:MemoryView}}`.

## Limitations
* Currently, `MemoryView` does not make use of `Core.GenericMemory`'s additional parameters, such as
  atomicity or address space.
  This may easily be added with a `GenericMemoryView` type, similar to `Memory` / `GenericMemory`.

* I can't figure out how to support reinterpreted arrays.
  Any way I can think of doing so will sigificantly complicate `MemoryView`, which takes away some of
  the appeal of this type's simplicity.
  It's possible that reinterpreted arrays are so outside Julia's ordinary memory management
  that this simply can't be done.

* Currently, `String`s are not backed by `Memory` in Julia. Therefore, creating a `MemoryView` of a string
  requires heap-allocating a new `Memory` pointing to the existing memory of the string.
  This can be fixed if `String` is re-implemented to be backed by `Memory`, but I don't know
  enough details about the implementation of `String` to know if this is practical.

## Alternative proposal
In `examples/alternative.jl`, there is an implementation where a `MemoryView` is just a pointer and a length.
This makes it nearly identical to `Random.UnsafeView`, however, compared to `UnsafeView`, this propsal has:

* The `MemoryKind` trait, useful to control dispatch to functions that can treat arrays _as being memory_
* The distinction between mutable and immutable memory views

Overall, I like the alternative proposal less. Raw pointers are bad for safety and ergonomics, and they interact
less nicely with the Julia runtime. Also, the existing `GenericMemoryRef` is essentially perfect for this purpose.

#### Advantages
* Pointer-based memviews are cheaper to construct, and do not allocate for strings, unlike `Memory`.
  Perhaps in the future, strings too will be backed by `Memory`.
* Their interaction with the GC is simpler (as there is no interaction)

#### Disadvantages
* While some low-level methods using `MemoryView` will just forward to calling external libraries where
  using a pointer is fine, many will be written in pure Julia. There, it's less nice to have raw pointers.
* Code using pointer-based memviews must make sure to only have the views exist inside `GC.@preserve` blocks,
  which is annoying and will almost certainly be violated accidentally somewhere
* We can't use advantages of the existing `Memory` infrasrtructure, e.g. having a `GenericMemRef` which supports
  atomic memory.
