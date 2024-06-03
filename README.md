# MemViews.jl
This is an experimental repo to work out an interface for a (possibly internal, unexported)
memory view type for use in Base Julia.

* See [the related issue on JuliaLang/julia](https://github.com/JuliaLang/julia/issues/54581).

#### Proposal with MemView backed by MemoryRef
* See the [type definitions](https://github.com/jakobnissen/MemViews.jl/blob/master/src/MemViews.jl)
* See [example code making use of MemViews](https://github.com/jakobnissen/MemViews.jl/blob/master/src/example_find.jl)

#### Proposal with MemView backed by pointers
* [Same types and example code as above](https://github.com/jakobnissen/MemViews.jl/blob/master/src/alternative.jl)

## Overview
The `MemView{T, M}` type represents a chunk of contiguous non-atomic memory in CPU address space.
The `MemKind` trait type is used for dispatch to correctly select methods that can
work on memory directly.
Conceptually, it's a `Memory{T}` with an offset and a length. Its layout is:

```julia
struct MemView{T, M} <: DenseVector{T}
    ref::MemoryRef{T}
    len::Int
end
```

The `M` parameter of `MemView{T, M}` may be `:mutable` or `:immutable`, corresponding
to the type aliases `MutableMemView{T}` and `ImmutableMemView{T}`.

## Implementing `MemView`s for your own types
New types `T` which are backed by dense memory should implement `MemView(x::T)`.
If `x` is mutable, `MemView(x)` should always return a `MutableMemory`.

Further, for types `T` that semantally _are_ chunks of memory, e.g. `Vector`,
`Memory`, `CodeUnits{UInt8, String}` and dense views of these, one should also
implement `MemKind(x::T) = IsMemView{V}()`, where `V` is the concrete type of `MemView`
instantiated by `MemView(x)`.
This will allow methods to opt-in to creating memory views from objects of type `T`
and operating on the views.

## Writing methods using `MemView`
For an example, see [`src/example_find.jl`](https://github.com/jakobnissen/MemViews.jl/blob/master/src/example_find.jl)

Typically, it makes sense to implement the low-level memory manipulation of an object
with functions that take `MemView`s. This has a few advantages:
* It allows multiple different types to use the same implementation, compiling it only once.
* It makes it more ergonomic to later have other memory-like types use the same implementation
* Since `MemView`s are simpler structs than say, `Vector`s, code using
  `MemView`s may be easier to reason about.

Above the low-level implementation, there will typically be a set of methods that
control dispatch, either to the `MemView` method if applicable, or to more generic
fallback methods otherwise.

At the very top of the dispatch chain, one would typically want to dispatch using
the `MemKind` trait. Objects implementing this trait can be directly coverted to
memory views.

It is idiomatic to, when writing a method that only reads memory, implement it
only for `ImmutableMemView`. The constructor `ImmutableMemView(x)` can be used
to get an immutable view, even for types for which `MemView(x)` returns a mutable view.
The advantage of this approach is that it makes the assumptions of the code clearer.

## Design decisions
#### Mutability
Mutable and immutable memory views are statically distinguished, such that users
can write methods that only take mutable memory views.
This will statically prevent users from accidentally mutating e.g. strings.

#### MemKind
The MemKind trait is used because constructing a MemView only for dispatch purposes
may not be able to be optimised away by the compiler for some types (currently, strings).

MemKind operates on instances, because it's possible some types may
be mutable or immutable depending on runtime information.
On the other hand, operating on types would allow users to do something
like this:

```julia
function foo(v::Vector{T}) where T
    M = MemKind(T)
    ...
end
```
Even for an empty `v` with no instances.

MemKind could be replaced with a function that returned `nothing`, or the correct
MemView type directly, but it's nicer to dispatch on `::MemKind` than on `::Union{Nothing, Type{<:MemView}}`.

## Limitations
* Currently, `MemView` does not make use of `Core.GenericMemory`'s additional parameters, such as
  atomicity or address space.
  This may easily be added with a `GenericMemView` type, similar to `Memory` / `GenericMemory`.

* I can't figure out how to support reinterpreted arrays.
  Any way I can think of doing so will sigificantly complicate `MemView`, which takes away some of
  the appeal of this type's simplicity.
  It's possible that reinterpreted arrays are so outside Julia's ordinary memory management
  that this simply can't be done.

* Currently, `String`s are not backed by `Memory` in Julia. Therefore, creating a `MemView` of a string
  requires heap-allocating a new `Memory` pointing to the existing memory of the string.
  This can be fixed if `String` is re-implemented to be backed by `Memory`, but I don't know
  enough details about the implementation of `String` to know if this is practical.

## Alternative proposal
In `src/alternative.jl`, there is an implementation where a `MemView` is just a pointer and a length.
This makes it nearly identical to `Random.UnsafeView`, however, compared to `UnsafeView`, this propsal has:

* The `MemKind` trait, useful to control dispatch to functions that can treat arrays _as being memory_
* The distinction between mutable and immutable memory views

Overall, I like the alternative proposal less. Raw pointers are bad for safety and ergonomics, and they interact
less nicely with the Julia runtime. Also, the existing `GenericMemoryRef` is essentially perfect for this purpose.

#### Advantages
* Pointer-based memviews are cheaper to construct, and do not allocate for strings, unlike `Memory`.
  Perhaps in the future, strings too will be backed by `Memory`.
* Their interaction with the GC is simpler (as there is no interaction)

#### Disadvantages
* While some low-level methods using `MemView` will just forward to calling external libraries where
  using a pointer is fine, many will be written in pure Julia. There, it's less nice to have raw pointers.
* Code using pointer-based memviews must make sure to only have the views exist inside `GC.@preserve` blocks,
  which is annoying and will almost certainly be violated accidentally somewhere
* We can't use advantages of the existing `Memory` infrasrtructure, e.g. having a `GenericMemRef` which supports
  atomic memory.
