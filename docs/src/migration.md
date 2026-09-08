```@meta
CurrentModule = MemoryViews
```

# Migrating from 0.4 to 0.5

Use this guide when migrating MemoryViews.jl from v0.4 to v0.5.
The guide is intended to be followed by humans and AI agents alike.

## `MemoryKind`, `IsMemory`, `NotMemory`, and `inner` have been removed
The purpose of the `MemoryKind` trait was to funnel dispatch of call-site-unknown types
toward potentially using `MemoryView`, based on whether a type `T` "was equivalent"
to its own `MemoryView`. However, I've found that "equivalence" is a property
specific to a call-site/type combination, and therefore doesn't work as a general
interface.
For example, `String` is memory-like in some aspects (hashing, equality), but
not others, and `Matrix` is memory-like in some aspects, but differs in its axes.

Mitigation: Opt in per function/type combination to construct a `MemoryView` and
forward function calls.
That is, instead of guiding dispatch with `MemoryKind` for a given function,
make sure every type that needs to use the `MemoryView` path for that function
has a dedicated method that constructs a `MemoryView`.

## Access to `parent`, `memoryref`, `v.ref`, and `v.len`
Previously, it was too easy to access a mutable `MemoryRef` from an `ImmutableMemoryView`,
which could cause undefined behaviour without explicitly using unsafe code, because supposedly
immutable memory (e.g. memory backing a `String`) could be mutated.
This topic includes several changes:

### Change 1: `parent` now returns a `MemoryView`, not `Memory`
We now have `parent(::T)::T where {T <: MemoryView}`, i.e. `parent(::ImmutableMemoryView{T})`
now returns `ImmutableMemoryView{T}`, and `parent(::MutableMemoryView{T})` returns
`MutableMemoryView{T}`. The returned view spans the entire memory.

Mitigation: Where previous use of `parent` only relied on returning a `DenseVector` covering
the whole underlying `Memory`, nothing needs to be done.
Where previous use relied on it returning `Memory`, review to make sure the existing code did not
mutate immutable memory.
Where the resulting `Memory` was mutated, consider switching to `MutableMemoryView`.
Where the resulting `Memory` was not mutated, replace it with the new
`unsafe_memory(::MemoryView{T})::Memory{T}`.

### Change 2: `memoryref` is now defined for `MutableMemoryView` only, not `MemoryView` in general
`memoryref(::ImmutableMemoryView)` returning a mutable `MemoryRef` made it too easy to accidentally
mutate immutable memory.

Mitigation: Make sure previous use of `memoryref(::ImmutableMemoryView)` did not lead to mutation
of immutable memory (e.g. memory backing a `String`).
Where the resulting `MemoryRef` was mutated, consider switching to `MutableMemoryView`.
Where a `MemoryRef` is needed, use the new `unsafe_memoryref(::MemoryView{T})::MemoryRef{T}`.

### Change 3: `Base.cconvert(::Type{<:Ptr}, x::MemoryView)` now returns `typeof(x)`, not `Ptr`
The new definition better conforms to the intended meaning of `cconvert`, which is not
supposed to be an unsafe operation.
It now returns its input argument instead.

Mitigation: Generic code should not have relied on `Base.cconvert` returning a `Ptr`,
as advised against in the documentation of `Base.cconvert`.
Where a `Ptr` is required, use `Base.unsafe_convert(::Type{<:Ptr}, ::MemoryView)`,
which still returns a `Ptr`, or call `pointer` directly.

### Change 4: Fields of `MemoryView` are now explicitly private implementation details
Ambiguous phrasing in the docs of previous versions could lead users to believe they could access
the fields `.ref` or `.len` directly. These are now private.

Mitigation: Where the underlying reference was needed, use `memoryref` for mutable views, and `unsafe_memoryref`
for possibly-immutable views. Obtain the length with `length(::MemoryView)`.

## `DelimitedIterator` now has three type parameters
`DelimitedIterator{T, M}` now has a third type parameter,
`DelimitedIterator{T, M, D}`, which stores the delimiter's type separately from
the input element type `T`.
This allows, for example, splitting a `Vector{Integer}`, which was impossible before.

Mitigation: When referring to the parameterized type, make sure to include
all three type parameters, lest you have a non-concrete `UnionAll` type.
Search for all occurrences of `DelimitedIterator{`.

## Reverse iteration preserves mutability
`Iterators.reverse` now stores the original `MemoryView`, preserving its
mutability. Applying it twice returns the original view; previously, the result
was always an `ImmutableMemoryView`, even for mutable input.

Mitigation: Code that relied on two applications of `Iterators.reverse(::MemoryView)`
always returning an `ImmutableMemoryView` should explicitly construct an
`ImmutableMemoryView` from the result of applying `Iterators.reverse` twice.
