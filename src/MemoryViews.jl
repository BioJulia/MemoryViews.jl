module MemoryViews

export MemoryView,
    ImmutableMemoryView,
    MutableMemoryView,
    split_each,
    unsafe_from_parts,
    unsafe_memoryref,
    unsafe_memory,
    split_first,
    split_last,
    split_at,
    split_unaligned

public Mutable, Immutable, DelimitedIterator, truncate

using LightBoundsErrors: checkbounds_lightboundserror, throw_lightboundserror

"""
Trait struct, only used in the mutability parameter of `MemoryView`
"""
struct Mutable end

"""
Trait struct, only used in the mutability parameter of `MemoryView`
"""
struct Immutable end

"""
    MemoryView{T, M} <: DenseVector{T}

`MemoryView` is the common representation of dense, contiguous, Julia-owned
data. `String`, `Vector`, `Memory` and other memory-backed types should have
a fast `MemoryView` constructor.

Construct from memory-backed values `x` with `MemoryView(x)`.

`MemoryView`s are guaranteed to point to contiguous, valid CPU memory,
except where they have size zero.

The parameter `M` controls the mutability of the memory view,
and may be `Mutable` or `Immutable`, corresponding to the
the aliases `MutableMemoryView{T}` and `ImmutableMemoryView{T}`.

# Examples
```jldoctest
julia> v = view([1, 2, 3, 4], 2:3);

julia> mem = MemoryView(v)
2-element MutableMemoryView{Int64}:
 2
 3

julia> MemoryView(codeunits("abc")) isa ImmutableMemoryView{UInt8}
true
```

# Extended help
New types `T` which are backed by dense memory should implement:
* `MemoryView(x::T)` to construct a memory view from `x`. This should
   always return a `MutableMemoryView` when the memory of `x` is mutable.
If `MemoryView(x)` is implemented, then `ImmutableMemoryView(x)` will
automatically work, even if `MemoryView(x)` returns a mutable view.

It is not possible to mutate memory though an `ImmutableMemoryView`, but the existence
of the view does not protect the same memory from being mutated though another
variable, or through explicitly unsafe functions.

The precise memory layout of the data in a `MemoryView` follows that of `Memory`.
This includes the fact that some elements in the array, such as  `String`s,
may be stored as pointers, and [isbits Union optimisations]
(https://docs.julialang.org/en/v1/devdocs/isbitsunionarrays/).

`MemoryView{T, M}` is guaranteed to be immutable and to have the same size as a
`MemoryRef{T}` and an `Int` combined.

"""
struct MemoryView{T, M <: Union{Mutable, Immutable}} <: DenseVector{T}
    # If the memview is empty, there is no guarantees where the ref points to
    ref::MemoryRef{T}
    len::Int

    global function unsafe_new_memoryview(::Type{M}, ref::MemoryRef{T}, len::Int) where {M, T}
        (M === Mutable || M === Immutable) ||
            error("Parameter M must be Mutable or Immutable")
        return new{T, M}(ref, len)
    end
end

const MutableMemoryView{T} = MemoryView{T, Mutable}
const ImmutableMemoryView{T} = MemoryView{T, Immutable}

"""
    unsafe_from_parts(ref::MemoryRef{T}, len::Int)::MutableMemoryView{T}

Create a mutable memory view from its parts.

**Safety:** Callers are responsible to ensure that:
* `len` is not negative
* All indices `i in 1:len` are valid for `ref` (i.e. `memoryref(ref, i)` would
  not throw)
* If `ref` is derived from immutable memory, it is the caller's responsibility
  to ensure that the resulting view is not mutated.
  For example, `ref` may be derived from a `String`, and mutating `String`s in
  Julia may result in undefined behaviour.
  In these cases, the caller may convert to `ImmutableMemoryView` immediately
  after calling this function.

# Examples
```jldoctest
julia> v = [1,2,3,4];

julia> ref = Base.cconvert(Ptr, v);

julia> view = unsafe_from_parts(ref, 3)
3-element MutableMemoryView{Int64}:
 1
 2
 3
```
"""
function unsafe_from_parts(ref::MemoryRef, len::Int)
    return unsafe_new_memoryview(Mutable, ref, len)
end

"""
    Base.memoryref(x::MutableMemoryView{T})::MemoryRef{T}

Get the `MemoryRef` of `x`. This reference is guaranteed to be inbounds,
except if `x` is empty, where it may point to one element past the end.

To get the `MemoryRef` from an immutable `MemoryView`, use
[`unsafe_memoryref`](@ref)
"""
Base.memoryref(@nospecialize(x::MutableMemoryView)) = x.ref

"""
    unsafe_memoryref(x::MemoryView{T})::MemoryRef{T}

Same as `memoryref(::MutableMemoryView)`, but also works for `ImmutableMemoryView`.
Users must ensure only to mutate the resulting `MemoryRef` if `x` does not alias
memory assumed to be immutable.

!!! warning
    As the resulting `MemoryRef` is mutable, users must take care that this
    function allows mutation of memory assumed to be immutable, such as
    the memory backing a `String`. This can cause undefined behavour.
"""
unsafe_memoryref(@nospecialize(x::MemoryView)) = x.ref

"""
    unsafe_memory(v::MemoryView{T})::Memory{T}

Get the entire `Memory` underlying `v`, including elements outside the view.
The returned memory is shared with `v`, not copied.

!!! warning
    As the resulting `Memory` is mutable, users must take care that this
    function allows mutation of memory assumed to be immutable, such as
    the memory backing a `String`. This can cause undefined behaviour.
"""
unsafe_memory(::MemoryView)

# The parent method for MemoryRef was added in 1.12.
@static if VERSION < v"1.12.0-DEV.966"
    unsafe_memory(@nospecialize(v::MemoryView)) = v.ref.mem
else
    unsafe_memory(@nospecialize(v::MemoryView)) = parent(v.ref)
end

_get_mutability(::MemoryView{T, M}) where {T, M} = M

# Mutable mem views can turn into immutable ones, but not vice versa
ImmutableMemoryView(x) = ImmutableMemoryView(MemoryView(x)::MemoryView)
function ImmutableMemoryView(x::MemoryView)
    return unsafe_new_memoryview(Immutable, x.ref, x.len)
end

# Constructors that allows users to specify eltype explicitly, e.g.
# ImmutableMemoryView{UInt8}([0x01])
# With mutability specified
function MemoryView{T, M}(x) where {T, M}
    return (MemoryView{X, M} where {X})(x)::MemoryView{T, M}
end

# With mutability unspecified
function MemoryView{T}(x) where {T}
    return MemoryView(x)::MemoryView{T}
end

include("construction.jl")
include("basic.jl")
include("delimited.jl")
include("base_arrays.jl")
include("io.jl")

end # module
