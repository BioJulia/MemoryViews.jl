module MemoryViews

export MemoryView,
    ImmutableMemoryView,
    MutableMemoryView,
    MemoryKind,
    IsMemory,
    NotMemory,
    inner,
    split_each,
    unsafe_from_parts,
    split_first,
    split_last,
    split_at,
    split_unaligned

public Mutable, Immutable, DelimitedIterator

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

View into a `Memory{T}`.
Construct from memory-backed values `x` with `MemoryView(x)`.

`MemoryView`s are guaranteed to point to contiguous, valid CPU memory,
except where they have size zero.

The parameter `M` controls the mutability of the memory view,
and may be `Mutable` or `Immutable`, corresponding to the
the aliases `MutableMemoryView{T}` and `ImmutableMemoryView{T}`.

See also: `MemoryKind`

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
* `MemoryKind(x::T)`, if `T` is semantically equal to its own memory view.
  Examples of this include `Vector`, `Memory`, and
  `Base.CodeUnits{UInt8, String}`. If so, `x == MemoryView(x)` should hold.

If `MemoryView(x)` is implemented, then `ImmutableMemoryView(x)` will
automatically work, even if `MemoryView(x)` returns a mutable view.

It is not possible to mutate memory though an `ImmutableMemoryView`, but the existence
of the view does not protect the same memory from being mutated though another
variable, or through explicitly unsafe functions.

The precise memory layout of the data in a `MemoryView` follows that of `Memory`.
This includes the fact that some elements in the array, such as  `String`s,
may be stored as pointers, and [isbits Union optimisations]
(https://docs.julialang.org/en/v1/devdocs/isbitsunionarrays/).

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
  to ensure that mutating the memory does not result in undefined behaviour.
  For example, `ref` may be derived from a `String`, and mutating `String`s in
  Julia may result in undefined behaviour.

# Examples
```jldoctest
julia> v = [1,2,3,4];

julia> ref = Base.memoryref(v);

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
    Base.memoryref(x::MemoryView{T})::MemoryRef{T}

Get the `MemoryRef` of `x`. This reference is guaranteed to be inbounds,
except if `x` is empty, where it may point to one element past the end.
"""
Base.memoryref(x::MemoryView) = x.ref

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

"""
    MemoryKind

Trait object used to signal if values of a type is semantically equal to their own `MemoryView`.
If so, `MemoryKind(T)` should return an instance of `IsMemory`,
else `NotMemory()`. The default implementation `MemoryKind(::Type)` returns `NotMemory()`.

If `MemoryKind(T) isa IsMemory{M}`, the following must hold:
1. `M` is a concrete subtype of `MemoryView`. To obtain `M` from an `m::IsMemory{M}`,
    use `inner(m)`.
2. `MemoryView(::T)` is a valid instance of `M` (except in cases where there can be invalid
   instances of `T` that instead errors, e.g. uninitialized instances).
3. `MemoryView(x) == x` for all instances `x::T`

Some objects can be turned into `MemoryView` without being `IsMemory`.
For example, `MemoryView(::String)` returns a valid `MemoryView` even though
`MemoryKind(String) === NotMemory()`.
This is because strings have different semantics than memory views - the latter
is a dense `AbstractArray` while strings are not, and so the fourth requirement
`MemoryView(x::String) == x` does not hold.

See also: [`MemoryView`](@ref)
"""
abstract type MemoryKind end

"""
    NotMemory <: MemoryKind

See: [`MemoryKind`](@ref)
"""
struct NotMemory <: MemoryKind end

"""
    IsMemory{T <: MemoryView} <: MemoryKind

See: [`MemoryKind`](@ref)
"""
struct IsMemory{T <: MemoryView} <: MemoryKind
    function IsMemory{T}() where {T}
        isconcretetype(T) || error("In IsMemory{T}, T must be concrete")
        return new{T}()
    end
end
IsMemory(T::Type{<:MemoryView}) = IsMemory{T}()

"""
    inner(::IsMemory{T})

Return `T` from an `IsMemory{T}`.

See: [`MemoryKind`](@ref)
"""
inner(::IsMemory{T}) where {T} = T

MemoryKind(::Type) = NotMemory()
MemoryKind(::Type{Union{}}) = NotMemory()
MemoryKind(::Type{T}) where {T <: MemoryView} = IsMemory(T)

include("construction.jl")
include("basic.jl")
include("delimited.jl")
include("base_arrays.jl")
include("io.jl")

end # module
