module MemViews

export MemView,
    ImmutableMemView, MutableMemView, MemKind, IsMemory, NotMemory, inner, as_bytes

"""
    Unsafe

Trait object used to dispatch to unsafe methods.
The `MemViews.unsafe` instance is the singleton instance of this type.
"""
struct Unsafe end

"Singleton instance of the trait type `Unsafe`"
const unsafe = Unsafe()

"""
    MemView{T, M} <: DenseVector{T}

View into a `Memory{T}`.
`MemView`s are guaranteed to point to contiguous, valid CPU memory,
except where they have size zero.

The parameter `M` controls the mutability of the memory view,
and may be `:mutable` or `:immutable`, corresponding to the
The aliases `MutableMemView{T}` and `ImmutableMemView{T}`.

New types `T` which are backed by dense memory should implement:
* `MemView(x::T)` to construct a memory view from `x`. This should
   always return a mutable view if `x` is mutable.
* `MemKind(x::T)`, if `T` is semantically equal to its own memory view.
  Examples of this include `Vector`, `Memory`, and
  `Base.CodeUnits{UInt8, String}`. If so, `x == MemView(x)` should hold.

If `MemView(x)` is implemented, then `ImmutableMemView(x)` will
automatically work, even if `MemView(x)` returns a mutable view.

See also: `MemKind`
"""
struct MemView{T, M} <: DenseVector{T}
    # If the memview is empty, there is no guarantees where the ref points to
    ref::MemoryRef{T}
    len::Int

    function MemView{T, M}(ref::MemoryRef{T}, len::Int) where {T, M}
        if M !== :mutable && M !== :immutable
            error("Parameter M must be :mutable or :immutable")
        end
        new{T, M}(ref, len)
    end
end

const MutableMemView{T} = MemView{T, :mutable}
const ImmutableMemView{T} = MemView{T, :immutable}

# Mutable mem views can turn into immutable ones, but not vice versa
ImmutableMemView(x) = ImmutableMemView(MemView(x)::MemView)
ImmutableMemView(x::MutableMemView{T}) where {T} = ImmutableMemView{T}(x.ref, x.len)
ImmutableMemView(x::ImmutableMemView) = x
MutableMemView(x::MutableMemView) = x

"""
    MutableMemView(::Unsafe, x::MemView)

Convert a memory view into a mutable memory view.
Note that it may cause undefined behaviour, if supposedly immutable data
is observed to be mutated.
"""
MutableMemView(::Unsafe, x::MemView{T}) where {T} = MemView{T, :mutable}(x.ref, x.len)

# TODO: I believe this is allowed under C's rules. It may be UB under Julia's.
# In that case, these methods should just be removed.
"""
    as_bytes(::Unsafe, x::MemView)

Convert a `MemView{T}` into a corresponding `MemView{UInt8}`.
If `T` is not a bitstype, throw an error.
Mutating the resulting view modifies any instances of `T`
in the original memory, which may cause some user-defined invariants
to no longer hold for these instances.
Note that this is allowed under C's strict aliasing rules.
"""
function as_bytes(::Unsafe, v::MemView{T, M}) where {T, M}
    isbitstype(T) || error("as_bytes only works on bitstypes element type")
    sz = sizeof(v)
    GC.@preserve v mem = unsafe_wrap(Memory{UInt8}, Ptr{UInt8}(pointer(v)), sz)
    MemView{UInt8, M}(memoryref(mem), sz)
end
as_bytes(::Unsafe, v::MemView{UInt8}) = v

"""
    MemKind

Trait object used to signal if values of a type is semantically equal to their own `MemView`.
If so, `MemKind(T)` should return an instance of `IsMemory`,
else `NotMemory()`. The default implementation `MemKind(::Type)` returns `NotMemory()`.

If `MemKind(T) isa IsMemory{M}`, the following must hold:
1. `M` is a concrete subtype of `MemView`. To obtain `M` from an `m::IsMemory{M}`,
    use `inner(m)`.
2. `MemView(T)` is a valid instance of `M`.
3. `inner(MemKind(typeof(x))) == typeof(MemView(x))`.

Some objects can be turned into `MemView` without being `IsMemory`.
For example, `MemView(::String)` returns a valid `MemView` even though
`MemKind(String) === NotMemory()`.
This is because strings have different semantics than mem views - the latter
is a dense `AbstractArray` while strings are not, and so the third requirement
`MemView(x::String) == x` does not hold.

See also: `MemView`
"""
abstract type MemKind end

"""
    NotMemory <: MemKind

See `MemKind`
"""
struct NotMemory <: MemKind end

"""
    IsMemory{T <: MemView} <: MemKind

See `MemKind`
"""
struct IsMemory{T <: MemView} <: MemKind
    function IsMemory{T}() where {T}
        isconcretetype(T) || error("In IsMemory{T}, T must be concrete")
        new{T}()
    end
end
IsMemory(T::Type{<:MemView}) = IsMemory{T}()

"""
    inner(::IsMemory{T})

Return `T` from an `IsMemory{T}`.
"""
inner(::IsMemory{T}) where {T} = T

MemKind(::Type) = NotMemory()
MemKind(::Type{Union{}}) = NotMemory()

include("construction.jl")
include("basic.jl")

end # module
