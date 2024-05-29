module MemViews

export MemView, ImmutableMemView, MutableMemView, MemKind, IsMemory, NotMemory, inner

#=
Should ReinterpretArray be supported?

That requires two params for memviews - for the outer and inner element type.
Dispatch on whether the two types are the same for get/setindex.
Needs a check for bitstypes only when instantiating with different parameters

Setindex is a little difficult - how to implement if the target type is smaller than the source type?
Docs of `unsafe_wrap` seems to suggest using that may cause UB.

I think this is too difficult.
=#

"""
    MemView{T, M} <: DenseVector{T}

View into a `Memory{T}`.
`MemView`s are guaranteed to point to contiguous, valid CPU memory,
except where they have size zero.

The parameter `M` controls the mutability of the memory view,
and may be `:mutable` or `:immutable`, corresponding to the
The aliases `MutableMemView{T}` and `ImmutableMemView{T}`.

New types `T` which are backed by dense memory should implement:
* `MemKind(x::T)`, if `T` is semantically a `DenseVector` equal
  to its own memory view. Examples of this include `Vector`, `Memory`, and
  `Base.CodeUnits{UInt8, String}`.
* `MemView(x::T)` to construct a memory view from `x`. This should
   always return a mutable view if `x` is mutable.

If `MemView(x)` is implemented, then `ImmutableMemView(x)` will
automatically work, even if `MemView(x)` returns a mutable view.

See also: `MemKind`
"""
struct MemView{T, M} <: DenseVector{T}
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
ImmutableMemView(x::MutableMemView{T}) where T = ImmutableMemView{T}(x.ref, x.len)
ImmutableMemView(x::ImmutableMemView) = x
MutableMemView(x::MutableMemView) = x

function ImmutableMemView(x)
    m = MemView(x)
    M = MemKind(x)
    M isa IsMemory && typeassert(m, inner(M))
    ImmutableMemView(m)
end

"""
    MemKind

Trait object used to signal if an instance can be represented by a `MemView`.
If so, `MemKind(x)` should return an instance of `IsMemory`,
else `NotMemory()`. The default implementation returns `NotMemory()`.

If `MemKind(x) isa IsMemory{T}`, the following must hold:
1. `T` is a concrete subtype of `MemView`. To obtain `T` from an `m::IsMemory{T}`,
    use `inner(m)`.
2. `MemView(x)` is a valid instance of `T`.
3. `MemView(x) == x`.

Some objects can be turned into `MemView` without being `IsMemory`.
For example, `MemView(::String)` returns a valid `MemView` even though
`MemKind(::String) === NotMemory()`.
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
    function IsMemory{T}() where T
        if !isconcretetype(T)
            error("In IsMemory{T}, T must be concrete")
        end
        new{T}()
    end
end
IsMemory(T::Type{<:MemView}) = IsMemory{T}()

"""
    inner(::IsMemory{T})

Return `T` from `IsMemory{T}`.
"""
inner(::IsMemory{T}) where T = T

MemKind(::Any) = NotMemory()

include("construction.jl")
include("basic.jl")

end # module