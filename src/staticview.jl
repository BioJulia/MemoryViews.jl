# Most methods for StaticView are implemented in basic.jl, by being defined on
# MemoryVector. This file contains methods specific to StaticView, either because
# their return type depends on N, or because they can be implemented faster
# when N is known.

# Construction
@noinline function throw_length_mismatch(expected::Int, actual::Int)
    throw(DimensionMismatch(lazy"Expected input of length $expected, got length $actual"))
end

function check_input_length(v::MemoryView, N::Int)
    length(v) == N || throw_length_mismatch(N, length(v))
    return nothing
end

# Note: These constructors are @inline and not @propagate_inbounds, such that
# @inbounds only removes the length check, and not any boundschecks in
# the MemoryView constructor of x.
@inline function StaticView{N}(x) where {N}
    v = MemoryView(x)::MemoryView
    check_length_parameter(N)
    @boundscheck check_input_length(v, N)
    return unsafe_new_staticview(_get_mutability(v), Val(N), v.ref)
end

@inline function StaticView{N, T}(x) where {N, T}
    v = MemoryView(x)::MemoryView{T}
    check_length_parameter(N)
    @boundscheck check_input_length(v, N)
    return unsafe_new_staticview(_get_mutability(v), Val(N), v.ref)
end

# Enables e.g. `ImmutableStaticView{N}(x)`, with the element type inferred.
@inline function (StaticView{N, T, M} where {T})(x) where {N, M}
    v = (MemoryView{X, M} where {X})(x)::MemoryView{<:Any, M}
    check_length_parameter(N)
    @boundscheck check_input_length(v, N)
    return unsafe_new_staticview(M, Val(N), v.ref)
end

@inline function StaticView{N, T, M}(x) where {N, T, M}
    v = (MemoryView{X, M} where {X})(x)::MemoryView{T, M}
    check_length_parameter(N)
    @boundscheck check_input_length(v, N)
    return unsafe_new_staticview(M, Val(N), v.ref)
end

Base.@propagate_inbounds function StaticView{N}(x::MemoryVector, start::Integer) where {N}
    v = MemoryView(x)
    check_length_parameter(N)
    start = Int(start)::Int
    len = length(v)
    @boundscheck if !((N ≤ len) & (((start - 1) % UInt) ≤ ((len - N) % UInt)))
        throw_lightboundserror(v, start:(start + N - 1))
    end
    # If N is zero, start may be one past the end, where we can't create a memoryref.
    ref = iszero(N) ? v.ref : @inbounds memoryref(v.ref, start)
    return unsafe_new_staticview(_get_mutability(v), Val(N), ref)
end

StaticView(x::StaticView) = x

function ImmutableStaticView(x::StaticView{N}) where {N}
    return unsafe_new_staticview(Immutable, Val(N), x.ref)
end

function MutableStaticView(x::StaticView)
    x isa MutableStaticView || throw(ArgumentError("Cannot construct a mutable StaticView from an immutable view"))
    return x
end

MemoryView(v::StaticView{N, T, M}) where {N, T, M} = unsafe_new_memoryview(M, v.ref, N)

_get_mutability(::StaticView{N, T, M}) where {N, T, M} = M

# Accessors
unsafe_memoryref(@nospecialize(x::StaticView)) = x.ref
memory(v::MutableStaticView) = unsafe_memory(v)
unsafe_memory(v::StaticView) = unsafe_memory(MemoryView(v))

"""
    parent(v::StaticView{N, T, M})::MemoryView{T, M}

Get a `MemoryView` with the same mutability as `v`, encompassing the entire
underlying `Memory`.
"""
Base.parent(v::StaticView) = parent(MemoryView(v))

Base.size(::StaticView{N}) where {N} = (N,)
Base.length(::StaticView{N}) where {N} = N
Base.cconvert(::Type{<:Ptr{T}}, v::StaticView{<:Any, T}) where {T} = v

function Base.copy(x::StaticView{N, T, M}) where {N, T, M}
    iszero(N) && return x
    mem = Memory{T}(undef, N)
    @inbounds copyto!(MemoryView(mem), x)
    return unsafe_new_staticview(M, Val(N), memoryref(mem))
end

# Like for MemoryView, `similar` returns mutable views, even if the input is immutable.
function Base.similar(::StaticView{N}, ::Type{T}) where {N, T}
    return unsafe_new_staticview(Mutable, Val(N), memoryref(Memory{T}(undef, N)))
end

Base.similar(v::StaticView{N, T}) where {N, T} = similar(v, T)

# The length is only known at runtime, so return a MemoryView
function Base.similar(::StaticView, ::Type{T}, dims::Tuple{Int}) where {T}
    return MemoryView(Memory{T}(undef, only(dims)))
end

function Base.empty(::StaticView{N, T, M}, ::Type{E}) where {N, T, M, E}
    return unsafe_new_staticview(M, Val(0), memoryref(Memory{E}()))
end

function Base.empty(::Type{StaticView{N, T, M}}) where {N, T, M}
    return unsafe_new_staticview(M, Val(0), memoryref(Memory{T}()))
end

# Indexing with ranges returns a MemoryView, since the length of the range is
# generally not known at compile time
Base.@propagate_inbounds function Base.getindex(v::StaticView, idx::AbstractUnitRange)
    return MemoryView(v)[idx]
end

Base.getindex(v::StaticView, ::Colon) = v
Base.@propagate_inbounds Base.view(v::StaticView, idx::AbstractUnitRange) = v[idx]

Base.@propagate_inbounds function truncate(v::StaticView, include_last::Integer)
    return truncate(MemoryView(v), include_last)
end

# For small views, comparing elementwise is much faster than calling memcmp,
# because the compiler can turn the loop into a few SIMD instructions.
# For larger views, memcmp is faster.
# This must match the Bits union, since we only want to use
# bitwise equality for bitstypes.
function Base.:(==)(a::StaticView{N, T}, b::StaticView{N, T}) where {N, T <: Bits}
    if !isbitstype(T) || N * sizeof(T) > 128
        return MemoryView(a) == MemoryView(b)
    end
    eq = true
    @inbounds for i in 1:N
        eq &= a[i] === b[i]
    end
    return eq
end

function Base.reverse(v::StaticView{N, T, M}) where {N, T, M}
    cp = similar(v)
    @inbounds for i in 1:N
        cp[i] = v[N + 1 - i]
    end
    return M === Mutable ? cp : ImmutableStaticView(cp)
end

Iterators.reverse(v::StaticView) = Iterators.reverse(MemoryView(v))

function split_first(v::StaticView{N, T, M}) where {N, T, M}
    @boundscheck checkbounds(v, 1)
    # If N is one, we can't create a memoryref to index 2.
    rest = if N == 1
        unsafe_new_staticview(M, Val(0), v.ref)
    else
        unsafe_new_staticview(M, Val(N - 1), @inbounds memoryref(v.ref, 2))
    end
    return (@inbounds(v[1]), rest)
end

function split_last(v::StaticView{N, T, M}) where {N, T, M}
    @boundscheck checkbounds(v, 1)
    return (@inbounds(v[N]), unsafe_new_staticview(M, Val(N - 1), v.ref))
end

Base.@propagate_inbounds split_at(v::StaticView, i::Integer) = split_at(MemoryView(v), i)

@inline split_unaligned(v::StaticView, alignment) = split_unaligned(MemoryView(v), alignment)
