function Base.setindex!(v::MutableMemoryView{T}, x, i::Int) where {T}
    @boundscheck checkbounds(v, i)
    xT = x isa T ? x : convert(T, x)::T
    ref = @inbounds memoryref(v.ref, i)
    @inbounds ref[] = xT
    return v
end

# The parent method for memoryref was added in 1.12. In versions before that,
# it can be accessed by reaching into internals.
@static if VERSION < v"1.12"
    Base.parent(v::MemoryView) = v.ref.mem
else
    Base.parent(v::MemoryView) = parent(v.ref)
end

Base.size(v::MemoryView) = (v.len,)
Base.IndexStyle(::Type{<:MemoryView}) = Base.IndexLinear()

function Base.iterate(x::MemoryView, i::Int = 1)
    ((i - 1) % UInt) < (length(x) % UInt) || return nothing
    return (@inbounds x[i], i + 1)
end

# Base.memoryindex exists in Julia 1.13 onwards.
@static if VERSION < v"1.13"
    function Base.parentindices(x::MemoryView)
        elz = Base.elsize(x)
        return if iszero(elz)
            offset = Int(x.ref.ptr_or_offset)
            ((1 + offset):(x.len + offset),)
        else
            byte_offset = pointer(x.ref) - pointer(x.ref.mem)
            elem_offset = div(byte_offset % UInt, elz % UInt) % Int
            ((elem_offset + 1):(elem_offset + x.len),)
        end
    end
else
    function Base.parentindices(x::MemoryView)
        start = Base.memoryindex(x.ref)
        return (start:(start + length(x) - 1),)
    end
end

function Base.copy(x::MemoryView{T, M}) where {T, M}
    isempty(x) && return x
    newmem = @inbounds x.ref.mem[only(parentindices(x))]
    return unsafe_new_memoryview(M, memoryref(newmem), x.len)
end

function Base.checkbounds(v::MemoryView, is...)
    checkbounds_lightboundserror(v, is...)
end

function Base.getindex(v::MemoryView, i::Integer)
    @boundscheck checkbounds(v, i)
    ref = @inbounds memoryref(v.ref, i)
    return @inbounds ref[]
end

function Base.similar(::MemoryView{T1, M}, ::Type{T2}, dims::Tuple{Int}) where {T1, T2, M}
    len = only(dims)
    memory = Memory{T2}(undef, len)
    # Note: `similar` needs to construct a mutable memory view, even if the input
    # type is not mutable
    return MemoryView(memory)
end

function Base.empty(::MemoryView{T1, M}, ::Type{T2}) where {T1, T2, M}
    return unsafe_new_memoryview(M, memoryref(Memory{T2}()), 0)
end

Base.empty(::Type{<:MemoryView{E, M}}) where {E, M} = unsafe_new_memoryview(M, memoryref(Memory{E}()), 0)
Base.pointer(x::MemoryView{T}) where {T} = Ptr{T}(pointer(x.ref))
Base.unsafe_convert(::Type{Ptr{T}}, v::MemoryView{T}) where {T} = pointer(v)
Base.cconvert(::Type{<:Ptr{T}}, v::MemoryView{T}) where {T} = v.ref
Base.elsize(::Type{<:MemoryView{T}}) where {T} = Base.elsize(Memory{T})
Base.sizeof(x::MemoryView) = Base.elsize(typeof(x)) * length(x)
Base.strides(::MemoryView) = (1,)

# For two distinct element types, they can't alias
Base.mightalias(::MemoryView, ::MemoryView) = false

function Base.mightalias(a::MemoryView{T}, b::MemoryView{T}) where {T}
    (isempty(a) | isempty(b)) && return false
    # We can't compare the underlying Memory with === to add a fast path here,
    # because users can create aliasing, but distinct Memory using unsafe_wrap.
    (p1, p2) = (pointer(a), pointer(b))
    elz = Base.elsize(a)
    return if p1 < p2
        p1 + length(a) * elz > p2
    else
        p2 + length(b) * elz > p1
    end
end

# We don't include strings here because this union is used for mightalias
# checks, which are done implicitly, and we don't want to construct memory
# views from strings implicitly, since that currently allocates.
const KNOWN_MEM_BACKED = Union{Array, Memory, ContiguousSubArray}

function Base.mightalias(a::MemoryView, b::KNOWN_MEM_BACKED)
    return Base.mightalias(a, ImmutableMemoryView(b))
end

function Base.mightalias(a::KNOWN_MEM_BACKED, b::MemoryView)
    return Base.mightalias(ImmutableMemoryView(a), b)
end

function Base.getindex(v::MemoryView{T, M}, idx::AbstractUnitRange) where {T, M}
    # This branch is necessary, because the memoryref can't point out of bounds.
    # So if the user gives an empty slice that is out of bounds, the boundscheck
    # may pass, but the memoryref construction will be OOB.
    isempty(idx) && return unsafe_new_memoryview(M, memoryref(v.ref.mem), 0)
    @boundscheck checkbounds(v, idx)
    newref = @inbounds memoryref(v.ref, Int(first(idx))::Int)
    return unsafe_new_memoryview(M, newref, Int(length(idx))::Int)
end

function Base.getindex(v::MemoryView{T, M}, idx::UnitRange{UInt}) where {T, M}
    isempty(idx) && return unsafe_new_memoryview(M, v.ref, 0)
    @boundscheck checkbounds(v, idx)
    newref = @inbounds memoryref(v.ref, first(idx) % Int)
    return unsafe_new_memoryview(M, newref, length(idx) % Int)
end

# Faster method, because we don't need to create a new memoryref, and also don't
# need to handle the empty case.
function Base.getindex(v::MemoryView{T, M}, idx::Base.OneTo) where {T, M}
    @boundscheck checkbounds(v, idx)
    return unsafe_new_memoryview(M, v.ref, last(idx))
end

Base.getindex(v::MemoryView, ::Colon) = v
Base.@propagate_inbounds Base.view(v::MemoryView, idx::AbstractUnitRange) = v[idx]

# Efficient way to get `mem[1:include_last]`.
# include_last must be in 0:length(mem)
function truncate(mem::MemoryView{T, M}, include_last::Integer) where {T, M}
    lst = Int(include_last)::Int
    @boundscheck if (lst % UInt) > length(mem) % UInt
        throw_lightboundserror(mem, lst)
    end
    return unsafe_new_memoryview(M, mem.ref, lst)
end

# Efficient way to get `mem[from:end]`.
# From must be in 1:length(mem).
function truncate_start_nonempty(mem::MemoryView{T, M}, from::Integer) where {T, M}
    frm = Int(from)::Int
    @boundscheck if ((frm - 1) % UInt) ≥ length(mem) % UInt
        throw_lightboundserror(mem, frm)
    end
    newref = @inbounds memoryref(mem.ref, frm)
    return unsafe_new_memoryview(M, newref, length(mem) - frm + 1)
end

# Efficient way to get `mem[from:end]`.
# From must be in 1:length(mem)+1.
function truncate_start(mem::MemoryView{T, M}, from::Integer) where {T, M}
    frm = Int(from)::Int
    @boundscheck if ((frm - 1) % UInt) > length(mem) % UInt
        throw_lightboundserror(mem, frm)
    end
    frm == 1 && return mem
    newref = @inbounds memoryref(mem.ref, frm - (from == length(mem) + 1))
    return unsafe_new_memoryview(M, newref, length(mem) - frm + 1)
end

function Base.unsafe_copyto!(dst::MutableMemoryView{T}, src::MemoryView{T}) where {T}
    iszero(length(src)) && return dst
    @inbounds unsafe_copyto!(dst.ref, src.ref, length(src) % UInt)
    return dst
end

function Base.copy!(dst::MutableMemoryView{T}, src::MemoryView{T}) where {T}
    @boundscheck length(dst) == length(src) || throw_lightboundserror(dst, eachindex(src))
    return unsafe_copyto!(dst, src)
end

function Base.copyto!(dst::MutableMemoryView{T}, src::MemoryView{T}) where {T}
    @boundscheck length(dst) ≥ length(src) || throw_lightboundserror(dst, eachindex(src))
    return unsafe_copyto!(dst, src)
end

function Base.fill!(v::MutableMemoryView{UInt8}, x::Integer)
    xT = convert(UInt8, x)::UInt8
    isempty(v) && return v
    GC.@preserve v @ccall memset(
        pointer(v)::Ptr{Nothing},
        Int32(xT)::Cint,
        (length(v) % UInt)::Csize_t
    )::Cvoid
    return v
end

# Optimised methods that don't boundscheck
function Base.findnext(p::Function, mem::MemoryView, start::Integer)
    i = Int(start)::Int
    @boundscheck (i < 1 && throw_lightboundserror(mem, i))
    @inbounds while i <= length(mem)
        p(mem[i]) && return i
        i += 1
    end
    return nothing
end

# The following two methods could be collapsed, but they aren't for two reasons:
# * To prevent ambiguity with Base
# * Because we DON'T want this code to run with MemoryView{Union{UInt8, Int8}}.
#   The latter might not be an issue since I don't think it's possible to construct
#   a Fix2 with a non-concrete type, but I'm not sure.
function Base.findnext(
        p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, UInt8},
        mem::MemoryView{UInt8},
        start::Integer,
    )
    return _findnext(mem, p.x, start)
end

function Base.findnext(
        p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, Int8},
        mem::MemoryView{Int8},
        start::Integer,
    )
    return _findnext(mem, p.x, start)
end

function Base.findnext(
        ::typeof(iszero),
        mem::Union{MemoryView{Int8}, MemoryView{UInt8}},
        i::Integer,
    )
    return _findnext(mem, zero(eltype(mem)), i)
end

Base.@propagate_inbounds function _findnext(
        mem::MemoryView{T},
        byte::T,
        start::Integer,
    ) where {T <: Union{UInt8, Int8}}
    start = Int(start)::Int
    @boundscheck(start < 1 && throw_lightboundserror(mem, start))
    start > length(mem) && return nothing
    im = @inbounds truncate_start_nonempty(ImmutableMemoryView(mem), start)
    v_ind = @something memchr(im, byte) return nothing
    return v_ind + start - 1
end

function memchr(mem::ImmutableMemoryView{T}, byte::T) where {T <: Union{Int8, UInt8}}
    isempty(mem) && return nothing
    GC.@preserve mem begin
        ptr = Ptr{UInt8}(pointer(mem))
        p = @ccall memchr(
            ptr::Ptr{UInt8},
            (byte % UInt8)::Cint,
            length(mem)::Csize_t,
        )::Ptr{Cvoid}
    end
    return p == C_NULL ? nothing : (p - ptr) % Int + 1
end

function Base.findprev(p::Function, mem::MemoryView, start::Integer)
    i = Int(start)::Int
    @boundscheck (i > length(mem) && throw_lightboundserror(mem, i))
    @inbounds while i > 0
        p(mem[i]) && return i
        i -= 1
    end
    return nothing
end

function Base.findprev(
        p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, UInt8},
        mem::MemoryView{UInt8},
        start::Integer,
    )
    return _findprev(mem, p.x, start)
end

function Base.findprev(
        p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, Int8},
        mem::MemoryView{Int8},
        start::Integer,
    )
    return _findprev(mem, p.x, start)
end

function Base.findprev(
        ::typeof(iszero),
        mem::Union{MemoryView{Int8}, MemoryView{UInt8}},
        i::Integer,
    )
    return _findprev(mem, zero(eltype(mem)), i)
end

Base.@propagate_inbounds function _findprev(
        mem::MemoryView{T},
        byte::T,
        start::Integer,
    ) where {T <: Union{UInt8, Int8}}
    start = Int(start)::Int
    @boundscheck (start > length(mem) && throw_lightboundserror(mem, start))
    start < 1 && return nothing
    im = @inbounds truncate(ImmutableMemoryView(mem), start)
    return memrchr(im, byte)
end

function memrchr(mem::ImmutableMemoryView{T}, byte::T) where {T <: Union{Int8, UInt8}}
    isempty(mem) && return nothing
    GC.@preserve mem begin
        ptr = Ptr{UInt8}(pointer(mem))
        p = @ccall memrchr(
            ptr::Ptr{UInt8},
            (byte % UInt8)::Cint,
            length(mem)::Csize_t,
        )::Ptr{Cvoid}
    end
    return p == C_NULL ? nothing : (p - ptr) % Int + 1
end

const BitsTypes =
    (Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, Char)
const Bits = Union{BitsTypes...}
const BitMemory = Union{map(T -> MemoryView{T}, BitsTypes)...}

# This dispatch makes sure that, if they have the same element bitstype, but the views
# are of different types due to mutability, we still dispatch to the correct methpd.
Base.:(==)(a::ImmutableMemoryView, b::MutableMemoryView) = a == ImmutableMemoryView(b)
Base.:(==)(a::MutableMemoryView, b::ImmutableMemoryView) = ImmutableMemoryView(a) == b

# Make sure to only dispatch if it's the exact same memory type.
function Base.:(==)(a::Mem, b::Mem) where {Mem <: BitMemory}
    length(a) == length(b) || return false
    (eltype(a) === Union{} || Base.issingletontype(eltype(a))) && return true
    a.ref === b.ref && return true
    nbytes = length(a) * sizeof(eltype(a))
    GC.@preserve a b begin
        aptr = Ptr{Nothing}(pointer(a))
        bptr = Ptr{Nothing}(pointer(b))
        y = @ccall memcmp(aptr::Ptr{Nothing}, bptr::Ptr{Nothing}, nbytes::Csize_t)::Cint
    end
    return iszero(y)
end

function Base.cmp(a::MemoryView{UInt8}, b::MemoryView{UInt8})
    y = if a.ref !== b.ref
        GC.@preserve a b begin
            aptr = Ptr{Nothing}(pointer(a))
            bptr = Ptr{Nothing}(pointer(b))
            @ccall memcmp(
                aptr::Ptr{Nothing},
                bptr::Ptr{Nothing},
                min(length(a), length(b))::Int,
            )::Cint
        end
    else
        Cint(0)
    end
    return iszero(y) ? sign(length(a) - length(b)) : Int(y)
end

function Base.reverse!(mem::MutableMemoryView)
    start = 1
    stop = length(mem)
    @inbounds for i in 1:(div(length(mem) % UInt, 2) % Int)
        (mem[start], mem[stop]) = (mem[stop], mem[start])
        start += 1
        stop -= 1
    end
    return mem
end

function Base.reverse(mem::MemoryView)
    cp = similar(mem)
    stop = length(cp) + 1
    @inbounds for i in 1:length(cp)
        cp[i] = mem[stop - i]
    end
    return if mem isa MutableMemoryView
        cp
    else
        ImmutableMemoryView(cp)
    end
end

struct ReverseMemoryView{T}
    # I can't think of a reason to allow mutable memory views here
    mem::ImmutableMemoryView{T}
end

function Iterators.reverse(mem::MemoryView{T}) where {T}
    return ReverseMemoryView{T}(ImmutableMemoryView(mem))
end
Iterators.reverse(x::ReverseMemoryView) = x.mem

Base.length(x::ReverseMemoryView) = length(x.mem)
Base.eltype(::Type{ReverseMemoryView{T}}) where {T} = T

function Base.iterate(x::ReverseMemoryView, state = length(x))
    iszero(state) && return nothing
    return (@inbounds(x.mem[state]), state - 1)
end

"""
    split_first(v::MemoryView{T}) -> Tuple{T, MemoryView{T}}

Return the first element of `v` and all other elements as a new memory view.

This function will throw a `LightBoundsError` if `v` is empty.

See also: [`split_last`](@ref)

# Examples
```jldoctest
julia> v = MemoryView([0x01, 0x02, 0x03]);

julia> split_first(v)
(0x01, UInt8[0x02, 0x03])

julia> split_first(v[1:1])
(0x01, UInt8[])

julia> split_first(v[1:0])
ERROR: LightBoundsErrors.LightBoundsError: out-of-bounds indexing: `collection[1]`, where:
[...]
```
"""
function split_first(v::MemoryView)
    @boundscheck checkbounds(v, 1)
    return (@inbounds(v[1]), @inbounds(truncate_start(v, 2)))
end

"""
    split_last(v::MemoryView{T}) -> Tuple{T, MemoryView{T}}

Return the last element of `v` and all other elements as a new memory view.

This function will throw a `LightBoundsError` if `v` is empty.

See also: [`split_first`](@ref)

# Examples
```jldoctest
julia> v = MemoryView([0x01, 0x02, 0x03]);

julia> split_last(v)
(0x03, UInt8[0x01, 0x02])

julia> split_last(v[1:1])
(0x01, UInt8[])

julia> split_last(v[1:0])
ERROR: LightBoundsErrors.LightBoundsError: out-of-bounds indexing: `collection[1]`, where:
[...]
```
"""
function split_last(v::MemoryView)
    @boundscheck checkbounds(v, 1)
    return (@inbounds(v[end]), @inbounds(truncate(v, length(v) - 1)))
end

"""
    split_at(v::T, i::Int) -> Tuple{T, T} where {T <: MemoryView}

Split a memory view into two at an index.

The first will contain all indices in `1:i-1`, the second `i:end`.
This function will throw a `LightBoundsError` if `i` is not in `1:end+1`.

# Examples
```jldoctest
julia> split_at(MemoryView([1,2,3,4,5]), 2)
([1], [2, 3, 4, 5])

julia> split_at(MemoryView(Int8[1, 2, 3]), 4)
(Int8[1, 2, 3], Int8[])
```
"""
function split_at(v::MemoryView, i::Int)
    @boundscheck if i ∉ 1:(lastindex(v) + 1)
        throw_lightboundserror(v, i)
    end
    return (@inbounds(truncate(v, i - 1)), @inbounds(truncate_start(v, i)))
end

"""
    split_unaligned(v::T, ::Val{A}) -> Tuple{T, T} where {T <: MemoryView}

Split memory view `v` into two views `a` and `b`, where `a` is the smallest prefix of `v`
that guarantees the starting memory address of `b` is is aligned to the integer value `A`.
`A` must be a normal bit-integer, and a power of two in the range 1:64.

If `v` is empty or already aligned, `a` will be empty.
If no elements of `v` is aligned, `b` will be empty and `a` will be equal to `v`.
The element type of `v` must be a bitstype.

!!! warning
    When using this function, make sure to `GC.@preserve v`, to make sure Julia
    does not move `v` in memory.

# Examples:
```
julia> split_unaligned(MemoryView(Int16[1, 2, 3]), Val(8))
(Int16[], Int16[1, 2, 3])

julia> split_unaligned(MemoryView(collect(0x01:0x20))[6:13], Val(8))
(UInt8[0x06, 0x07, 0x08], UInt8[0x09, 0x0a, 0x0b, 0x0c, 0x0d])
```
"""
function split_unaligned(v::MemoryView{T, M}, ::Val{A}) where {A, T, M}
    isbitstype(eltype(v)) || error("Alignment can only be computed for views of bitstypes")
    A isa Bits || error("Invalid alignment")
    in(A, (1, 2, 4, 8, 16, 32, 64)) || error("Invalid alignment")
    alignment = A % UInt
    mask = alignment - 1
    sz = Base.elsize(v)
    # Early return here to avoid division by zero: Size sz is statically known,
    # this will be compiled away
    iszero(sz) && return (unsafe_new_memoryview(M, v.ref, 0), v)
    unaligned_bytes = ((alignment - (UInt(pointer(v)) & mask)) & mask)
    n_elements = min(length(v), div(unaligned_bytes, sz % UInt) % Int)
    return @inbounds split_at(v, n_elements + 1)
end
