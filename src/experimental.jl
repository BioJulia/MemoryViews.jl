# API which I'm not sure should be kept

export split_first, split_last, split_at, split_unaligned

"""
    split_first(v::MemoryView{T}) -> Tuple{T, MemoryView{T}}

Return the first element of `v` and all other elements as a new memory view.

This function will throw a `BoundsError` if `v` is empty.

See also: [`split_last`](@ref)

# Examples
```jldoctest
julia> v = MemoryView([0x01, 0x02, 0x03]);

julia> split_first(v)
(0x01, [0x02, 0x03])

julia> split_first(v[1:1])
(0x01, UInt8[])

julia> split_first(v[1:0])
ERROR: BoundsError: attempt to access 0-element MutableMemoryView{UInt8} at index [1]
[...]
```
"""
function split_first(v::MemoryView)
    @boundscheck checkbounds(v, 1)
    newref = @inbounds memoryref(v.ref, 1 + (length(v) > 1))
    fst = @inbounds v[1]
    (fst, typeof(v)(unsafe, newref, length(v) - 1))
end

"""
    split_last(v::MemoryView{T}) -> Tuple{T, MemoryView{T}}

Return the last element of `v` and all other elements as a new memory view.

This function will throw a `BoundsError` if `v` is empty.

See also: [`split_first`](@ref)

# Examples
```jldoctest
julia> v = MemoryView([0x01, 0x02, 0x03]);

julia> split_last(v)
(0x03, [0x01, 0x02])

julia> split_last(v[1:1])
(0x01, UInt8[])

julia> split_last(v[1:0])
ERROR: BoundsError: attempt to access 0-element MutableMemoryView{UInt8} at index [1]
[...]
```
"""
function split_last(v::MemoryView)
    @boundscheck checkbounds(v, 1)
    lst = @inbounds v[end]
    (lst, typeof(v)(unsafe, v.ref, length(v) - 1))
end

"""
    split_at(v::T, i::Int) -> Tuple{T, T} where {T <: MemoryView}

Split a memory view into two at an index.

The first will contain all indices in `1:i-1`, the second `i:end`.
This function will throw a `BoundsError` if `i` is not in `1:end+1`.

# Examples
```jldocstest
julia> split_at(MemoryView([1,2,3,4,5]), 2)
([1], [2, 3, 4, 5])

julia> split_at(MemoryView(Int8[1, 2, 3]), 4)
(Int8[1, 2, 3], Int8[])
```
"""
function split_at(v::MemoryView, i::Int)
    @boundscheck checkbounds(1:(lastindex(v) + 1), i)
    fst = typeof(v)(unsafe, v.ref, i - 1)
    ref = i > lastindex(v) ? v.ref : @inbounds memoryref(v.ref, i)
    lst = typeof(v)(unsafe, ref, length(v) - i + 1)
    (fst, lst)
end

"""
    split_unaligned(v::T, ::Val{A}) -> Tuple{T, T} where {T <: MemoryView}

Split memory view `v` into two views `a` and `b`, `a` is the smallest prefix of `v`
that gaurantees `b` is aligned to the integer value `A`.
`A` must be a normal bit-integer, and a power of two in the range 1:64.
If `v` is empty or already aligned, `a` will be empty.
The element type of `v` must be a bitstype.


# Examples:
```
julia> split_unaligned(MemoryView(Int16[1, 2, 3]), Val(8))
(Int16[], Int16[1, 2, 3])

julia> split_unaligned(MemoryView(collect(0x01:0x20))[6:13], Val(8))
(UInt8[0x06, 0x07, 0x08], UInt8[0x09, 0x0a, 0x0b, 0x0c, 0x0d])
```
"""
function split_unaligned(v::MemoryView, ::Val{A}) where {A}
    isbitstype(eltype(v)) || error("Alignment can only be computed for views of bitstypes")
    A isa Bits || error("Invalid alignment")
    in(A, (1, 2, 4, 8, 16, 32, 64)) || error("Invalid alignment")
    alignment = A % UInt
    mask = alignment - 1
    sz = Base.elsize(v)
    # Early return here to avoid division by zero: Size sz is statically known,
    # this will be compiled away
    iszero(sz) && return (typeof(v)(unsafe, v.ref, 0), v)
    unaligned_bytes = ((alignment - (UInt(pointer(v)) & mask)) & mask)
    n_elements = div(unaligned_bytes, sz % UInt) % Int
    @inbounds split_at(v, n_elements + 1)
end
