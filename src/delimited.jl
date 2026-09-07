"""
Iterator struct created by `split_each`.
Type and parameters are public, but otherwise the interface is defined by `split_each`, 
"""
struct DelimitedIterator{T, M, D}
    v::MemoryView{T, M}
    d::D
end

Base.IteratorSize(::Type{<:DelimitedIterator}) = Base.SizeUnknown()
Base.eltype(::Type{<:DelimitedIterator{T, M}}) where {T, M} = MemoryView{T, M}

function Base.iterate(d::DelimitedIterator, state::Int = 1)
    len = length(d.v)
    if state > len
        # Make sure that an empty view has no elements, but that a view ends
        # on the delimiter has an empty element at the end
        return if state > len + 1 || iszero(len)
            nothing
        else
            v = @inbounds d.v[len:(len - 1)]
            (v, state + 1)
        end
    end
    next = findnext(isequal(d.d), d.v, state)
    return if isnothing(next)
        (@inbounds d.v[state:len], len + 2)
    else
        (@inbounds d.v[state:(next - 1)], next + 1)
    end
end

"""
    split_each(data, x::T)

Return an iterator over memory-backed data `data` of eltype `T`.
Returns `MemoryView`s of the same elements as `data`, separated by by `x`.
Items are compared by `isequal`.
The delimiter `x` must satisfy `x isa T`.

An empty input `data` yields no elements. Empty elements are otherwise
yielded.

# Examples
```jldoctest
julia> split_each(b"abbc", UInt8('b')) |> collect |> print
ImmutableMemoryView{UInt8}[[0x61], [], [0x63]]

julia> split_each(b"babceb", UInt8('b')) |> collect |> print
ImmutableMemoryView{UInt8}[[], [0x61], [0x63, 0x65], []]

julia> split_each(UInt8[], UInt8('b')) |> collect |> print
MutableMemoryView{UInt8}[]
```
"""
function split_each(x, d::D) where {D}
    v = MemoryView(x)::MemoryView
    T = eltype(v)
    d isa T || error("Delimiter must be an instance of eltype(MemoryView(x))")
    M = _get_mutability(v)
    return DelimitedIterator{T, M, D}(v, d)
end
