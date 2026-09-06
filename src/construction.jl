MemoryView(@nospecialize(v::MemoryView)) = v

# Array and Memory
MemoryView(A::Memory{T}) where {T} = unsafe_new_memoryview(Mutable, memoryref(A), length(A))
MemoryView(A::Array{T}) where {T} = unsafe_new_memoryview(Mutable, Base.cconvert(Ptr, A), length(A))

# Strings
MemoryView(s::String) = ImmutableMemoryView(unsafe_wrap(Memory{UInt8}, s))
function MemoryView(s::SubString)
    v = ImmutableMemoryView(parent(s))
    return v[(s.offset + 1):(s.offset + ncodeunits(s))]
end

# Special implementation for SubString{String}, which we can guarantee never
# has out of bounds indices, unless the user previously misused @inbounds
function MemoryView(s::SubString{String})
    memview = MemoryView(parent(s))
    isempty(memview) && return memview
    newref = @inbounds memoryref(memview.ref, s.offset + 1)
    return unsafe_new_memoryview(Immutable, newref, ncodeunits(s))
end

MemoryView(s::Base.CodeUnits) = MemoryView(s.s)

# SubArrays with fast linear indexing are contiguous when either their first
# index is an AbstractUnitRange, or all their indices are scalars. Together
# with the index-tuple restriction, L == true guarantees unit linear stride.
const ContiguousSubArray = SubArray{
    T, N, P, I, true,
} where {
    T,
    N,
    P,
    I <: Union{Tuple{AbstractUnitRange, Vararg{Any}}, Tuple{Vararg{Integer}}},
}

first_parent_index(i::Integer) = i
first_parent_index(i) = first(i)

function MemoryView(s::ContiguousSubArray{T, N, P}) where {T, N, P}
    p = parent(s)::P
    memview = MemoryView(p)::MemoryView{T}
    isempty(s) && return memview[1:0]

    parent_inds = map(first_parent_index, parentindices(s))
    linear_inds = LinearIndices(p)
    parent_start = linear_inds[parent_inds...]
    start = Int(parent_start - first(linear_inds) + 1)::Int
    stop = start + length(s) - 1

    @boundscheck checkbounds_lightboundserror(memview, start:stop)
    return @inbounds memview[start:stop]
end
