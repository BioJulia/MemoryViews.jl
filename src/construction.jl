MemoryView(v::MemoryView) = v

# Array and Memory
# Array with more than 1 dimension is not equal to the view, since they have different axes
MemoryKind(::Type{<:Array{T}}) where {T} = NotMemory()
MemoryKind(::Type{<:Vector{T}}) where {T} = IsMemory(MutableMemoryView{T})
MemoryKind(::Type{<:Memory{T}}) where {T} = IsMemory(MutableMemoryView{T})
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

# CodeUnits are semantically IsMemory, but only if the underlying string
# implements MemoryView, which some AbstractStrings may not
function MemoryKind(::Type{<:Base.CodeUnits{C, S}}) where {C, S}
    # Strings are normally immutable. New, mutable string types
    # would need to overload this method.
    return hasmethod(MemoryView, (S,)) ? IsMemory(ImmutableMemoryView{C}) : NotMemory()
end

MemoryView(s::Base.CodeUnits) = MemoryView(s.s)

# SubArrays
# This is quite tricky, because the indexing can be:
# * <: AbstractUnitRange
# * StepRange, with step == 1
# * Integer (zero-dimensional along one axis)

# It can also be multidimensional, but if so, all axes but the last one
# must span the entire dimension.
# And if so, we would need to compute the linear indices.

# For now, I've only accepted 1-D views.
const ContiguousSubArray = SubArray{
    T, N, P, I, true,
} where {T, N, P, I <: Union{Tuple{Integer}, Tuple{AbstractUnitRange}}}

MemoryKind(::Type{<:ContiguousSubArray{T, N, P}}) where {T, N, P} = MemoryKind(P)

function MemoryView(s::ContiguousSubArray{T, N, P}) where {T, N, P}
    memview = MemoryView(parent(s)::P)
    inds = only(parentindices(s))
    @boundscheck checkbounds_lightboundserror(memview.ref.mem, inds)
    return @inbounds memview[inds]
end
