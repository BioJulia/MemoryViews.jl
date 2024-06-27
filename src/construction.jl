MemoryView(v::MemoryView) = v

# Array and Memory
MemoryKind(::Type{<:Array{T}}) where {T} = IsMemory(MutableMemoryView{T})
MemoryKind(::Type{<:Memory{T}}) where {T} = IsMemory(MutableMemoryView{T})
MemoryView(A::Memory{T}) where {T} = MutableMemoryView{T}(unsafe, memoryref(A), length(A))
MemoryView(A::Array{T}) where {T} = MutableMemoryView{T}(unsafe, A.ref, length(A))

# Strings
MemoryView(s::String) = ImmutableMemoryView(unsafe_wrap(Memory{UInt8}, s))
function MemoryView(s::SubString)
    memview = MemoryView(parent(s))
    codesize = sizeof(codeunit(s))
    offset = codesize * s.offset
    len = s.ncodeunits * codesize
    mem = memview.ref.mem
    span = (offset + 1):len
    @boundscheck checkbounds(mem, span)
    @inbounds typeof(memview)(unsafe, memoryref(mem, offset + 1), s.ncodeunits * codesize)
end

# Special implementation for SubString{String}, which we can guarantee never
# has out of bounds indices, unless the user previously misused @inbounds
function MemoryView(s::SubString{String})
    memview = MemoryView(parent(s))
    newref = @inbounds memoryref(memview.ref, s.offset + 1)
    ImmutableMemoryView{UInt8}(unsafe, newref, s.ncodeunits)
end

# CodeUnits are semantically IsMemory, but only if the underlying string
# implements MemoryView, which some AbstractStrings may not
function MemoryKind(::Type{<:Base.CodeUnits{C, S}}) where {C, S}
    # Strings are normally immutable. New, mutable string types
    # would need to overload this method.
    hasmethod(MemoryView, (S,)) ? IsMemory(ImmutableMemoryView{C}) : NotMemory()
end

MemoryView(s::Base.CodeUnits) = MemoryView(s.s)

# SubArrays
# TODO: Identical to FastContiguousSubArray in Base
const ContiguousSubArray = SubArray{
    T,
    N,
    P,
    <:Union{Tuple{Vararg{Real}}, Tuple{AbstractUnitRange, Vararg{Any}}},
} where {T, N, P}

MemoryKind(::Type{<:ContiguousSubArray{T, N, P}}) where {T, N, P} = MemoryKind(P)
function MemoryView(s::ContiguousSubArray{T, N, P}) where {T, N, P}
    memview = MemoryView(parent(s)::P)
    inds = only(parentindices(s))
    @boundscheck checkbounds(memview.ref.mem, inds)
    @inbounds memview[inds]
end
