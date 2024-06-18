MemView(v::MemView) = v

# Array and Memory
MemKind(::Union{Array{T}, Memory{T}}) where {T} = IsMemory(MutableMemView{T})
MemView(A::Memory{T}) where {T} = MutableMemView{T}(memoryref(A), length(A))
MemView(A::Array{T}) where {T} = MutableMemView{T}(A.ref, length(A))

# Strings
MemView(s::String) = ImmutableMemView(unsafe_wrap(Memory{UInt8}, s))
function MemView(s::SubString)
    memview = MemView(parent(s))
    codesize = sizeof(codeunit(s))
    offset = codesize * s.offset
    len = s.ncodeunits * codesize
    mem = memview.ref.mem
    span = (offset + 1):len
    @boundscheck checkbounds(mem, span)
    @inbounds typeof(memview)(memoryref(mem, offset + 1), s.ncodeunits * codesize)
end

# Special implementation for SubString{String}, which we can guarantee never
# has out of bounds indices, unless the user previously misused @inbounds
function MemView(s::SubString{String})
    memview = MemView(parent(s))
    newref = @inbounds memoryref(memview.ref, s.offset + 1)
    ImmutableMemView{UInt8}(newref, s.ncodeunits)
end

# CodeUnits are semantically IsMemory, but only if the underlying string
# implements MemView, which some AbstractStrings may not
function MemKind(::Base.CodeUnits{C, S}) where {C, S}
    # Strings are normally immutable. New, mutable string types
    # would need to overload this method.
    hasmethod(MemView, (S,)) ? IsMemory(ImmutableMemView{C}) : NotMemory()
end

MemView(s::Base.CodeUnits) = MemView(s.s)

# SubArrays
# TODO: Identical to FastContiguousSubArray in Base
const ContiguousSubArray = SubArray{
    T,
    N,
    P,
    <:Union{Tuple{Vararg{Real}}, Tuple{AbstractUnitRange, Vararg{Any}}},
} where {T, N, P}

MemKind(s::ContiguousSubArray{T, N, P}) where {T, N, P} = MemKind(parent(s)::P)
function MemView(s::ContiguousSubArray{T, N, P}) where {T, N, P}
    memview = MemView(parent(s)::P)
    inds = only(parentindices(s))
    @boundscheck checkbounds(memview.ref.mem, inds)
    @inbounds memview[inds]
end
