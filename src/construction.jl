# Array and Memory
MemKind(::Union{Array{T}, Memory{T}}) where {T} = IsMemory(MutableMemView{T})
MemView(A::Memory{T}) where {T} = MutableMemView{T}(MemoryRef(A), length(A))
MemView(A::Array{T}) where {T} = MutableMemView{T}(A.ref, length(A))

# Strings
MemView(s::String) = ImmutableMemView(unsafe_wrap(Memory{UInt8}, s))
MemView(s::SubString) = MemView(parent(s))[only(parentindices(s))]

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
    (MemView(parent(s)::P)::MemView)[only(parentindices(s))]
end
