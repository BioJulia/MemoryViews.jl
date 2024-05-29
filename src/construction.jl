# Array
MemKind(::Array{T}) where {T} = IsMemory(MutableMemView{T})
MemView(A::Array{T}) where {T} = MutableMemView{T}(A.ref, length(A))

# Memory
MemKind(::Memory{T}) where {T} = IsMemory(MutableMemView{T})
MemView(A::Memory{T}) where {T} = MutableMemView{T}(MemoryRef(A), length(A))

# Strings
# TODO: I don't know if the resulting view is safe.
# Can the GC delete the string if it goes out of scope
# while the memory still exists?
function MemView(s::String)
    ImmutableMemView{UInt8}(MemoryRef(unsafe_wrap(Memory{UInt8}, s)), ncodeunits(s))
end

function MemView(s::SubString{String})
    mem = unsafe_wrap(Memory{UInt8}, parent(s))
    ImmutableMemView{UInt8}(MemoryRef(mem, s.offset + 1), ncodeunits(s))
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
    v = MemView(parent(s)::P)
    L = length(s)
    # We assume that subarrays are IsMemory if they implement MemView.
    # I can't think of any situation in where that might not be true.
    inner(MemKind(s))(MemoryRef(v.ref, 1 + s.offset1), L)
end
