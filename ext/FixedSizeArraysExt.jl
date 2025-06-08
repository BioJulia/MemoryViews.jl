module FixedSizeArraysExt

using FixedSizeArrays: FixedSizeArray
using MemoryViews: MemoryViews, MemoryView, MemoryKind, MutableMemoryView

MemoryViews.MemoryKind(::Type{<:FixedSizeArray{T, N, M}}) where {T, N, M} = MemoryKind(M)
MemoryViews.MemoryView(x::FixedSizeArray) = MemoryView(x.mem)

end # module
