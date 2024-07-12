module FixedSizeArraysExt

using FixedSizeArrays: FixedSizeArray
using MemoryViews: MemoryViews, MemoryView, MemoryKind, MutableMemoryView

MemoryViews.MemoryKind(::Type{<:FixedSizeArray{T}}) where {T} = IsMemory(MutableMemoryView{T})
MemoryViews.MemoryView(x::FixedSizeArray) = MemoryView(x.mem)

end # module
