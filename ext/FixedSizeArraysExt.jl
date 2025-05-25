module FixedSizeArraysExt

using FixedSizeArrays: FixedSizeArray
using MemoryViews: MemoryViews, MemoryView, MemoryKind, MutableMemoryView

MemoryViews.MemoryKind(::Type{<:FixedSizeArray{T, N, M}}) where {T, N, M} = MemoryKind(M)

function MemoryViews.MemoryView(x::FixedSizeArray)
    mem = MemoryView(parent(x))
    ind = only(parentindices(x))
    return @inbounds mem[ind]
end

end # module
