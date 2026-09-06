module FixedSizeArraysExt

using FixedSizeArrays: FixedSizeArray
using MemoryViews: MemoryViews, MemoryView

MemoryViews.MemoryView(x::FixedSizeArray) = MemoryView(parent(x))

end # module
