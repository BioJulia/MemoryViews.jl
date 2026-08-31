module LibDeflateExt

using LibDeflate: LibDeflate
using MemoryViews: MemoryVector, MutableMemoryVector

# Note: I intentionally limits this to `UInt8` memory, even though the LibDeflate
# type supports all bitstypes (on v0.4).
# I think reading/writing arbitrary bitstypes using pointers is a little shady,
# so the user should opt-in by constructing the pointers manually.
#
# For LibDeflate v1, this is restricted to byte arrays. Users may use the
# raw pointer methods to opt-into using MemoryView of other eltypes in LibDeflate.
function LibDeflate.ReadableMemory(mem::MemoryVector{UInt8})
    len = (length(mem) & typemax(Int)) % UInt
    return LibDeflate.ReadableMemory(pointer(mem), len)
end

function LibDeflate.WriteableMemory(mem::MutableMemoryVector{UInt8})
    len = (length(mem) & typemax(Int)) % UInt
    return LibDeflate.WriteableMemory(pointer(mem), len)
end

end # module
