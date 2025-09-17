module LibDeflateExt

using LibDeflate: LibDeflate
using MemoryViews: MemoryView, MutableMemoryView

# Note: I intentionally limits this to `UInt8` memory, even though the LibDeflate
# type supports all bitstypes.
# I think reading/writing arbitrary bitstypes using pointers is a little shady,
# so the user should opt-in by constructing the pointers manually.
function LibDeflate.ReadableMemory(mem::MemoryView{UInt8})
    return LibDeflate.ReadableMemory(pointer(mem), length(mem) % UInt)
end

function LibDeflate.WriteableMemory(mem::MutableMemoryView{UInt8})
    return LibDeflate.WriteableMemory(pointer(mem), length(mem) % UInt)
end

end # module
