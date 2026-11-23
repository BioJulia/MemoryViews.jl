module TranscodingStreamsExt

using TranscodingStreams: TranscodingStream
using MemoryViews: MemoryViews, MutableMemoryVector

# Resolve ambiguity with TranscodingStreams' method for `DenseArray{UInt8}`.
# TranscodingStream is buffered, and implements `eof`, `bytesavailable` and
# `unsafe_read`, so the generic implementation is efficient.
function Base.readbytes!(io::TranscodingStream, v::MutableMemoryVector{UInt8}, nb::Integer = length(v))
    return MemoryViews._readbytes!(io, v, nb)
end

end # module
