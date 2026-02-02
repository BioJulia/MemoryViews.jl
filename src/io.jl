# Note: This implementation intentionally throws ambiguity errors,
# because the fallback definition for ::IO ::AbstractArray{UInt8}
# is a performance disaster.
# This encourages implementors of IOs to implement a method for this.
function Base.readbytes!(io::IO, v::MutableMemoryView{UInt8}, nb::Integer = length(v))
    nb = Int(nb)::Int
    # A view of all the bytes not yet read
    remaining = @inbounds v[1:min(nb, length(v))]
    while !isempty(remaining)
        eof(io) && break
        # Read at least 1 byte if not EOF, thereby filling the internal buffer.
        # and read no more than the remaining number of bytes
        ba = clamp(bytesavailable(io), 1, length(remaining))
        GC.@preserve v unsafe_read(io, Base.unsafe_convert(Ptr{UInt8}, remaining), ba % UInt)
        remaining = remaining[(ba + 1):end]
    end
    return min(nb, length(v) - length(remaining))
end
