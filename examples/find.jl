using MemoryViews

# Findfirst is implemented in terms of findnext
my_findfirst(p, haystack) = my_findnext(p, haystack, firstindex(haystack))

# The generic: Use fallback
my_findnext(p, haystack, i) = _my_findnext(p, haystack, i)

# Default fallback
function _my_findnext(p, haystack, i)
    lst = lastindex(haystack)
    while i ≤ lst
        p(haystack[i]) && return i
        i = nextind(haystack, i)
    end
    return nothing
end

# We can use a MemoryView to implement character searching in strings.
function my_findnext(
        p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, <:AbstractChar},
        s::Union{String, SubString{String}},
        i::Int,
    )
    i < 1 && throw(BoundsError(s, i))
    c = Char(p.x)::Char
    byte = (reinterpret(UInt32, c) >> 24) % UInt8
    mem = MemoryView(s)
    # If the char is ASCII, then it's a single byte, and we
    # can just find that in the string.
    # Note that this is still correct for invalid UTF8 strings.
    isascii(c) && return find_next_byte(byte, mem, i)
    # Else, we can use memchr to find every candidate occurence
    while true
        i = find_next_byte(byte, mem, i)
        i === nothing && return nothing
        # The found byte may be a non-starting byte of another
        # char. Hence, we need to check if the index is valid.
        isvalid(s, i) && s[i] == c && return i
        i += 1
    end
    return nothing
end

# Byte memory views use the optimized method directly.
function _my_findnext(
        p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, UInt8},
        haystack::MemoryView{UInt8},
        i,
    )
    ind = Int(i)::Int - Int(firstindex(haystack))::Int + 1
    ind < 1 && throw(BoundsError(haystack, i))
    return find_next_byte(p.x, ImmutableMemoryView(haystack), ind)
end

# Explicitly forward supported memory-backed byte containers.
const ContiguousByteSubArray = SubArray{
    UInt8, N, P, I, true,
} where {N, P, I <: Union{Tuple{Integer}, Tuple{AbstractUnitRange}}}

function my_findnext(
        p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, UInt8},
        haystack::Union{Vector{UInt8}, Memory{UInt8}, Base.CodeUnits{UInt8}, ContiguousByteSubArray},
        i,
    )
    return _my_findnext(p, ImmutableMemoryView(haystack), i)
end

# Wrapper around memchr.
# The use of concrete types makes this unsafe code easier to review and statically check.
function find_next_byte(needle::UInt8, haystack::ImmutableMemoryView{UInt8}, i::Int)
    len = length(haystack) - i + 1
    len < 1 && return nothing
    ulen = len % UInt
    GC.@preserve haystack begin
        ptr = pointer(haystack, i)
        p = @ccall memchr(ptr::Ptr{Cvoid}, needle::Cint, ulen::Csize_t)::Ptr{Cvoid}
    end
    return p == C_NULL ? nothing : (p - ptr + i) % Int
end

using Test

@testset "Various uses of byte_search" begin
    @test my_findfirst(==(0x01), [0x01, 0x02, 0x03]) == 1
    @test my_findfirst(==(0x62), view(codeunits("abcd"), 2:3)) == 1

    @test my_findfirst(==('c'), "abcde") == 3
    @test my_findfirst(==('δ'), "αβγδϵ") == 7

    @test my_findfirst(==(0x62), "abcdef") === nothing
    @test my_findfirst(==(0x01), [1, 2, 3]) == 1
    @test my_findfirst(==(0x01), view([0x01, 0x02, 0x03], 1:2:3)) == 1

    # Note that even quite complex nested types will correctly dispatch to
    # the memchr implementation with little overhead
    @test my_findfirst(isequal(0x65), view(codeunits(view("abcdefg", Base.OneTo(5))), :)) ==
        5
end
