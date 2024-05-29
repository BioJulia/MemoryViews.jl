# Findfirst is implemented in terms of findnext
my_findfirst(p, haystack) = my_findnext(p, haystack, firstindex(haystack))

# When the predicate is looking for a single byte, we dispatch to see
# if the haystack is a chunk of memory. In that case, we can use memchr
function my_findnext(
    p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, UInt8},
    haystack,
    k,
)
    _my_findnext(MemKind(haystack), p, haystack, k)
end

# Default fallback
function my_findnext(p, haystack, i)
    lst = lastindex(haystack)
    while i ≤ lst
        p(haystack[i]) && return i
        i = nextind(haystack, i)
    end
    nothing
end

# String implementation - strings are not IsMemory, but
# we can still use the MemViews interface to implement
# char searching.
function my_findnext(
    p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, <:AbstractChar},
    s::Union{String, SubString{String}},
    i::Int,
)
    i < 1 && throw(BoundsError(s, i))
    c = Char(p.x)::Char
    byte = (reinterpret(UInt32, c) >> 24) % UInt8
    mem = MemView(s)
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
    nothing
end

# Fallback - if the haystack is not IsMemory of bytes, we invoke the fallback definiion
_my_findnext(::MemKind, p, haystack, i) = @invoke my_findnext(p::Any, haystack::Any, i::Any)

# If it is bytes, we can convert the haystack to an ImmutableMemView,
# and use the memory view's optimised method
function _my_findnext(
    ::IsMemory{<:MemView{UInt8}},
    p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, UInt8},
    haystack,
    i
)
    ind = Int(i)::Int - Int(firstindex(haystack))::Int + 1
    ind < 1 && throw(BoundsError(haystack, i))
    find_next_byte(p.x, ImmutableMemView(haystack), ind)
end

# Wrapper around memchr
function find_next_byte(needle::UInt8, haystack::ImmutableMemView{UInt8}, i::Int)
    len = length(haystack) - i + 1
    len < 1 && return nothing
    ulen = len % UInt
    GC.@preserve haystack begin
        ptr = pointer(haystack, i)
        p = @ccall memchr(ptr::Ptr{UInt8}, needle::UInt8, ulen::UInt)::Ptr{Nothing}
    end 
    p == C_NULL ? nothing : (p - ptr + i) % Int
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
end
