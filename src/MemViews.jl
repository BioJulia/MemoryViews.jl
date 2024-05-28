module MemViews

module t

#=
Should ReinterpretArray be supported?

That requires two params for memviews - for the outer and inner element type.
Dispatch on whether the two types are the same for get/setindex.
Needs a check for bitstypes only when instantiating with different parameters

Setindex is a little difficult - how to implement if the target type is smaller than the source type?
Docs of `unsafe_wrap` seems to suggest using that may cause UB.

I think this is too difficult.
=#

# Problem: Dispatching on ::Union{Nothing, Type{<:MemView}} is ugly
# Could we instead return either NotMemory or Memory{T <: MemView}, both <: MemoryTrait?

"Internal trait object for unsafe methods"
struct Unsafe end
const unsafe = Unsafe()

struct MemView{T, M} <: DenseVector{T}
    ref::MemoryRef{T}
    len::Int

    function MemView{T, M}(ref::MemoryRef{T}, len::Int) where {T, M}
        if M !== :mutable && M !== :immutable
            error("Parameter M must be :mutable or :immutable")
        end
        new{T, M}(ref, len)
    end
end

const MutableMemView{T} = MemView{T, :mutable}
const ImmutableMemView{T} = MemView{T, :immutable}

# Mutable mem views can turn into immutable ones, but not vice versa
ImmutableMemView(x::MutableMemView{T}) where T = ImmutableMemView{T}(x.ref, x.len)
ImmutableMemView(x::ImmutableMemView) = x
MutableMemView(x::MutableMemView) = x

# Convert an immutable to a mutable mem view
MutableMemView{T}(::Unsafe, x::MemView{T}) where T = MutableMemView{T}(x.ref, x.len)

ImmutableMemView(x) = ImmutableMemView(MemView(x))

memview_type(::Any) = nothing

################### Constructors ############################

memview_type(::Array{T}) where T = MutableMemView{T}
MemView(A::Array{T}) where T = MutableMemView{T}(A.ref, length(A))

# Arrays
memview_type(::Memory{T}) where T = MutableMemView{T}
MemView(A::Memory{T}) where T = MutableMemView{T}(MemoryRef(A), length(A))

# Strings etc
# TODO: I don't know if the resulting view is safe.
# Can the GC delete the string if it goes out of scope
# while the memory still exists?
function MemView(s::String)
    ImmutableMemView{UInt8}(
        MemoryRef(unsafe_wrap(Memory{UInt8}, s)),
        ncodeunits(s)
    )
end

function MemView(s::SubString{String})
    mem = unsafe_wrap(Memory{UInt8}, parent(s))
    ImmutableMemView{UInt8}(MemoryRef(mem, s.offset + 1), s.ncodeunits)
end

memview_type(::Base.CodeUnits{UInt8, <:Union{String, SubString{String}}}) = ImmutableMemView{UInt8}
MemView(s::Base.CodeUnits) = MemView(s.s)

# TODO: Identical to FastContiguousSubArray in Base
const ContiguousSubArray = SubArray{T, N, P, I, true} where {T, N, P, I<:Union{Tuple{Vararg{Real}}, Tuple{AbstractUnitRange, Vararg{Any}}}}

memview_type(s::ContiguousSubArray{T, N, P}) where {T, N, P} = memview_type(parent(s)::P)

function MemView(s::ContiguousSubArray{T, N, P}) where {T, N, P}
    v = MemView(parent(s)::P)
    L = length(s)
    memview_type(s)(MemoryRef(v.ref.mem, 1 + s.offset1), L)
end

#################################


function Base.setindex!(v::MutableMemView{T}, x, i::Int) where T
    @boundscheck i ∈ 1:v.len || throw(BoundsError(v, i)) || throw(BoundsError(v, (i,)))
    Base.memoryrefset!(Base.memoryref(v.ref, i, false), x isa T ? x : convert(T,x)::T, :not_atomic, false)
    return v
end

Base.length(v::MemView) = v.len
Base.size(v::MemView) = (length(v),)
Base.IndexStyle(::Type{<:MemView}) = Base.IndexLinear()

function Base.iterate(x::MemView, i::Int=1)
    i > length(x) && return nothing
    (@inbounds x[i], i + 1)
end

function Base.getindex(v::MemView, i::Integer)
    @boundscheck i ∈ 1:v.len || throw(BoundsError(v, i))
    Base.memoryrefget(Base.memoryref(v.ref, i, false), :not_atomic, false)
end

# TODO: What do we want to do here?
Base.:(==)(a::MemView, b::MemView) = throw(MethodError(==, (a, b)))
Base.hash(x::MemView, u::UInt) = throw(MethodError(hash, (x, u)))

Base.pointer(x::MemView{T}) where T = Ptr{T}(pointer(x.ref))
Base.unsafe_convert(::Type{Ptr{T}}, v::MemView{T}) where T = pointer(v)
Base.elsize(::Type{<:MemView{T}}) where T = Base.elsize(Memory{T})
Base.sizeof(x::MemView) = sizeof(eltype(x)) * length(x)
Base.strides(::MemView) = (1,)



#################################### Example

my_findfirst(p, haystack) = my_findnext(p, haystack, firstindex(haystack))

function my_findnext(
    p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, UInt8},
    haystack,
    k
)
    _my_findnext(memview_type(haystack), p, haystack, k)
end

function my_findnext(
    p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, <:AbstractChar},
    s::Union{String, SubString{String}},
    i::Int,
)
    i < 1 && throw(BoundsError(s, i))
    c = Char(p.x)::Char
    byte = (reinterpret(UInt32, c) >> 24) % UInt8
    mem = MemView(s)
    isascii(c) && return find_next_byte(byte, mem, i)
    while true
        i = find_next_byte(byte, mem, i)
        i === nothing && return nothing
        isvalid(s, i) && s[i] == c && return i
        i += 1
    end
    nothing
end

function _my_findnext(::Union{Nothing, Type{<:MemView}}, p, haystack, i)
    lst = lastindex(haystack)
    while i ≤ lst
        p(haystack[i]) && return i
        i = nextind(haystack, i)
    end
    nothing
end

function _my_findnext(
    ::Type{<:MemView{UInt8}},
    p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, UInt8},
    haystack,
    i
)
    ind = Int(i)::Int - Int(firstindex(haystack))::Int + 1
    ind < 1 && throw(BoundsError(haystack, i))
    find_next_byte(p.x, ImmutableMemView(haystack), ind)
end

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

end # module


end # module MemViews
