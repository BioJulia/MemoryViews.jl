module Alternative

#=
An alternative implementation of MemViews.
Here, a MemView is simply a pointer and a length. It does not interact with the GC,
and so it is up to users to make sure that MemViews only exist inside GC.@preserve blocks.

## Advantages:
* Cheaper to construct than the other mem views
* The relationship between the GC and aliases mem objects is simpler
  - There is no relationship :)

## Disadvantages
* User has to remember GC.@preserve, no MemView must escape its GC.@preserve block
* Possibly harder to extend to atomic memviews?
=#

# N.B - this must only exist within a GC.@preserve block
struct MemView{T, M} <: DenseVector{T}
    ptr::Ptr{T}
    len::Int

    function MemView{T, M}(ptr::Ptr{T}, len::Int) where {T, M}
        if M !== :mutable && M !== :immutable
            error("Parameter M must be :mutable or :immutable")
        end
        new{T, M}(ptr, len)
    end
end

# Aliases for convenience
const MutableMemView{T} = MemView{T, :mutable}
const ImmutableMemView{T} = MemView{T, :immutable}

# Trivial constructors
ImmutableMemView(x::MutableMemView{T}) where {T} = ImmutableMemView{T}(x.ptr, x.len)
ImmutableMemView(x::ImmutableMemView) = x
MutableMemView(x::MutableMemView) = x

# This constructor is a little different, because it's a typical top-level constructor
# called on user-defined types. It does two things:
# 1. Convert an immutable OR mutable memory view to an immutable one
# 2. Assert that if MemKind is implemented as IsMemory{T}, MemView(x) returns a T.
# We assume 2. is done at compile time and so has zero overhead. 
function ImmutableMemView(x)
    m = MemView(x)
    M = MemKind(x)
    M isa IsMemory && typeassert(m, inner(M))
    ImmutableMemView(m)
end

const ContiguousSubArray = SubArray{
    T,
    N,
    P,
    <:Union{Tuple{Vararg{Real}}, Tuple{AbstractUnitRange, Vararg{Any}}},
} where {T, N, P}

# Constructors
MemView(A::Union{Memory{T}, Array{T}}) where {T} = MutableMemView{T}(pointer(A), length(A))
MemView(s::Union{String, SubString{String}}) = ImmutableMemView{UInt8}(pointer(s), ncodeunits(s))
MemView(s::Base.CodeUnits) = MemView(s.s)
function MemView(s::ContiguousSubArray{T, N, P}) where {T, N, P}
    v = MemView(parent(s)::P)
    L = length(s)
    inner(MemKind(s))(pointer(v, s.offset1 + 1), L)
end

# Memkind 
abstract type MemKind end
struct NotMemory <: MemKind end
struct IsMemory{T <: MemView} <: MemKind
    function IsMemory{T}() where {T}
        isconcretetype(T) || error("In IsMemory{T}, T must be concrete")
        new{T}()
    end
end
IsMemory(T::Type{<:MemView}) = IsMemory{T}()
inner(::IsMemory{T}) where {T} = T

# MemKind implementations
MemKind(::Any) = NotMemory()
MemKind(::Union{Memory{T}, Array{T}}) where {T} = IsMemory(MutableMemView{T})
function MemKind(::Base.CodeUnits{C, S}) where {C, S}
    hasmethod(MemView, (S,)) ? IsMemory(ImmutableMemView{C}) : NotMemory()
end
MemKind(s::ContiguousSubArray{T, N, P}) where {T, N, P} = MemKind(parent(s)::P)

###########################################
# Basic Array functionality of MemViews
###########################################
function Base.setindex!(v::MutableMemView{T}, x, i::Int) where {T}
    @boundscheck checkbounds(v, i)
    unsafe_store!(pointer(v, i), x)
    return v
end

Base.size(v::MemView) = (v.len,)
Base.IndexStyle(::Type{<:MemView}) = Base.IndexLinear()

function Base.iterate(x::MemView, i::Int=1)
    i > length(x) && return nothing
    (@inbounds x[i], i + 1)
end

function Base.getindex(v::MemView, i::Integer)
    @boundscheck checkbounds(v, i)
    unsafe_load(pointer(v, i))
end

Base.pointer(x::MemView, i::Int) = x.ptr + Base.elsize(x) * (i - 1)
Base.pointer(x::MemView) = x.ptr
Base.unsafe_convert(::Type{Ptr{T}}, v::MemView{T}) where {T} = pointer(v)
Base.elsize(::Type{<:MemView{T}}) where {T} = Base.elsize(Memory{T})
Base.sizeof(x::MemView) = Base.elsize(x) * length(x)
Base.strides(::MemView) = (1,)

function Base.getindex(v::MemView, idx::AbstractUnitRange)
    @boundscheck checkbounds(v, idx)
    L = Int(length(idx))::Int
    s = Int(first(idx))::Int
    typeof(v)(pointer(v, s), L)
end

Base.view(v::MemView, idx::AbstractUnitRange) = v[idx]

###########################################
# Implementation
############################################

my_findfirst(p, haystack) = my_findnext(p, haystack, firstindex(haystack))

function my_findnext(p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, UInt8}, haystack, k)
    _my_findnext(MemKind(haystack), p, haystack, k)
end

function my_findnext(p, haystack, i)
    lst = lastindex(haystack)
    while i ≤ lst
        p(haystack[i]) && return i
        i = nextind(haystack, i)
    end
    nothing
end

function my_findnext(
    p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, <:AbstractChar},
    s::Union{String, SubString{String}},
    i::Int,
)
    i < 1 && throw(BoundsError(s, i))
    c = Char(p.x)::Char
    byte = (reinterpret(UInt32, c) >> 24) % UInt8
    GC.@preserve s begin
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
end

_my_findnext(::MemKind, p, haystack, i) = @invoke my_findnext(p::Any, haystack::Any, i::Any)

function _my_findnext(
    ::IsMemory{<:MemView{UInt8}},
    p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, UInt8},
    haystack,
    i,
)
    ind = Int(i)::Int - Int(firstindex(haystack))::Int + 1
    ind < 1 && throw(BoundsError(haystack, i))
    GC.@preserve haystack find_next_byte(p.x, MemView(haystack), ind)
end

function find_next_byte(needle::UInt8, mem::MemView{UInt8}, i::Int)
    len = mem.len - i + 1
    len < 1 && return nothing
    ulen = len % UInt
    p = @ccall memchr((mem.ptr + i - 1)::Ptr{UInt8}, needle::UInt8, ulen::UInt)::Ptr{Nothing}
    p == C_NULL ? nothing : (p - mem.ptr + 1) % Int
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
    @test my_findfirst(isequal(0x65), view(codeunits(view("abcdefg", Base.OneTo(5))), :)) == 5
end


end # module