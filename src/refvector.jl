"""
    RefVector{T} <: DenseVector{T}
    RefVector{T}(undef, len::Integer)
    RefVector(memory::Memory{T})

A mutable vector backed by a `MemoryRef{T}`.
Unlike [`MemoryView`](@ref), a `RefVector` does not store its length inline;
its length is obtained from the parent `Memory`.
This makes `RefVector` one word smaller than `MemoryView`.

Constructing from `Memory` aliases its storage.

`RefVector` is useful as a field in wrapper types which store their
length elsewhere. Such wrappers can perform their own bounds check and access
the `RefVector` with `@inbounds`, avoiding the pointer indirection needed to
load the length of the `RefVector`.

# Examples
```jldoctest
julia> v = RefVector(memoryref(fill!(Memory{Int}(undef, 3), 6)))
3-element RefVector{Int64}:
 6
 6
 6

julia> ref = memoryref(v); typeof(ref) === MemoryRef{Int}
true
```
"""
struct RefVector{T} <: DenseVector{T}
    ref::MemoryRef{T}
end

const MemoryVector{T} = Union{MemoryView{T}, RefVector{T}}
const MutableMemoryVector{T} = Union{MutableMemoryView{T}, RefVector{T}}

function RefVector{T}(::UndefInitializer, len::Integer) where {T}
    memory = Memory{T}(undef, Int(len)::Int)
    return RefVector(memoryref(memory))
end

RefVector{T}(memory::Memory{T}) where {T} = RefVector(memoryref(memory))
RefVector(memory::Memory{T}) where {T} = RefVector{T}(memory)

"""
    MemoryView{T}(v::RefVector{T}, len::Int)::MutableMemoryView{T}

Construct a mutable memory view of the first `len` elements of `v`.
"""
function MemoryView{T}(v::RefVector{T}, len::Int) where {T}
    @boundscheck if (len % UInt) > (length(v) % UInt)
        throw_lightboundserror(v, Base.OneTo(len))
    end
    return unsafe_new_memoryview(Mutable, v.ref, len)
end

MemoryView(v::RefVector{T}) where {T} = @inbounds MemoryView{T}(v, length(v))
MemoryKind(::Type{<:RefVector{T}}) where {T} = IsMemory(MutableMemoryView{T})

Base.size(v::RefVector) = (length(v),)

function Base.length(v::RefVector)
    return length(parent(v)) - memoryrefindex(v.ref) + 1
end

Base.@propagate_inbounds function Base.getindex(v::RefVector, idx::AbstractUnitRange)
    return MemoryView(v)[idx]
end

Base.getindex(v::RefVector, ::Colon) = MemoryView(v)
Base.@propagate_inbounds Base.view(v::RefVector, idx::AbstractUnitRange) = v[idx]

function Base.copy(x::RefVector)
    mem = copy(MemoryView(x))
    return RefVector(mem.ref)
end

function Base.similar(@nospecialize(::RefVector), ::Type{T}, dims::Tuple{Int}) where {T}
    return RefVector{T}(undef, only(dims))
end

Base.empty(@nospecialize(::RefVector), ::Type{T}) where {T} = RefVector{T}(undef, 0)
Base.empty(::Type{RefVector{T}}) where {T} = RefVector{T}(undef, 0)

function Base.reverse(mem::RefVector)
    reversed = reverse(MemoryView(mem))
    return RefVector(reversed.ref)
end
