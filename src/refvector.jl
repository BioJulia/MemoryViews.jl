"""
    unsafe_refvector(ref::MemoryRef{T})::RefVector{T}

Create a `RefVector{T}` from an existing `MemoryRef{T}`, aliasing
its memory.
Users must ensure that the `MemoryRef` does not refer to immutable memory,
e.g. is not obtained from a `String` or `ImmutableMemoryView`

Because the resulting `RefVector` spans from the input `ref` and
until the end of the underlying `Memory`, it may contain uninitialized
elements.
"""
function unsafe_refvector end

"""
    RefVector{T} <: DenseVector{T}
    RefVector{T}(undef, len::Integer)
    RefVector(memory::Memory)

A mutable `DenseVector` backed by `Memory`.

`RefVector{T}` is guaranteed to be an immutable struct only containing a
`MemoryRef{T}`. Its length spans from the ref and to the end of the underlying
`Memory`.

Constructing from `Memory` uses the entire `Memory` as the backing storage.
The `undef` constructor creates a new uninitialized `Memory` of the given
`len`.

See [`unsafe_refvector`](@ref) to construct a `RefVector` from a `MemoryRef`.

Slicing into a `RefVector` gives a `MutableMemoryView` aliasing the `RefVector`.

`RefVector` has the following notable differences from `MemoryView`:
* Its length must run to the end of the underlying `Memory`
* `RefVector` is one word smaller in size than `MemoryView`
* Its length is not stored inline; it is behind a pointer dereference.

These properties make `RefVector` useful as backing storage for wrapper
types which carry their own length. For such types, bounds checking may
happen relative to the wrapper type, and the `RefVector` length is rarely
queried.

Obtain the inner reference with `memoryref(::RefVector)`.

# Examples
```jldoctest
julia> mem = fill!(Memory{Int}(undef, 3), 6);

julia> v = RefVector(mem)
3-element RefVector{Int64}:
 6
 6
 6

julia> v[2] = 3;

julia> v == [6, 3, 6]
true

julia> memoryref(v) === memoryref(mem)
true
```
"""
struct RefVector{T} <: DenseVector{T}
    ref::MemoryRef{T}

    global function unsafe_refvector(ref::MemoryRef{T}) where {T}
        return new{T}(ref)
    end
end

const MemoryVector{T} = Union{MemoryView{T}, RefVector{T}}
const MutableMemoryVector{T} = Union{MutableMemoryView{T}, RefVector{T}}

function RefVector{T}(::UndefInitializer, len::Integer) where {T}
    return RefVector(Memory{T}(undef, Int(len)::Int))
end

RefVector{T}(memory::Memory{T}) where {T} = unsafe_refvector(memoryref(memory))
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
    return unsafe_refvector(mem.ref)
end

function Base.similar(@nospecialize(::RefVector), ::Type{T}, dims::Tuple{Int}) where {T}
    return RefVector{T}(undef, only(dims))
end

Base.empty(@nospecialize(::RefVector), ::Type{T}) where {T} = RefVector{T}(undef, 0)
Base.empty(::Type{RefVector{T}}) where {T} = RefVector{T}(undef, 0)

function Base.reverse(mem::RefVector)
    reversed = reverse(MemoryView(mem))
    return unsafe_refvector(reversed.ref)
end

Iterators.reverse(mem::RefVector) = Iterators.reverse(MemoryView(mem))
