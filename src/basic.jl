function Base.setindex!(v::MutableMemView{T}, x, i::Int) where {T}
    @boundscheck checkbounds(v, i)
    xT = x isa T ? x : convert(T, x)::T
    ref = @inbounds memoryref(v.ref, i)
    @inbounds ref[] = xT
    return v
end

Base.size(v::MemView) = (v.len,)
Base.IndexStyle(::Type{<:MemView}) = Base.IndexLinear()

function Base.iterate(x::MemView, i::Int=1)
    ((i - 1) % UInt) < (length(x) % UInt) || return nothing
    (@inbounds x[i], i + 1)
end

function Base.parentindices(x::MemView)
    byte_offset = pointer(x.ref) - pointer(x.ref.mem)
    elem_offset = div(byte_offset % UInt, Base.elsize(x) % UInt) % Int
    (elem_offset + 1):(elem_offset + x.len)
end

function Base.copy(x::MemView)
    isempty(x) && return x
    newmem = @inbounds x.ref.mem[parentindices(x)]
    typeof(x)(unsafe, memoryref(newmem), x.len)
end

function Base.getindex(v::MemView, i::Integer)
    @boundscheck checkbounds(v, i)
    ref = @inbounds memoryref(v.ref, i)
    @inbounds ref[]
end

function Base.similar(mem::MemView{T1, M}, ::Type{T2}, dims::Tuple{Int}) where {T1, T2, M}
    len = Int(only(dims))::Int
    memory = Memory{T2}(undef, len)
    MemView{T2, M}(unsafe, memoryref(memory), len)
end

function Base.empty(mem::MemView{T1, M}, ::Type{T2}) where {T1, T2, M}
    MemView{T2, M}(unsafe, memoryref(Memory{T2}()), 0)
end

Base.empty(T::Type{<:MemView{E}}) where {E} = T(unsafe, memoryref(Memory{E}()), 0)

Base.pointer(x::MemView{T}) where {T} = Ptr{T}(pointer(x.ref))
Base.unsafe_convert(::Type{Ptr{T}}, v::MemView{T}) where {T} = pointer(v)
Base.elsize(::Type{<:MemView{T}}) where {T} = Base.elsize(Memory{T})
Base.sizeof(x::MemView) = Base.elsize(typeof(x)) * length(x)
Base.strides(::MemView) = (1,)

function Base.getindex(v::MemView, idx::AbstractUnitRange)
    # This branch is necessary, because the memoryref can't point out of bounds.
    # So if the user gives an empty slice that is out of bounds, the boundscheck
    # may pass, but the memoryref construction will be OOB.
    isempty(idx) && return typeof(v)(unsafe, memoryref(v.ref.mem), 0)
    @boundscheck checkbounds(v, idx)
    newref = @inbounds memoryref(v.ref, Int(first(idx))::Int)
    typeof(v)(unsafe, newref, length(idx))
end

Base.getindex(v::MemView, ::Colon) = v
Base.view(v::MemView, idx::AbstractUnitRange) = v[idx]

function Base.unsafe_copyto!(dst::MutableMemView{T}, src::MemView{T}) where {T}
    iszero(length(src)) && return dst
    @inbounds unsafe_copyto!(dst.ref, src.ref, length(src))
    return dst
end

function Base.copy!(dst::MutableMemView{T}, src::MemView{T}) where {T}
    @boundscheck length(dst) == length(src) || throw(BoundsError(dst, eachindex(src)))
    unsafe_copyto!(dst, src)
end

function Base.copyto!(dst::MutableMemView{T}, src::MemView{T}) where {T}
    @boundscheck length(dst) ≥ length(src) || throw(BoundsError(dst, eachindex(src)))
    unsafe_copyto!(dst, src)
end

function Base.findnext(
    p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, T},
    mem::MemView{T},
    start::Integer,
) where {T <: Union{UInt8, Int8}}
    start = Int(start)::Int
    real_start = max(start, 1)
    v = @inbounds ImmutableMemView(mem[real_start:end])
    v_ind = @something memchr(v, p.x) return nothing
    v_ind + real_start - 1
end

function memchr(mem::ImmutableMemView{T}, byte::T) where {T <: Union{Int8, UInt8}}
    isempty(mem) && return nothing
    GC.@preserve mem begin
        ptr = Ptr{UInt8}(pointer(mem))
        p = @ccall memchr(
            ptr::Ptr{UInt8},
            (byte % UInt8)::UInt8,
            length(mem)::Int,
        )::Ptr{Cvoid}
    end
    p == C_NULL ? nothing : (p - ptr) % Int + 1
end

const Bits =
    Union{Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, Char}

function Base.:(==)(a::MemView{T}, b::MemView{T}) where {T <: Bits}
    length(a) == length(b) || return false
    T === Union{} && return true
    a.ref === b.ref && return true
    GC.@preserve a b begin
        aptr = Ptr{Nothing}(pointer(a))
        bptr = Ptr{Nothing}(pointer(a))
        y = @ccall memcmp(aptr::Ptr{Nothing}, bptr::Ptr{Nothing}, length(a)::Int)::Cint
    end
    iszero(y)
end
