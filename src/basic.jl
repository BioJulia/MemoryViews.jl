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
    elem_offset = div(byte_offset, Base.elsize(x)) % Int
    elem_offset + 1: elem_offset + x.len
end

function Base.copy(x::MutableMemView)
    isempty(x) && return x
    newmem = @inbounds x.ref.mem[parentindices(x)]
    typeof(x)(memoryref(newmem), x.len)
end

Base.copy(x::ImmutableMemView) = x

function Base.getindex(v::MemView, i::Integer)
    @boundscheck checkbounds(v, i)
    Base.memoryrefget(@inbounds(memoryref(v.ref, i)), :not_atomic, false)
end

Base.pointer(x::MemView{T}) where {T} = Ptr{T}(pointer(x.ref))
Base.unsafe_convert(::Type{Ptr{T}}, v::MemView{T}) where {T} = pointer(v)
Base.elsize(::Type{<:MemView{T}}) where {T} = Base.elsize(Memory{T})
Base.sizeof(x::MemView) = sizeof(eltype(x)) * length(x)
Base.strides(::MemView) = (1,)

function Base.getindex(v::MemView, idx::AbstractUnitRange)
    # This branch is necessary, because the memoryref can't point out of bounds.
    # So if the user gives an empty slice that is out of bounds, the boundscheck
    # may pass, but the memoryref construction will be OOB.
    isempty(idx) && return typeof(v)(memoryref(v.ref.mem), 0)
    @boundscheck checkbounds(v, idx)
    newref = @inbounds memoryref(v.ref, Int(first(idx))::Int)
    typeof(v)(newref, length(idx))
end

Base.view(v::MemView, idx::AbstractUnitRange) = v[idx]

function Base.unsafe_copyto!(dest::MutableMemView{T}, doffs, src::MemView{T}, soffs, n) where T
    iszero(n) && return dest
    dst = @inbounds memoryref(dest.ref, Int(doffs)::Int)
    src = @inbounds memoryref(src.ref, Int(soffs)::Int)
    @inbounds unsafe_copyto!(dst, src, Int(n)::Int)
    return dest
end

function Base.findnext(
    p::Base.Fix2{<:Union{typeof(==), typeof(isequal)}, T},
    mem::MemView{T},
    start::Integer
) where {T <: Union{UInt8, Int8}}
    start = Int(start)::Int
    real_start = max(start, 1)
    v = @inbounds ImmutableMemView(mem[real_start:end])
    v_ind = @something memchr(v, p.x) return nothing
    v_ind + real_start - 1
end

function memchr(mem::ImmutableMemView{T}, byte::T) where {T <: Union{Int, UInt8}}
    isempty(mem) && return nothing
    GC.@preserve mem begin
        ptr = Ptr{UInt8}(pointer(mem))
        p = @ccall memchr(ptr::Ptr{UInt8}, (byte % UInt8)::UInt8, length(mem)::Int)::Ptr{Cvoid}
    end
    p == C_NULL ? nothing : (p - ptr) % Int + 1
end
