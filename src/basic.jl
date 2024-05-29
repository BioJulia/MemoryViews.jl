function Base.setindex!(v::MutableMemView{T}, x, i::Int) where {T}
    @boundscheck checkbounds(v, i)
    Base.memoryrefset!(
        Base.memoryref(v.ref, i, false),
        x isa T ? x : convert(T, x)::T,
        :not_atomic,
        false,
    )
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
    Base.memoryrefget(Base.memoryref(v.ref, i, false), :not_atomic, false)
end

Base.pointer(x::MemView{T}) where {T} = Ptr{T}(pointer(x.ref))
Base.unsafe_convert(::Type{Ptr{T}}, v::MemView{T}) where {T} = pointer(v)
Base.elsize(::Type{<:MemView{T}}) where {T} = Base.elsize(Memory{T})
Base.sizeof(x::MemView) = sizeof(eltype(x)) * length(x)
Base.strides(::MemView) = (1,)

function Base.getindex(v::MemView, idx::AbstractUnitRange)
    @boundscheck checkbounds(v, idx)
    typeof(v)(Base.memoryref(v.ref, first(idx), false), length(idx))
end

Base.view(v::MemView, idx::AbstractUnitRange) = v[idx]
