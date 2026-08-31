Base.Vector(x::MemoryVector{T}) where {T} = Vector{T}(x)

function Base.Vector{T}(mem::MemoryVector{T}) where {T}
    return copyto!(Vector{T}(undef, length(mem)), mem)
end

Base.Memory(x::MemoryVector{T}) where {T} = Memory{T}(x)

function Base.Memory{T}(x::MemoryVector{T}) where {T}
    return if isempty(x)
        Memory{T}()
    else
        @inbounds copy!(Memory{T}(undef, length(x)), x)
    end
end

function Base.copyto!(A::Union{Memory{T}, Array{T}}, mem::MemoryVector{T}) where {T}
    copyto!(MemoryView(A), mem)
    return A
end

function Base.copy!(A::Union{Memory{T}, Array{T}}, mem::MemoryVector{T}) where {T}
    length(A) == length(mem) || resize!(A, length(mem))
    copy!(MemoryView(A), mem)
    return A
end

function Base.copyto!(mem::MutableMemoryVector{T}, A::Union{Memory{T}, Array{T}}) where {T}
    return copyto!(mem, MemoryView(A))
end

function Base.copy!(mem::MutableMemoryVector{T}, A::Union{Memory{T}, Array{T}}) where {T}
    return copy!(mem, MemoryView(A))
end

function Base.append!(v::Vector{T}, mem::MemoryView{T}) where {T}
    isempty(mem) && return v
    old_len = length(v)
    resize!(v, length(v) + length(mem))
    dst = @inbounds MemoryView(v)[(old_len + 1):end]
    @inbounds copy!(dst, mem)
    return v
end
