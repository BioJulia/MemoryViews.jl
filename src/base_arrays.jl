Base.Vector(x::MemoryView{T}) where {T} = Vector{T}(x)

function Base.Vector{T}(mem::MemoryView{T}) where {T}
    return copyto!(Vector{T}(undef, length(mem)), mem)
end

Base.Memory(x::MemoryView{T}) where {T} = Memory{T}(x)

function Base.Memory{T}(x::MemoryView{T}) where {T}
    return if isempty(x)
        Memory{T}()
    else
        @inbounds copy!(Memory{T}(undef, length(x)), x)
    end
end

function Base.copyto!(A::Union{Memory{T}, Array{T}}, mem::MemoryView{T}) where {T}
    copyto!(MemoryView(A), mem)
    return A
end

function Base.copy!(A::Union{Memory{T}, Array{T}}, mem::MemoryView{T}) where {T}
    length(A) == length(mem) || resize!(A, length(mem))
    copy!(MemoryView(A), mem)
    return A
end

function Base.append!(v::Vector{T}, mem::MemoryView{T}) where {T}
    old_len = length(v)
    resize!(v, length(v) + length(mem))
    dst = @inbounds MemoryView(v)[(old_len + 1):end]
    @inbounds copy!(dst, mem)
    return v
end
