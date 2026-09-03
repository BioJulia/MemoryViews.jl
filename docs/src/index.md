```@meta
CurrentModule = MemoryViews
DocTestSetup = quote
    using MemoryViews
end
```

# MemoryViews.jl
This package provide the `MemoryView` type, which is a lightweight and simple view into `Memory`.
The `MemoryView` type is a useful low-level building block for code that operates on chunks of memory.

#### Features:
* Simple and easy to reason about
* Low-overhead, efficient methods
* A safer alternative to pointers

The `MemoryView` type has the following layout:

```julia
struct MemoryView{T, M} <: DenseVector{T}
    ref::MemoryRef{T},
    len::Int
end
```

The `M` parameter is either `Mutable` or `Immutable`, which are unexported but public types defined in this package.
MemoryViews also provide the following aliases for convenience:

```julia
const MutableMemoryView{T} = MemoryView{T, Mutable}
const ImmutableMemoryView{T} = MemoryView{T, Immutable}
```

Immutable memory views are immutable, in that they do not support `setindex!` or other
mutating methods. The existence of an `ImmutableMemoryView` does not protect its underlying
data from being mutated through another variable, or though explicitly unsafe functions,
such as using raw pointers.

## Usage
### Constructing memory views
Construct memory views from `x` with `MemoryView(x)`.
MemoryViews should be constructable from any type that is stored as an array densely in memory.
It can also be constructed from other non-array types that are represented by a chunk of memory (like a `String`).
By default, constructors exists for the memory-backed types in Base:

```jldoctest; output=false
# Vectors
@assert MemoryView(["a", "b", "c"]) isa MemoryView

# Strings
@assert MemoryView("abc") isa MemoryView

# Even complex nested memory-backed types
@assert MemoryView(view(codeunits(view("abcd", Base.OneTo(2))), :)) isa MemoryView

# output

```

For values `x` that are mutable such as `Memory`s and `Array`s (and `SubArray`s of those),
`MemoryView(x)` return `MutableMemoryView`:

```jldoctest; output=false
@assert MemoryView(Int32[1,2,3]) isa MutableMemoryView{Int32}
@assert MemoryView(Memory{String}(undef, 3)) isa MutableMemoryView{String}

# output

```

For values that are immutable, `MemoryView` return `ImmutableMemoryView`s:

```jldoctest; output=false
@assert MemoryView("abc") isa ImmutableMemoryView{UInt8}

# output

```

The constructor `ImmutableMemoryView(x)` will construct an immutable view no matter
if the type returned by `MemoryView(x)` is mutable or not.
This is because it's always possible to convert a mutable memory view to an immutable one:

```jldoctest; output=false
@assert MemoryView(UInt[]) isa MutableMemoryView{UInt}
@assert ImmutableMemoryView(UInt[]) isa ImmutableMemoryView{UInt}

# output

```

Hence, when adding new constructors for new types, you should only add
methods to `MemoryView`.
This should return a mutable memview where possible.

### Indexing
`MemoryView{T}` is a subtype of `AbstractVector{T}`, and mostly behave like you would expect
an abstract vector to behave w.r.t. indexing:

```jldoctest
mem = MemoryView([1,2,3,4,5,6,7,8])

println(mem[2])
println(mem[2:4])
println(mem[end])
println(mem[:])

# output
2
[2, 3, 4]
8
[1, 2, 3, 4, 5, 6, 7, 8]
```

One exception is slicing, which does not copy the underlying data, but simply
returns a new view of the same data.
To copy explicitly, use `copy`, which will create a new `MemoryView` that looks
into a copy of the underlying data:

```jldoctest
mem1 = MemoryView([1,2,3])
mem2 = mem1[1:3]
mem3 = copy(mem1)
mem1[1] = 3
println(mem2[1])
println(mem3[1])

# output
3
1
```

### `RefVector` for backing storage
The package also provides `RefVector{T}`, a mutable `DenseVector{T}` that is guaranteed to be an immutable struct containing no other sized data than a `MemoryRef{T}`.

It differs from `MutableMemoryView{T}` in that its length is always from the ref and to the end of the underlying memory.
Furthermore, the length is not stored inline, but inside the `Memory` and thus behind a pointer indirection.
This makes the type one word smaller than `MemoryView` at the cost of less efficient length queries.

The type is intended to act as a backing type for wrappers which carry their own length information.
For example, we could have a type like:
```julia
struct MyArray{T, N}
    ref::RefVector{T}
    size::NTuple{N, UInt}
end
```
Here, using a `RefVector` saves one word in size, and the slower access to its length does not matter,
because `MyArray` would do the boundschecking and forward to the `RefVector` with `@inbounds`, such that its length need not be queried.

```jldoctest; output=false
memory = Memory{Int}(undef, 4)
refvector = RefVector(memory)
@assert length(refvector) == 4
@assert sizeof(typeof(refvector)) + sizeof(Int) == sizeof(typeof(MemoryView(memory)))

# output

```

`RefVector` can be constructed from a `Memory`, in which case the input is used as the storage.
Use `unsafe_refvector` to construct one from a `MemoryRef`, after ensuring the reference does not point to immutable memory.
It also has a `RefVector{T}(undef, len::Int)` constructor, which is equivalent to `RefVector(Memory{T}(undef, len))`.
