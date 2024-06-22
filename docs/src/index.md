```@meta
CurrentModule = MemViews
DocTestSetup = quote
    using MemViews
end
```

# MemViews.jl
This package provide the `MemView` type, which is a lightweight and simple view into `Memory`.
The `MemView` type is a useful low-level building block for code that operates on chunks of memory.

#### Features:
* Simple and easy to reason about
* Low-overhead, efficient methods
* A safer alternative to pointers

The `MemView` type has the following layout:

```julia
struct MemView{T, M} <: AbstractVector{T}
    ref::MemoryRef{T},
    len::Int
end
```

The `M` parameter is either `Mutable` or `Immutable`, which are unexported types defined in this package.
MemViews also provide the following aliases for convenience:

```julia
const MutableMemView{T} = MemView{T, Mutable}
const ImmutableMemView{T} = MemView{T, Immutable}
```

Immutable memory views are immutable, in that they do not support `setindex!` or other
mutating methods. The existence of an `ImmutableMemView` does not protect its underlying
data from being mutated through another variable.

## Usage
### Constructing memory views
Construct memory views from `x` with `MemView(x)`.
MemViews should be constructable from any type that is stored as an array densely in memory.
It can also be conctructed from other non-array types that are represented by a chunk of memory (like a `String`).
By default, constructors exists for the memory-backed types in Base:

```jldoctest; output=false
# Vectors
@assert MemView(["a", "b", "c"]) isa MemView

# Strings
@assert MemView("abc") isa MemView

# Even complex nested memory-backed types
@assert MemView(view(codeunits(view("abcd", Base.OneTo(2))), :)) isa MemView

# output

```

For values `x` that are mutable such as `Memory`s and `Array`s (and `SubArray`s of those),
`MemView(x)` return `MutableMemView`:

```jldoctest; output=false
@assert MemView(Int32[1,2,3]) isa MutableMemView{Int32}
@assert MemView(Memory{String}(undef, 3)) isa MutableMemView{String}

# output

```

For values that are immutable, `MemView` return `ImmutableMemView`s:

```jldoctest; output=false
@assert MemView("abc") isa ImmutableMemView{UInt8}

# output

```

The constructor `ImmutableMemView(x)` will construct an immutable view no matter
if the type returned by `MemView(x)` is mutable or not.
This is because it's always possible to convert a mutable memory view to an immutable one:

```jldoctest; output=false
@assert MemView(UInt[]) isa MutableMemView{UInt}
@assert ImmutableMemView(UInt[]) isa ImmutableMemView{UInt}

# output

```

Hence, when adding new constructors for new types, you should only add
methods to `MemView`.
This should return a mutable memview where possible.

### Indexing
`MemView{T}` is a subtype of `AbstractVector{T}`, and mostly behave like you would expect
an abstract vector to behave w.r.t. indexing:

```jldoctest
mem = MemView([1,2,3,4,5,6,7,8])

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
To copy explicitly, use `copy`, which will create a new `MemView` that looks
into a copy of the underlying data:

```jldoctest
mem1 = MemView([1,2,3])
mem2 = mem1[1:3]
mem3 = copy(mem1)
mem1[1] = 3
println(mem2[1])
println(mem3[1])

# output
3
1
```
