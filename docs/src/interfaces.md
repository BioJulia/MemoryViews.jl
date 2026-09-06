```@meta
CurrentModule = MemoryViews
DocTestSetup = quote
    using MemoryViews
end
```

# MemoryViews in interfaces
The intended purpose of the MemoryView type is to ease manipulation of memory-backed objects through a kind of low-level abstraction.
Strings, substrings, `Memory`, dense views of `Matrix` and countless other types all have the same data representation, namely as simply a chunk of memory.
This means they share important properties: Searching for a one-byte `Char` inside a `String` needs to ccall the exact same `memchr` as searching for `Int8` in a subarray of `Memory`.
Likewise, checking that two substrings are equal can use the same implementation as code checking that two bytearrays are equal.
Obviously, writing the same implementation for each of these types is wasteful.

Unfortunately, Julia's system of abstract types are poorly equipped to handle this.
This is because abstract types represent shared _behaviour_, whereas in this case, what unites these many different types are the underlying _representation_ - exactly the thing that abstract types want to paper over!

MemoryViews.jl addresses this with the simple `MemoryView` type: a unified
representation of a chunk of memory. Whenever a method operates on "just" a
chunk of memory, implement it for `MemoryView`. Other supported input types can
forward explicitly to that implementation.

!!! tip
    Even if you only ever intend a method to work for, say, `Vector`, it can still be a good idea to implement it for `MemoryView`.
    First, it makes it explicit that you only use `Vector` for its properties as a chunk of memory, and not for, say, its ability
    to be resized.
    Second, you can implement the method for `ImmutableMemoryView`, letting both caller and callee know that the argument is not being mutated.
    Third, after implementing your method for `MemoryView`, it may be easy to also make your method work for `Memory` and other memory-backed types!

## Implementing `MemoryView` interfaces
When implementing a method with a fast path for memory-backed types, define the
optimized implementation on `MemoryView`. Add forwarding methods for each input
type whose semantics are appropriate for your function, and retain a generic
fallback when needed.

An example could be:
```julia
function my_hash(mem::ImmutableMemoryView{UInt8})
    # some optimised low-level memory manipulation with `mem` of bytes
end

# Forward types supported by this operation.
my_hash(x::Union{Vector{UInt8}, Memory{UInt8}}) = my_hash(ImmutableMemoryView(x))
my_hash(x::Union{String, SubString{String}}) = my_hash(ImmutableMemoryView(x))

# Generic fallback.
my_hash(x) = generic_hash(x)
```

This explicit dispatch is useful because having a `MemoryView` constructor does
not by itself mean that an object has the same semantics as its memory
representation. For example, a `String` can be viewed as bytes, but is not an
`AbstractVector{UInt8}`.
