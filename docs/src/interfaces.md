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

MemoryViews.jl addresses this by introducing two types: At the bottom of abstraction, the simple `MemoryView` type is most basic, unified instantiation of the underlying representation (a chunk of memory).
At the top, the `MemoryKind` trait controls dispatch such that the low-level `MemoryView` implementation is called for the right types.
The idea is that whenever you write a method that operates on "just" a chunk of memory, you implement it for `MemoryView`.
Then, you write methods with `MemoryKind` to make sure all the proper function calls gets dispatched to your `MemoryView` implementation.

!!! tip
    Even if you only ever intend a method to work for, say, `Vector`, it can still be a good idea to implement it for `MemoryView`.
    First, it makes it explicit that you only use `Vector` for its properties as a chunk of memory, and not for, say, its ability
    to be resized.
    Second, you can implement the method for `ImmutableMemoryView`, letting both caller and callee know that the argument is not being mutated.
    Third, after implementing your method for `MemoryView`, it may be easy to also make your method work for `Memory` and other memory-backed types!

## The `MemoryKind` trait
`MemoryKind` answers the question: Can instances of a type be treated as equal to its own memory view?
For a type `T`, `MemoryKind(T)` returns one of two types:
* `NotMemory()` if `T`s are not equivalent to its own memory. Examples include `Int`, which has no memory representation because
  they are not heap allocated, and `String`, which _are_ backed by memory, but which are semantically different from an `AbstractVector`
  containing its bytes.
* `IsMemory{M}()` where `M` is a concrete subtype of `MemoryView`, if instances of `T` _are_ equivalent to their own memory.
  Examples include `Array`s and `Codeunits{String}`. For these objects, it's the case that `x == MemoryView(x)`.

```jldoctest
julia> MemoryKind(Vector{Union{Int32, UInt32}})
IsMemory{MutableMemoryView{Union{Int32, UInt32}}}()

julia> MemoryKind(Matrix{String})
IsMemory{MutableMemoryView{String}}()

julia> MemoryKind(SubString{String})
NotMemory()
```

## Implementing `MemoryView` interfaces
When implementing a method that has a fast-past for memory-like types, you typically want to
* At the top level, dispatch on `MemoryKind` of your argument to funnel the memory-like objects into
  your optimised `MemoryView` function
* At the low level, use `MemoryView` for the implementation of the optimised version

An example could be:
```julia
# Dispatch on `MemoryKind`
my_hash(x) = my_hash(MemoryKind(typeof(x)), x)

# For objects that are bytes, call the function taking only the memory
# representation of `x`
my_hash(::IsMemory{<:MemoryView{UInt8}}, x) = my_hash(ImmutableMemoryView(x))

# IsMemory with eltype other than UInt8 can't use the fast low-level function
my_hash(T::IsMemory, x) = my_hash(NotMemory(), x)

function my_hash(::NotMemory, x)
    # fallback implementation
end

function my_hash(mem::ImmutableMemoryView{UInt8})
    # some optimised low-level memory manipulation with `mem` of bytes
end

# Handle e.g. strings separately, since they are not semantically equal to
# an array of elements in memory, but for this method in particular,
# we want to treat strings as if they are.
function my_hash(x::Union{String, SubString{String}})
    my_hash(MemoryView(x))
end
```