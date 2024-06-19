```@meta
CurrentModule = MemViews
DocTestSetup = quote
    using MemViews
end
```

# MemViews in interfaces
The intended purpose of the MemView type is to provide a kind of low-level abstraction for memory-backed objects.
Strings, substrings, `Memory`, dense views of `Matrix` and countless other types all have the same data representation, namely as simply a chunk of memory.
This means they share important properties: Searching for a one-byte `Char` inside a `String` needs to ccall the exact same `memchr` as searching for `Int8` in a subarray of `Memory`, so obviously, those to functions calls ought to dispath to the same method.

Unfortunately, Julia's system of abstract types are poorly equipped to handle this.
This is because abstract types represent united _behaviour_, whereas in this case, what unites these many different types are the underlying _representation_ - exactly the thing that abstract types want to paper over!

MemViews.jl addresses this by introducing two types: At the bottom of abstraction, the simple, concrete type `MemView` is the simplest, unified instantiation of the underlying representation (a chunk of memory).
At the top, the `MemKind` trait controls dispatch.
The idea is that whenever you write a method that operates on "just" a chunk of memory, you implement it for `MemView`.
Then, you write methods with `MemKind` to make sure all the proper function calls gets dispatched to your `MemView` implementation.

!!! tip
    Even if you only ever intend a method to work for, say, `Vector`, it can still be a good idea to implement it for `MemView`.
    First, it makes it explicit that you only use `Vector` for its properties as a chunk of memory, and not for, say, its ability
    to be resized.
    Second, you can implement the method for `ImmutableMemView`, letting both caller and callee know that the argument is not being mutated.
    Third, after implementing your method for `MemView`, it may be easy to also make your method work for `Memory` and other memory-backed types!

## The `MemKind` trait
`MemKind` answers the question: Can instances of a type be treated as equal to its own memory view?
For a type `T`, `MemKind(T)` returns one of two types:
* `NotMemory()` if `T`s are not equivalent to its own memory. Examples include `Int`, which has no memory representation because
  they are not heap allocated, and `String`, which _are_ backed by memory, but which are semantically different from abstract vectors.
* `IsMemory{M}()` where `M` is a concrete subtype of `MemView`, if instances of `T` _are_ equivalent to their own memory.
  Examples include `Array`s and `Codeunits{String}`. For these objects, it's the case that `x == MemView(x)`.

```jldoctest
julia> MemKind(Vector{Union{Int32, UInt32}})
IsMemory{MutableMemView{Union{Int32, UInt32}}}()

julia> MemKind(Matrix{String})
IsMemory{MutableMemView{String}}()

julia> MemKind(SubString{String})
NotMemory()
```

## Implementing `MemView` interfaces
When implementing a method that has a fast-past for memory-like types, you typically want to
* At the top level, dispatch on `MemKind` of your argument to funnel the memory-like objects into
  your optimised `MemView` function
* At the low level, use `MemView` for the implementation of the optimised version

An example could be:
```julia
# Dispatch on `MemKind`
my_hash(x) = my_hash(MemKind(typeof(x)), x)

function my_hash(::IsMemory{<:MemView{UInt8}}, x)
    mem = MemView(x)
    # some optimised low-level memory manipulation with `mem`
end

# IsMemory with eltype other than UInt8 can't use the fast low-level function
my_hash(T::IsMemory, x) = my_hash(NotMemory(), x)

function my_hash(::NotMemory, x)
    # fallback implementation
end

# Handle e.g. strings separately, since they are not semantically memory,
# but for strings in particular, we want to treat it just like as if they
# were just a chunk of bytes.
function my_hash(x::Union{String, SubString{String}})
    my_hash(MemView(x))
end
```