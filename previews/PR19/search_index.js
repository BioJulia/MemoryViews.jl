var documenterSearchIndex = {"docs":
[{"location":"base/#MemoryViews.jl","page":"MemoryViews in Base","title":"MemoryViews.jl","text":"","category":"section"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"It is my hope that MemoryViews, or something like MemoryViews, will eventually be moved into Base Julia. This is because Base Julia, too, includes code that uses the concept of a memory-backed array. However, Base currently lacks any kind of interface and internal API to handle memory-backed objects.","category":"page"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"See the related issue on JuliaLang/julia.","category":"page"},{"location":"base/#What's-wrong-with-SubArrays-of-Memory-as-memory-views?","page":"MemoryViews in Base","title":"What's wrong with SubArrays of Memory as memory views?","text":"","category":"section"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"SubArray is generic over too much, and is therefore too hard to reason about, and to uphold its guarantees.","category":"page"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"First, it's generic over the array type, meaning it may be backed by Memory or Vector, but also UnitRange or Base.LogRange (bitstypes, so not backed by memory), BitMatrix (memory-backed, but elements are stored packed), OffsetArrays, CodeUnits (memory-backed but immutable) and many more. What can you do with the underlying array, generally speaking? Take a pointer to it? No. Assume one-based indexing? No. Assume a stride of one? No. Assume mutability? No.","category":"page"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"Second, it's generic over the index type. It may be UnitRange{Int}, of course, but also Base.OneTo{UInt16}, or StepRange{BigInteger}, CartesianIndices (which it itself generic over the indexes), Colon. Can you define the subset of these types which indicate dense indices? I can't.","category":"page"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"Third, it's multidimensional. It may collect to a Vector or Matrix.","category":"page"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"This is not a design flaw of SubArray - it's a perfectly fine design choice, which enables SubArray to be extremely flexible and broadly useful. Unfortunately, it also makes it nearly impossible to write robust, low-level code using SubArray, because it's almost impossible not to violate the assumptions of a subset of SubArrays many concrete types. Practically speaking, what happens is that methods taking SubArray fall back to only assuming what can be assumed about AbstractArray - which may be inefficient, and buggy (as the recurring bugs due to assumption of one-based indexing has taught us).","category":"page"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"In contrast, a MemoryView{T} is always represented by exactly a MemoryRef{T} and an Int as length. You know exactly what you get.","category":"page"},{"location":"base/#Design-decisions","page":"MemoryViews in Base","title":"Design decisions","text":"","category":"section"},{"location":"base/#Mutability","page":"MemoryViews in Base","title":"Mutability","text":"","category":"section"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"Mutable and immutable memory views are statically distinguished, such that users can write methods that only take mutable memory views. This will statically prevent users from accidentally mutating e.g. strings.","category":"page"},{"location":"base/#MemoryKind","page":"MemoryViews in Base","title":"MemoryKind","text":"","category":"section"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"The MemoryKind trait is used because constructing a MemoryView only for dispatch purposes may not be able to be optimised away by the compiler for some types (currently, strings).","category":"page"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"MemoryKind could be replaced with a function that returned nothing, or the correct MemoryView type directly, but it's nicer to dispatch on ::MemoryKind than on ::Union{Nothing, Type{<:MemoryView}}.","category":"page"},{"location":"base/#Limitations","page":"MemoryViews in Base","title":"Limitations","text":"","category":"section"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"Currently, MemoryView does not make use of Core.GenericMemory's additional parameters, such as atomicity or address space. This may easily be added with a GenericMemoryView type, similar to Memory / GenericMemory.\nI can't figure out how to support reinterpreted arrays. Any way I can think of doing so will significantly complicate MemoryView, which takes away some of the appeal of this type's simplicity. It's possible that reinterpreted arrays are so outside Julia's ordinary memory management that this simply can't be done.\nCurrently, Strings are not backed by Memory in Julia. Therefore, creating a MemoryView of a string requires heap-allocating a new Memory pointing to the existing memory of the string. This can be fixed if String is re-implemented to be backed by Memory, but I don't know enough details about the implementation of String to know if this is practical.","category":"page"},{"location":"base/#Alternative-proposal","page":"MemoryViews in Base","title":"Alternative proposal","text":"","category":"section"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"In examples/alternative.jl, there is an implementation where a MemoryView is just a pointer and a length. This makes it nearly identical to Random.UnsafeView, however, compared to UnsafeView, this proposal has:","category":"page"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"The MemoryKind trait, useful to control dispatch to functions that can treat arrays as being memory\nThe distinction between mutable and immutable memory views","category":"page"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"Overall, I like the alternative proposal less. Raw pointers are bad for safety and ergonomics, and they interact less nicely with the Julia runtime. Also, the existing GenericMemoryRef is essentially perfect for this purpose.","category":"page"},{"location":"base/#Advantages","page":"MemoryViews in Base","title":"Advantages","text":"","category":"section"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"Pointer-based memviews are cheaper to construct, and do not allocate for strings, unlike Memory. Perhaps in the future, strings too will be backed by Memory.\nTheir interaction with the GC is simpler (as there is no interaction)","category":"page"},{"location":"base/#Disadvantages","page":"MemoryViews in Base","title":"Disadvantages","text":"","category":"section"},{"location":"base/","page":"MemoryViews in Base","title":"MemoryViews in Base","text":"While some low-level methods using MemoryView will just forward to calling external libraries where using a pointer is fine, many will be written in pure Julia. There, it's less nice to have raw pointers.\nCode using pointer-based memviews must make sure to only have the views exist inside GC.@preserve blocks, which is annoying and will almost certainly be violated accidentally somewhere\nWe can't use advantages of the existing Memory infrastructure, e.g. having a GenericMemRef which supports atomic memory.","category":"page"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"CurrentModule = MemoryViews\nDocTestSetup = quote\n    using MemoryViews\nend","category":"page"},{"location":"interfaces/#MemoryViews-in-interfaces","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"","category":"section"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"The intended purpose of the MemoryView type is to ease manipulation of memory-backed objects through a kind of low-level abstraction. Strings, substrings, Memory, dense views of Matrix and countless other types all have the same data representation, namely as simply a chunk of memory. This means they share important properties: Searching for a one-byte Char inside a String needs to ccall the exact same memchr as searching for Int8 in a subarray of Memory. Likewise, checking that two substrings are equal can use the same implementation as code checking that two bytearrays are equal. Obviously, writing the same implementation for each of these types is wasteful.","category":"page"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"Unfortunately, Julia's system of abstract types are poorly equipped to handle this. This is because abstract types represent shared behaviour, whereas in this case, what unites these many different types are the underlying representation - exactly the thing that abstract types want to paper over!","category":"page"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"MemoryViews.jl addresses this by introducing two types: At the bottom of abstraction, the simple MemoryView type is most basic, unified instantiation of the underlying representation (a chunk of memory). At the top, the MemoryKind trait controls dispatch such that the low-level MemoryView implementation is called for the right types. The idea is that whenever you write a method that operates on \"just\" a chunk of memory, you implement it for MemoryView. Then, you write methods with MemoryKind to make sure all the proper function calls gets dispatched to your MemoryView implementation.","category":"page"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"tip: Tip\nEven if you only ever intend a method to work for, say, Vector, it can still be a good idea to implement it for MemoryView. First, it makes it explicit that you only use Vector for its properties as a chunk of memory, and not for, say, its ability to be resized. Second, you can implement the method for ImmutableMemoryView, letting both caller and callee know that the argument is not being mutated. Third, after implementing your method for MemoryView, it may be easy to also make your method work for Memory and other memory-backed types!","category":"page"},{"location":"interfaces/#The-MemoryKind-trait","page":"MemoryViews in interfaces","title":"The MemoryKind trait","text":"","category":"section"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"MemoryKind answers the question: Can instances of a type be treated as equal to its own memory view? For a type T, MemoryKind(T) returns one of two types:","category":"page"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"NotMemory() if Ts are not equivalent to its own memory. Examples include Int, which has no memory representation because they are not heap allocated, and String, which are backed by memory, but which are semantically different from an AbstractVector containing its bytes.\nIsMemory{M}() where M is a concrete subtype of MemoryView, if instances of T are equivalent to their own memory. Examples include Arrays and Codeunits{String}. For these objects, it's the case that x == MemoryView(x).","category":"page"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"julia> MemoryKind(Vector{Union{Int32, UInt32}})\nIsMemory{MutableMemoryView{Union{Int32, UInt32}}}()\n\njulia> MemoryKind(Matrix{String})\nIsMemory{MutableMemoryView{String}}()\n\njulia> MemoryKind(SubString{String})\nNotMemory()","category":"page"},{"location":"interfaces/#Implementing-MemoryView-interfaces","page":"MemoryViews in interfaces","title":"Implementing MemoryView interfaces","text":"","category":"section"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"When implementing a method that has a fast-past for memory-like types, you typically want to","category":"page"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"At the top level, dispatch on MemoryKind of your argument to funnel the memory-like objects into your optimised MemoryView function\nAt the low level, use MemoryView for the implementation of the optimised version","category":"page"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"An example could be:","category":"page"},{"location":"interfaces/","page":"MemoryViews in interfaces","title":"MemoryViews in interfaces","text":"# Dispatch on `MemoryKind`\nmy_hash(x) = my_hash(MemoryKind(typeof(x)), x)\n\n# For objects that are bytes, call the function taking only the memory\n# representation of `x`\nmy_hash(::IsMemory{<:MemoryView{UInt8}}, x) = my_hash(ImmutableMemoryView(x))\n\n# IsMemory with eltype other than UInt8 can't use the fast low-level function\nmy_hash(T::IsMemory, x) = my_hash(NotMemory(), x)\n\nfunction my_hash(::NotMemory, x)\n    # fallback implementation\nend\n\nfunction my_hash(mem::ImmutableMemoryView{UInt8})\n    # some optimised low-level memory manipulation with `mem` of bytes\nend\n\n# Handle e.g. strings separately, since they are not semantically equal to\n# an array of elements in memory, but for this method in particular,\n# we want to treat strings as if they are.\nfunction my_hash(x::Union{String, SubString{String}})\n    my_hash(MemoryView(x))\nend","category":"page"},{"location":"reference/#Reference","page":"Reference","title":"Reference","text":"","category":"section"},{"location":"reference/","page":"Reference","title":"Reference","text":"Modules = [MemoryViews]\nOrder   = [:type, :function]","category":"page"},{"location":"reference/#MemoryViews.Immutable","page":"Reference","title":"MemoryViews.Immutable","text":"Trait struct, only used in the mutability parameter of MemoryView\n\n\n\n\n\n","category":"type"},{"location":"reference/#MemoryViews.IsMemory","page":"Reference","title":"MemoryViews.IsMemory","text":"IsMemory{T <: MemoryView} <: MemoryKind\n\nSee: MemoryKind\n\n\n\n\n\n","category":"type"},{"location":"reference/#MemoryViews.MemoryKind","page":"Reference","title":"MemoryViews.MemoryKind","text":"MemoryKind\n\nTrait object used to signal if values of a type is semantically equal to their own MemoryView. If so, MemoryKind(T) should return an instance of IsMemory, else NotMemory(). The default implementation MemoryKind(::Type) returns NotMemory().\n\nIf MemoryKind(T) isa IsMemory{M}, the following must hold:\n\nM is a concrete subtype of MemoryView. To obtain M from an m::IsMemory{M},  use inner(m).\nMemoryView(T) is a valid instance of M.\nMemoryView(x) == x for all instances x::T\n\nSome objects can be turned into MemoryView without being IsMemory. For example, MemoryView(::String) returns a valid MemoryView even though MemoryKind(String) === NotMemory(). This is because strings have different semantics than memory views - the latter is a dense AbstractArray while strings are not, and so the fourth requirement MemoryView(x::String) == x does not hold.\n\nSee also: MemoryView\n\n\n\n\n\n","category":"type"},{"location":"reference/#MemoryViews.MemoryView","page":"Reference","title":"MemoryViews.MemoryView","text":"MemoryView{T, M} <: DenseVector{T}\n\nView into a Memory{T}. Construct from memory-backed values x with MemoryView(x).\n\nMemoryViews are guaranteed to point to contiguous, valid CPU memory, except where they have size zero.\n\nThe parameter M controls the mutability of the memory view, and may be Mutable or Immutable, corresponding to the the aliases MutableMemoryView{T} and ImmutableMemoryView{T}.\n\nSee also: MemoryKind\n\nExamples\n\njulia> v = view([1, 2, 3, 4], 2:3);\n\njulia> mem = MemoryView(v)\n2-element MutableMemoryView{Int64}:\n 2\n 3\n\njulia> MemoryView(codeunits(\"abc\")) isa ImmutableMemoryView{UInt8}\ntrue\n\nExtended help\n\nNew types T which are backed by dense memory should implement:\n\nMemoryView(x::T) to construct a memory view from x. This should  always return a MutableMemoryView when the memory of x is mutable.\nMemoryKind(x::T), if T is semantically equal to its own memory view. Examples of this include Vector, Memory, and Base.CodeUnits{UInt8, String}. If so, x == MemoryView(x) should hold.\n\nIf MemoryView(x) is implemented, then ImmutableMemoryView(x) will automatically work, even if MemoryView(x) returns a mutable view.\n\nIt is not possible to mutate memory though an ImmutableMemoryView, but the existence of the view does not protect the same memory from being mutated though another variable.\n\nThe precise memory layout of the data in a MemoryView follows that of Memory. This includes the fact that some elements in the array, such as  Strings, may be stored as pointers, and isbits Union optimisations.\n\n\n\n\n\n","category":"type"},{"location":"reference/#MemoryViews.Mutable","page":"Reference","title":"MemoryViews.Mutable","text":"Trait struct, only used in the mutability parameter of MemoryView\n\n\n\n\n\n","category":"type"},{"location":"reference/#MemoryViews.NotMemory","page":"Reference","title":"MemoryViews.NotMemory","text":"NotMemory <: MemoryKind\n\nSee: MemoryKind\n\n\n\n\n\n","category":"type"},{"location":"reference/#Base.unsafe_wrap-Union{Tuple{T}, Tuple{Type{MutableMemoryView}, MemoryView{T}}} where T","page":"Reference","title":"Base.unsafe_wrap","text":"unsafe_wrap(MutableMemoryView, x::MemoryView)\n\nConvert a memory view into a mutable memory view. Note that it may cause undefined behaviour, if supposedly immutable data is observed to be mutated.\n\n\n\n\n\n","category":"method"},{"location":"reference/#MemoryViews.inner-Union{Tuple{IsMemory{T}}, Tuple{T}} where T","page":"Reference","title":"MemoryViews.inner","text":"inner(::IsMemory{T})\n\nReturn T from an IsMemory{T}.\n\nSee: MemoryKind\n\n\n\n\n\n","category":"method"},{"location":"reference/#MemoryViews.split_at-Tuple{MemoryView, Int64}","page":"Reference","title":"MemoryViews.split_at","text":"split_at(v::T, i::Int) -> Tuple{T, T} where {T <: MemoryView}\n\nSplit a memory view into two at an index.\n\nThe first will contain all indices in 1:i-1, the second i:end. This function will throw a BoundsError if i is not in 1:end+1.\n\nExamples\n\njulia> split_at(MemoryView([1,2,3,4,5]), 2)\n([1], [2, 3, 4, 5])\n\njulia> split_at(MemoryView(Int8[1, 2, 3]), 4)\n(Int8[1, 2, 3], Int8[])\n\n\n\n\n\n","category":"method"},{"location":"reference/#MemoryViews.split_first-Tuple{MemoryView}","page":"Reference","title":"MemoryViews.split_first","text":"split_first(v::MemoryView{T}) -> Tuple{T, MemoryView{T}}\n\nReturn the first element of v and all other elements as a new memory view.\n\nThis function will throw a BoundsError if v is empty.\n\nSee also: split_last\n\nExamples\n\njulia> v = MemoryView([0x01, 0x02, 0x03]);\n\njulia> split_first(v)\n(0x01, UInt8[0x02, 0x03])\n\njulia> split_first(v[1:1])\n(0x01, UInt8[])\n\njulia> split_first(v[1:0])\nERROR: BoundsError: attempt to access 0-element MutableMemoryView{UInt8} at index [1]\n[...]\n\n\n\n\n\n","category":"method"},{"location":"reference/#MemoryViews.split_last-Tuple{MemoryView}","page":"Reference","title":"MemoryViews.split_last","text":"split_last(v::MemoryView{T}) -> Tuple{T, MemoryView{T}}\n\nReturn the last element of v and all other elements as a new memory view.\n\nThis function will throw a BoundsError if v is empty.\n\nSee also: split_first\n\nExamples\n\njulia> v = MemoryView([0x01, 0x02, 0x03]);\n\njulia> split_last(v)\n(0x03, UInt8[0x01, 0x02])\n\njulia> split_last(v[1:1])\n(0x01, UInt8[])\n\njulia> split_last(v[1:0])\nERROR: BoundsError: attempt to access 0-element MutableMemoryView{UInt8} at index [1]\n[...]\n\n\n\n\n\n","category":"method"},{"location":"reference/#MemoryViews.split_unaligned-Union{Tuple{M}, Tuple{T}, Tuple{A}, Tuple{MemoryView{T, M}, Val{A}}} where {A, T, M}","page":"Reference","title":"MemoryViews.split_unaligned","text":"split_unaligned(v::T, ::Val{A}) -> Tuple{T, T} where {T <: MemoryView}\n\nSplit memory view v into two views a and b, where a is the smallest prefix of v that guarantees the starting memory address of b is is aligned to the integer value A. A must be a normal bit-integer, and a power of two in the range 1:64.\n\nIf v is empty or already aligned, a will be empty. If no elements of v is aligned, b will be empty and a will be equal to v. The element type of v must be a bitstype.\n\nExamples:\n\njulia> split_unaligned(MemoryView(Int16[1, 2, 3]), Val(8))\n(Int16[], Int16[1, 2, 3])\n\njulia> split_unaligned(MemoryView(collect(0x01:0x20))[6:13], Val(8))\n(UInt8[0x06, 0x07, 0x08], UInt8[0x09, 0x0a, 0x0b, 0x0c, 0x0d])\n\n\n\n\n\n","category":"method"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"CurrentModule = MemoryViews\nDocTestSetup = quote\n    using MemoryViews\nend","category":"page"},{"location":"#MemoryViews.jl","page":"MemoryViews","title":"MemoryViews.jl","text":"","category":"section"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"This package provide the MemoryView type, which is a lightweight and simple view into Memory. The MemoryView type is a useful low-level building block for code that operates on chunks of memory.","category":"page"},{"location":"#Features:","page":"MemoryViews","title":"Features:","text":"","category":"section"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"Simple and easy to reason about\nLow-overhead, efficient methods\nA safer alternative to pointers","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"The MemoryView type has the following layout:","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"struct MemoryView{T, M} <: AbstractVector{T}\n    ref::MemoryRef{T},\n    len::Int\nend","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"The M parameter is either Mutable or Immutable, which are unexported types defined in this package. MemoryViews also provide the following aliases for convenience:","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"const MutableMemoryView{T} = MemoryView{T, Mutable}\nconst ImmutableMemoryView{T} = MemoryView{T, Immutable}","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"Immutable memory views are immutable, in that they do not support setindex! or other mutating methods. The existence of an ImmutableMemoryView does not protect its underlying data from being mutated through another variable.","category":"page"},{"location":"#Usage","page":"MemoryViews","title":"Usage","text":"","category":"section"},{"location":"#Constructing-memory-views","page":"MemoryViews","title":"Constructing memory views","text":"","category":"section"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"Construct memory views from x with MemoryView(x). MemoryViews should be constructable from any type that is stored as an array densely in memory. It can also be constructed from other non-array types that are represented by a chunk of memory (like a String). By default, constructors exists for the memory-backed types in Base:","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"# Vectors\n@assert MemoryView([\"a\", \"b\", \"c\"]) isa MemoryView\n\n# Strings\n@assert MemoryView(\"abc\") isa MemoryView\n\n# Even complex nested memory-backed types\n@assert MemoryView(view(codeunits(view(\"abcd\", Base.OneTo(2))), :)) isa MemoryView","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"For values x that are mutable such as Memorys and Arrays (and SubArrays of those), MemoryView(x) return MutableMemoryView:","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"@assert MemoryView(Int32[1,2,3]) isa MutableMemoryView{Int32}\n@assert MemoryView(Memory{String}(undef, 3)) isa MutableMemoryView{String}","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"For values that are immutable, MemoryView return ImmutableMemoryViews:","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"@assert MemoryView(\"abc\") isa ImmutableMemoryView{UInt8}","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"The constructor ImmutableMemoryView(x) will construct an immutable view no matter if the type returned by MemoryView(x) is mutable or not. This is because it's always possible to convert a mutable memory view to an immutable one:","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"@assert MemoryView(UInt[]) isa MutableMemoryView{UInt}\n@assert ImmutableMemoryView(UInt[]) isa ImmutableMemoryView{UInt}","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"Hence, when adding new constructors for new types, you should only add methods to MemoryView. This should return a mutable memview where possible.","category":"page"},{"location":"#Indexing","page":"MemoryViews","title":"Indexing","text":"","category":"section"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"MemoryView{T} is a subtype of AbstractVector{T}, and mostly behave like you would expect an abstract vector to behave w.r.t. indexing:","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"mem = MemoryView([1,2,3,4,5,6,7,8])\n\nprintln(mem[2])\nprintln(mem[2:4])\nprintln(mem[end])\nprintln(mem[:])\n\n# output\n2\n[2, 3, 4]\n8\n[1, 2, 3, 4, 5, 6, 7, 8]","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"One exception is slicing, which does not copy the underlying data, but simply returns a new view of the same data. To copy explicitly, use copy, which will create a new MemoryView that looks into a copy of the underlying data:","category":"page"},{"location":"","page":"MemoryViews","title":"MemoryViews","text":"mem1 = MemoryView([1,2,3])\nmem2 = mem1[1:3]\nmem3 = copy(mem1)\nmem1[1] = 3\nprintln(mem2[1])\nprintln(mem3[1])\n\n# output\n3\n1","category":"page"}]
}
