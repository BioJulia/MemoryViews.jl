# <img src="./sticker.svg" width="30%" align="right" /> MemoryViews

[![Latest Release](https://img.shields.io/github/release/BioJulia/MemoryViews.jl.svg)](https://github.com/BioJulia/MemoryViews.jl/releases/latest)
[![MIT license](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/BioJulia/MemoryViews.jl/blob/master/LICENSE)
[![Documentation](https://img.shields.io/badge/docs-dev-blue.svg)](https://biojulia.github.io/MemoryViews.jl/dev)
[![](https://codecov.io/gh/BioJulia/MemoryViews.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/BioJulia/MemoryViews.jl)

This package implements `MemoryView`, a simple, low-level view into a chunk of `Memory`, as well as the `MemoryKind` trait to guide dispatch of generic methods to memory views.
It is intended to be used as a foundational base for other packages.

To learn how to use the package, [read the documentation](https://biojulia.github.io/MemoryViews.jl/dev/)

## Example
### Basic usage
```julia
# Create complex nested types backed by memory
v1 = view(codeunits("abc"), :)
v2 = view([0x61, 0x62, 0x63], Base.OneTo(3))

# This call hits the slow, generic fallback implementation in Base,
# because it's very difficult to correctly cover all possible
# combinations of types in the method
copyto!(v2, v1)

# These are ordinary `AbstractVector`s, in this case with
# element type UInt8.
mem1 = MemoryView(v1)
mem2 = MemoryView(v2)

# Both views are `MemoryView{UInt8}`, which has the fast path
# implemented. Precisely because we represent "memory" as a simple,
# concrete types, it's easier to provide these kinds of guarantees.
copyto!(mem2, mem1)

# Use the memory views as ordinary vectors
fst = mem1[1]
reverse!(mem1) # ... etc
```

### Dispatching to MemoryView
```julia
function foo(x::ImmutableMemoryView)
    # low-level implementation
end

function foo(::NotMemory, x::AbstractArray)
    # slow, generic fallback
end

# Dispatch with the `MemoryKind` trait
foo(::IsMemory, x) = foo(ImmutableMemoryView(x))
foo(x) = foo(MemoryKind(typeof(x)), x)

# Optionally: Also support strings
foo(x::AbstractString) = foo(codeunits(x))
```

## Good to know
* `MemoryViews.jl` currently only works on the master branch of Julia.
* Slicing a memory view produces a memory view - it does not copy.

## Limitations
* Many optimised fast methods for more established types like `Vector` are missing for `MemoryView`.
  These are added over time. Please make an issue or a PR as you encounter missing methods.

* Currently, `MemoryView` does not make use of `Core.GenericMemory`'s additional parameters, such as atomicity or address space.
  This may easily be added with a `GenericMemoryView` type, similar to `Memory` / `GenericMemory`.

* I can't figure out how to support reinterpreted arrays.
  Any way I can think of doing so will sigificantly complicate `MemoryView`, which takes away some of
  the appeal of this type's simplicity.
  It's possible that reinterpreted arrays are so outside Julia's ordinary memory management
  that this simply can't be done.

* Currently, `String`s are not backed by `Memory` in Julia. Therefore, creating a `MemoryView` of a string
  requires heap-allocating a new `Memory` pointing to the existing memory of the string.
  This can be fixed if `String` is re-implemented to be backed by `Memory`, but I don't know
  enough details about the implementation of `String` to know if this is practical.

## Contributing
Make an issue or PR on this repository, or get in touch with the BioJulia community over at the [Julia Slack](https://julialang.org/slack/) or Zulip servers.
