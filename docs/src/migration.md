```@meta
CurrentModule = MemoryViews
```

# Migrating from 0.4 to 0.5

This page lists the breaking changes in MemoryViews 0.5 and how to update code
written for MemoryViews 0.4.

## Accessing the `MemoryRef` of an immutable view

`Base.memoryref(::ImmutableMemoryView)` has been removed. The returned
`MemoryRef` permits mutation, even when it comes from an immutable view. In
particular, mutating a reference into storage assumed to be immutable, such as
the storage backing a `String`, can cause undefined behavior.

Code that operates on a `MutableMemoryView` does not need to change:

```julia
view = MemoryView([1, 2, 3])
ref = Base.memoryref(view)
```

If code must obtain a `MemoryRef` from an `ImmutableMemoryView`, replace
`Base.memoryref(view)` with [`unsafe_memoryref(view)`](@ref):

```julia
view = MemoryView("abc")
ref = unsafe_memoryref(view)
```

The caller is responsible for ensuring that the resulting reference is not used
to mutate memory assumed to be immutable. Prefer keeping APIs in terms of
`ImmutableMemoryView` when access to the underlying `MemoryRef` is not required.

## Removal of the `MemoryKind` interface

The `MemoryKind` trait and its `IsMemory` and `NotMemory` types have been
removed, along with the trait-specific `inner` function. Code should dispatch
directly on `MemoryView` instead.

For example, replace trait-based dispatch:

```julia
process(x) = process(MemoryKind(typeof(x)), x)
process(::IsMemory, x) = process(ImmutableMemoryView(x))
process(::NotMemory, x) = fallback(x)
```

with methods for memory views and explicit forwarding methods for other
supported inputs:

```julia
process(x::ImmutableMemoryView) = optimized_implementation(x)
process(x::Union{Vector, Memory}) = process(ImmutableMemoryView(x))
process(x) = fallback(x)
```

Packages that extended `MemoryKind(::Type{T})` should remove those methods. A
`MemoryView(::T)` constructor may still be provided when `T` is backed by dense,
contiguous memory.
