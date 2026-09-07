```@meta
CurrentModule = MemoryViews
```

# Migrating from 0.4 to 0.5

This page lists the breaking changes in MemoryViews 0.5 and how to update code
written for MemoryViews 0.4.

## Reverse iteration preserves mutability

`Iterators.reverse` now stores the original `MemoryView`, preserving its
mutability. Applying it twice returns the original view; previously, the result
was always an `ImmutableMemoryView`, even for mutable input.

```julia
view = MemoryView([1, 2, 3])
restored = Iterators.reverse(Iterators.reverse(view))
restored === view # true
restored[1] = 4 # Mutates the original backing array
```

If code relied on the result being immutable, convert it explicitly with
`ImmutableMemoryView(restored)`. Immutable inputs still return the original
immutable view after reversing twice.

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

## Accessing parent memory

`parent(::MemoryView)` now returns a view of the entire backing `Memory`,
preserving the input's type and mutability. For a sliced view, the parent also
includes the elements outside the slice. An immutable view's parent remains
immutable.

Code that truly needs the backing memory of an immutable view must first make
the unsafe operation explicit:

```julia
view = MemoryView("abc")
memory = unsafe_memory(view)
```

The returned memory must not be mutated when it backs data assumed to be
immutable, such as a `String`.

## Pointer conversion

`Base.cconvert(Ptr{T}, view)` now returns `view` itself, keeping the owning
object rooted during a foreign call. Use `Base.unsafe_convert` for the second
conversion step, or call `pointer` directly:

```julia
converted = Base.cconvert(Ptr{UInt8}, view)
ptr = Base.unsafe_convert(Ptr{UInt8}, converted)

# Equivalent when the caller manages preservation:
ptr = pointer(view)
```

Previously, `cconvert` returned the underlying `MemoryRef`.

## Internal fields

The field names of `MemoryView` are no longer public API. Replace direct field
access with the corresponding interface:

* Replace `view.len` with `length(view)`.
* Replace `view.ref` with `Base.memoryref(view)` for mutable views.
* For an immutable view, use `unsafe_memoryref(view)` only when explicitly
  unsafe access is necessary.

`MemoryView{T, M}` remains guaranteed to be immutable and to have the same size
as a `MemoryRef{T}` and an `Int` combined. Its field count, field order, field
types, and field names are internal.

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
