# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MemoryViews.jl provides `MemoryView`, a low-level view into `Memory{T}` for Julia ≥ 1.11. It's a `DenseVector{T}` subtype representing a `MemoryRef{T}` + length, with static mutability tracking via type parameter (`Mutable`/`Immutable`). The package also defines the `MemoryKind` trait for dispatch on memory-backed types.

## Commands

```bash
# Run tests
JULIA_TEST_FAILFAST=true julia --project -e 'using Pkg; Pkg.test()'

# Format code
runic -i .
```

## Architecture

**Core types** (defined in `src/MemoryViews.jl`):
- `MemoryView{T, M}` where `M ∈ {Mutable, Immutable}` — the main type
- `MemoryKind` trait: `IsMemory{T}` / `NotMemory` for dispatch

**Source files**:
- `construction.jl` — constructors from Array, Memory, String, SubArray, CodeUnits
- `basic.jl` — indexing, slicing (returns views, not copies), copying, find operations with memchr/memrchr C calls, comparison via memcmp
- `experimental.jl` — `split_first`, `split_last`, `split_at`, `split_unaligned`
- `delimited.jl` — `split_each` delimiter iterator
- `base_arrays.jl` — Vector/Memory conversion, append
- `io.jl` — `readbytes!`

**Extensions** (`ext/`): StringViews, FixedSizeArrays, LibDeflate integration.

## Key Patterns

- Slicing creates views into the same memory (no allocation)
- Performance-critical paths use `@ccall` to libc (`memset`, `memcmp`, `memchr`, `memrchr`) with `GC.@preserve`
- Version-conditional code for Julia 1.12+ vs 1.13+ (e.g., `Base.memoryindex` for `parentindices`)
- Trait-based dispatch pattern: define `foo(x)` → `foo(MemoryKind(typeof(x)), x)` → specialized on `IsMemory`/`NotMemory`
- `@boundscheck`/`@inbounds` used throughout for safe-by-default with opt-in elision
