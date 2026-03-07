# State Layout

Design state memory layout. Defines the arena-based model that decouples state layout from compile-time LLVM struct types.

For the compilation model, see [compilation-model.md](compilation-model.md). For architectural motivation, see [architecture-principles.md](architecture-principles.md).

## Motivation

Lyra's compilation model separates elaboration-time properties (container sizes, instance topology) from execution-time properties (packed widths, compiled code shape). This requires a state model where:

- Packed fields use efficient static offsets (compile-time constants)
- Unpacked containers use elaboration-resolved sizes and offsets
- Layout is computed once after elaboration, before simulation begins
- No dynamic allocation occurs during simulation

A monolithic compile-time LLVM struct couples layout to compilation. Changing an unpacked array size forces recompilation. The arena model removes this coupling.

## Arena Layout

Design state is a contiguous byte arena allocated once after elaboration:

```
DesignState = byte arena + layout metadata
```

Each module instance occupies a region within the arena, addressed via `this_base`. Within that region, fields are accessed by offset.

### Offset Categories

**Static offsets** (compile-time constants in SpecLayout):

- Packed scalar fields (`bit`, `logic`, `int`, packed structs)
- Real/shortreal fields
- Handle slots (string, dynamic array, queue pointers)

These offsets are known at specialization compilation time and embedded directly in generated code.

**Elaboration-resolved offsets** (computed during elaboration):

- Unpacked array base offsets (depends on container size)
- Fields that follow unpacked containers (their offset shifts with container size)

These offsets are computed during elaboration and stored in layout metadata. Compiled code loads the base offset from metadata at access time.

### Access Patterns

Packed field access (zero-overhead, identical to current model):

```
value = load(this_base + static_offset)
```

Unpacked container element access:

```
base = load_metadata(container_descriptor)
value = load(this_base + base + index * element_stride)
```

The extra metadata load for containers is negligible: unpacked array access already involves index computation with a data dependency.

## Allocation Lifecycle

1. **Compilation**: SpecLayout records static offsets for packed fields and container descriptors for unpacked regions. No design-global layout knowledge.

2. **Elaboration**: Assembly resolves container sizes from elaborated parameters. Computes final layout per instance. Allocates the arena.

3. **Simulation**: Arena is immutable in structure. No allocations, no resizing. Only field values change.

## Relationship to Specialization

The arena model refines how SpecLayout works, not what it means:

- `ModuleSpecId` remains the compilation unit
- `this_base + offset` remains the addressing model
- Static offsets for packed fields remain compile-time constants
- Container offsets become elaboration-resolved metadata instead of compile-time constants

Two instances with the same `ModuleSpecId` but different unpacked array sizes share compiled code. They have different layout metadata but identical process functions.

## Concept Mapping

| Concept                   | Role                                  | Changed?                              |
| ------------------------- | ------------------------------------- | ------------------------------------- |
| `ModuleSpecId`            | Compilation unit identity             | No                                    |
| `SpecLayout`              | Per-specialization field layout       | Refined: adds container descriptors   |
| `this_base`               | Instance state base pointer           | No                                    |
| `DesignState`             | Design-wide state memory              | Refined: arena instead of LLVM struct |
| Assembly                  | Computes instance placements          | Extended: resolves container sizes    |
| Packed field access       | `this_base + constant`                | No                                    |
| Unpacked container access | `this_base + metadata_offset + index` | New: metadata indirection             |

## Non-Goals

- Packed types remain specialization boundaries. Packed width affects arithmetic instructions and must produce different compiled code.
- Dynamic arrays, queues, and associative arrays remain heap-allocated handle objects. The arena model applies to fixed-size unpacked containers only.
- The MIR interpreter is unaffected.
