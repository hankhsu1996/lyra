# State Layout

> Before editing, see [documentation-guidelines.md](documentation-guidelines.md). Architecture docs describe the target, not history. No "current state," migration plans, or queue references.

Byte-level storage layout for simulation state. Decouples state layout from compile-time LLVM struct types.

For the natural model, see [natural-model.md](natural-model.md). For the compilation model, see [compilation-model.md](compilation-model.md). For architectural motivation, see [architecture-principles.md](architecture-principles.md).

## Target Model

The target follows from the natural model: an instance is an object that owns its state. Member access is object pointer + local offset. Each instance has its own storage region. Design-global coordination (scheduler, event queues) exists as an outer layer, not as the defining representation of instance state.

## Byte-Level Layout (Orthogonal to Object-Local vs Design-Global)

This doc describes byte-level layout: how individual members are sized, aligned, and addressed within a storage region using byte offsets. This is orthogonal to the object-local vs design-global question. Per-instance objects use the same byte-level layout internally -- the layout rules apply regardless of whether the storage region is carved from a design-global arena or allocated per-instance.

The byte-level model exists because a monolithic compile-time LLVM struct couples layout to compilation. Changing an unpacked array size forces recompilation. Byte-level layout removes this coupling.

Requirements:

- Packed fields use efficient static offsets (compile-time constants)
- Unpacked containers use elaboration-resolved sizes and offsets
- Layout is computed once after elaboration, before simulation begins
- No dynamic allocation occurs during simulation

## Instance Storage

Each module instance occupies its own storage region, addressed via `this_base`. Within that region, fields are accessed by offset.

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

1. **Compilation**: SpecLayout records static offsets for packed fields and container descriptors for unpacked regions. No design-global layout knowledge. These are body-shaped facts that describe the type, not any particular instance.

2. **Construction**: Resolves container sizes from elaborated parameters. Computes final layout per instance. Allocates storage.

3. **Simulation**: Storage is immutable in structure. No allocations, no resizing. Only field values change.

## Storage Ownership

Not every slot in an instance's logical range owns its own storage in the arena. Port connections can create **forwarded aliases** -- slots that share another slot's storage instead of occupying their own region.

### Two Storage Binding Kinds

| Kind                  | Meaning                                                                     | Instance-relative offset?               |
| --------------------- | --------------------------------------------------------------------------- | --------------------------------------- |
| OwnedLocalStorage     | Slot owns bytes in its instance's arena region                              | Yes -- computable from instance base    |
| ForwardedStorageAlias | Slot aliases a canonical owner's storage (possibly in a different instance) | No -- must use design-global addressing |

The `SlotStorageBinding` variant (in `layout/storage_types.hpp`) is the semantic source of truth. All downstream consumers pattern-match on it; there is no way to accidentally compute a relative offset for a forwarded alias.

### When Forwarding Occurs

The forwarding analysis (`forwarding_analysis.cpp`) identifies connection-relay candidates: slots written by exactly one connection process, read by one or more downstream connections (all self-triggered, full-slot), with no behavioral process triggers. Such slots are pure pass-through relays. Their storage is collapsed to the upstream source, eliminating redundant dirty-tracking, connection evaluation, and propagation work.

A typical example: a module with a pass-through input port connected to a child. The intermediate port slot becomes a forwarded alias; the child reads directly from the grandparent's storage.

### Pipeline Boundary

Forwarding facts are computed once during `BuildDesignLayout` (from the `ForwardingMap`) and encoded into `slot_storage_bindings`. No later pipeline stage receives the `ForwardingMap` directly. All downstream layout and codegen stages see only the `SlotStorageBinding` variant, which makes the owned-vs-forwarded distinction structural.

### Instance Storage Base

An instance's storage base (`InstanceStorageBase`) is the arena-absolute offset of its first owned-local slot, not its first logical slot. Instances whose slots are all forwarded aliases have no local storage region (`abs_byte_offset = nullopt`).

### Body Processes and Forwarded Slots

Body processes can reference forwarded alias slots (e.g., a clocked process that reads a pass-through input port without triggering on it). Codegen dispatches on `SpecSlotAccessKind`: owned-local slots use instance-relative addressing (`this_ptr + offset`), forwarded slots use design-global addressing (`design_ptr + canonical_owner_offset`). The classification is precomputed per specialization and validated across all instances.

## Relationship to Specialization

The byte-level layout model refines how SpecLayout works, not what it means:

- `ModuleSpecId` remains the compilation unit
- `this_base + offset` remains the addressing model
- Static offsets for packed fields remain compile-time constants
- Container offsets become elaboration-resolved metadata instead of compile-time constants

Two instances with the same `ModuleSpecId` but different unpacked array sizes share compiled code. They have different layout metadata but identical process functions. This is the natural model: same type, different objects.

## Concept Mapping

| Concept                   | Role                                   |
| ------------------------- | -------------------------------------- |
| `ModuleSpecId`            | Type identity (compilation unit)       |
| `SpecLayout`              | Type-level field layout (body-shaped)  |
| `this_base`               | Object pointer (instance state base)   |
| Instance storage          | Object-owned storage region            |
| Packed field access       | `this_base + constant` (member access) |
| Unpacked container access | `this_base + metadata_offset + index`  |

## Canonical Storage Contract

Lyra owns the byte-level storage layout for all persistent state. LLVM is a codegen vehicle -- its type system describes transient SSA compute form, not storage layout. The two may differ (e.g., wide 4-state values have different padding rules). Crossing between them happens only at explicit storage-boundary helpers.

This contract is defined by `SlotStorageSpec`, resolved once per slot at layout time. All downstream consumers (commit, metadata, initialization) derive layout facts from the resolved spec, never from LLVM type introspection.

### Per-Type Storage Rules

| Type kind                  | Storage form                  | Rule                                                                                                     |
| -------------------------- | ----------------------------- | -------------------------------------------------------------------------------------------------------- |
| Packed 2-state             | Single integer lane           | `GetStorageByteSize(bit_width)` bytes. Power-of-2 up to 8; byte-aligned above 64 bits.                   |
| Packed 4-state             | Two dense integer lanes       | Known lane at offset 0, unknown lane at `lane_byte_size`. No inter-lane padding. Total = 2 \* lane size. |
| Float (real)               | IEEE 754 double               | 8 bytes, 8-byte aligned.                                                                                 |
| Float (shortreal)          | IEEE 754 single               | 4 bytes, 4-byte aligned.                                                                                 |
| Unpacked array             | N elements at constant stride | Stride = align_up(element_size, element_align). Recursive through element spec.                          |
| Unpacked struct            | Fields at computed offsets    | Each field aligned per its spec. Recursive through field specs.                                          |
| Unpacked union             | Max-member overlay            | Size = max member size, aligned to max member alignment. All bytes written on every store.               |
| Handle (string, container) | Pointer-width slot            | Committed through separate handle paths, not canonical storage materialization.                          |

### Key Invariant

No typed LLVM object is ever stored directly to storage bytes. All stores go through `EmitStoreToCanonicalStorage` (which decomposes aggregates recursively) or `EmitPackedToCanonicalBits` (which flattens scalars to canonical integer form). This ensures the byte-level layout matches the canonical contract, not LLVM's internal padding/alignment choices.

## Non-Goals

- Packed types remain specialization boundaries. Packed width affects arithmetic instructions and must produce different compiled code.
- Dynamic arrays, queues, and associative arrays remain heap-allocated handle objects. The byte-level layout applies to fixed-size unpacked containers only.
