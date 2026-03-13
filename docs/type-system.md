# Type System

Lyra's type system follows production compiler patterns: types are interned, values reference types by pointer, and backend is independent of frontend.

## Architecture

**Layering**: slang (frontend) -> lowering (AST->HIR) -> common/ -> HIR/MIR/Codegen/Interpreter

The `common/` layer has no dependency on slang. Type conversion from `slang::ast::Type` to `common::Type` happens exactly once, at the AST->HIR boundary in the lowering layer.

## Type Interning

All types are allocated through a `TypeArena` that interns them:

- Structurally identical types share a single instance
- Types compared by pointer equality (fast)
- Arena uses bump allocation (types never individually freed)
- Arena outlives all values that reference types

Nested type references (e.g., array element type) use raw pointers to interned types, not shared_ptr. This is safe because the arena owns all types and outlives all references.

## Type Kinds

| Kind              | Description                       |
| ----------------- | --------------------------------- |
| kVoid             | No value                          |
| kIntegral         | bit, logic, int, byte, etc.       |
| kReal, kShortReal | Floating point                    |
| kString           | Dynamic string                    |
| kEnum             | Enum with member names and values |
| kUnpackedArray    | Fixed-size array                  |
| kDynamicArray     | Resizable array                   |
| kQueue            | Bounded or unbounded queue        |
| kPackedStruct     | Struct stored as bitvector        |
| kUnpackedStruct   | Struct with independent fields    |
| kUnpackedUnion    | Union with shared storage         |
| kPointer          | Address of storage location       |

### Pointer Types

Pointer types (`Pointer<T>`) represent addresses of storage locations. In MIR, these correspond to Place with Projection - see [mir-design.md](mir-design.md).

### Enum as First-Class Type

Enums are `Kind::kEnum`, not a hack on kIntegral. The enum type carries:

- Bit width and signedness (underlying type)
- Type name
- All member names and values

This enables methods like `.next()`, `.prev()`, `.name()` to access enum metadata directly through the type pointer.

## Design Decisions

### Why intern types?

- **Memory**: Each unique type exists once
- **Speed**: Pointer comparison instead of structural comparison
- **Dispatch**: Type pointer usable as map key

### Why bump allocator?

- Types are created at compile time, live until end
- Never individually freed
- Fast allocation, no fragmentation

### Why no slang in backend?

- **Modularity**: Backend could work with different frontends
- **Testing**: Test backend without parsing SystemVerilog
- **Stability**: External API changes don't ripple through backend
- **Lifetime**: Backend types outlive frontend compilation

## Type Ownership Boundaries

A single SystemVerilog type encodes facts owned by different phases of the compilation model. `TypeArena` serves the runtime/codegen world -- it captures the full type shape needed for value operations, layout computation, and code generation. But not all type facts are relevant to all consumers.

**Compile-owned type facts** (specialization identity): packed width, signedness, two-state vs four-state, packed array element type and range, struct/union field layout, enum base type and member values, unpacked container element type. These are captured in `CompileOwnedTypeStore` as structured, pointer-free, hashable descriptors. Two instances with different compile-owned type facts require different specializations.

**Constructor-owned type facts** (realization metadata): unpacked array dimensions, queue bounds, container sizing. These affect instance layout but not compiled code. They are resolved during realization and do not split specializations.

**Runtime-owned state**: field values, container contents, dynamic storage. Changes during simulation.

`TypeArena` captures both compile-owned and constructor-owned properties in a unified representation. `CompileOwnedTypeStore` is a separate projection that captures only compile-owned facts, stripping constructor-owned properties (dimensions, bounds) and names. Both derive from the same frontend types.

See [compilation-model.md](compilation-model.md) for the full ownership model and specialization boundary rules.

## Four-State Representation

SystemVerilog has 4-state logic (0, 1, X, Z). The representation uses separate masks:

| Component | Purpose                          |
| --------- | -------------------------------- |
| value     | The 0/1 bit pattern              |
| x_mask    | Bits that are X (unknown)        |
| z_mask    | Bits that are Z (high-impedance) |

**Invariant:** `x_mask & z_mask == 0` - a bit cannot be both X and Z. Violation is an internal compiler error.

Two-state values have no masks (or all-zero masks). Types determine whether a value is two-state or four-state; the layout follows from the type.
