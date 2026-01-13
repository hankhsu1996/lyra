# Type System

Lyra's type system follows production compiler patterns: types are interned, values reference types by pointer, and backend is independent of frontend.

## Architecture

**Layering**: slang (frontend) -> lowering (AST->MIR) -> common/ -> MIR/LIR/Codegen/Interpreter

The `common/` layer has no dependency on slang. Type conversion from `slang::ast::Type` to `common::Type` happens exactly once, at the AST->MIR boundary in the lowering layer.

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

Pointer types (`Pointer<T>`) represent addresses of storage locations. See `docs/lir-design.md` for how pointer types integrate with SSA and enable separation of addressing from access.

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
