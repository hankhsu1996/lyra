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

### Enum as First-Class Type

Enums are `Kind::kEnum`, not a hack on kIntegral. The enum type carries:

- Bit width and signedness (underlying type)
- Type name
- All member names and values

This enables methods like `.next()`, `.prev()`, `.name()` to access enum metadata directly through the type pointer.

## Value Representation

Two value types with distinct purposes:

**Constant** (compile-time): Immutable values from source code.

- `type`: The value's type
- `value`: Storage (integral, string, etc.)
- Used in MIR/LIR for constant expressions

**RuntimeValue** (runtime): Execution state in interpreter.

- `type`: Pointer to interned type
- `value`: Variant holding actual data
- Mutable, supports all runtime operations

The separation follows LLVM's design where `llvm::Constant` is distinct from runtime values. `RuntimeValue::FromConstant()` converts at the interpreter boundary.

## Intrinsic Operations

Intrinsic operations (`.size()`, `.push_back()`, `.next()`, etc.) are compiler intrinsics with fixed semantics known at compile time. They are NOT user-defined functions.

### Method vs Op

The key distinction is **identity** (reference vs value semantics):

| Category             | Semantics                                                           | Example                         |
| -------------------- | ------------------------------------------------------------------- | ------------------------------- |
| **Intrinsic Method** | Receiver has identity (two arrays with same contents are different) | `array.size()`, `queue.push(x)` |
| **Intrinsic Op**     | Operands are pure values (two equal values are indistinguishable)   | `enum.next(v)`, `enum.name()`   |

Enum operations look like methods syntactically (`s.next()`), but the "receiver" is passed by value - no identity. This makes them Ops, not Methods.

### Dispatch

| Kind   | Resolved at      | LIR stores               | Interpreter |
| ------ | ---------------- | ------------------------ | ----------- |
| Method | MIR→LIR lowering | Function pointer         | Direct call |
| Op     | MIR→LIR lowering | Enum (`IntrinsicOpKind`) | Switch      |

Some ops fold to constants at lowering (`enum.first()`, `$bits(type)`) and never become runtime instructions.

### Type Metadata Access

Intrinsic operations access type metadata through type pointers stored in the instruction at lowering time. This follows the principle that all semantic decisions happen at lowering, not runtime.

For `kIntrinsicOp`, the instruction stores a `type_context` field providing the type needed to interpret the operation. This is necessary because `result_type` may differ from the operand type (e.g., `enum.name()` returns string, but needs the enum type for member lookup).

### Supported Operations

| Receiver    | Operations                                                             |
| ----------- | ---------------------------------------------------------------------- |
| array/queue | `size()`, `delete()`                                                   |
| queue       | `push_back()`, `push_front()`, `pop_back()`, `pop_front()`, `insert()` |
| enum        | `first()`, `last()`, `next()`, `prev()`, `name()`                      |

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

### Why separate Constant and RuntimeValue?

- **Semantics**: Constants describe program meaning, RuntimeValues hold execution state
- **Immutability**: Constants are immutable and shareable; RuntimeValues are mutable
- **Clear boundary**: Conversion at interpreter boundary makes the compile-time/runtime split explicit
