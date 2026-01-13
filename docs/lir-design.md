# LIR Design

LIR (Low-level IR) is a register-based IR using Static Single Assignment (SSA) form, designed for interpretation and analysis.

## SSA Principles

LIR follows LLVM-style SSA semantics:

- **SSA values** are symbolic IDs (names like `%t0`) representing exactly one definition, mapped to runtime values via a table
- **Labels** are control-flow metadata, not SSA values - stored in dedicated instruction fields, not operands
- **Constants** are compile-time entities - the `kConstant` instruction materializes them into SSA values
- **Operands are SSA values only** - no variables, constants, or labels in the operand list

## Pointer Types

Pointer-ness is a **type distinction**, not a separate SSA class. An SSA value with type `Pointer<T>` represents an address. Both value-typed and pointer-typed SSA values share the same ID space and temp table.

```
SSAValue (unified ID space)
    ├── type = int              → RuntimeValue = int64_t
    ├── type = logic[7:0]       → RuntimeValue = BitVector
    └── type = Pointer<T>       → RuntimeValue = PointerValue
```

This follows LLVM's model where `getelementptr` produces an SSA value whose type is a pointer.

## Addressing and Access

Pointer types enable clean separation of **addressing** (where) from **access** (what):

| Category   | Operations                                                   | Effect                                                   |
| ---------- | ------------------------------------------------------------ | -------------------------------------------------------- |
| Addressing | `ResolveVar`, `ResolveIndex`, `ResolveField`, `ResolveSlice` | Produce pointer-typed SSA values (pure, no side effects) |
| Access     | `Load`, `Store`, `StoreNBA`                                  | Consume pointer-typed SSA values (side effects)          |

Example for `arr[i].field = value`:

```
%p0 : Pointer<arr_t>   = resolve_var arr
%p1 : Pointer<elem_t>  = resolve_index %p0, %i
%p2 : Pointer<field_t> = resolve_field %p1, 2
store %p2, %value
```

Type checking at compile time:

- `resolve_index` requires `Pointer<Array<T>>`, produces `Pointer<T>`
- `store` requires `Pointer<T>` and `T`

## Value Representation

Two value types with distinct purposes:

**Constant** (compile-time): Immutable values from source code.

- Used in MIR/LIR for constant expressions
- `kConstant` instruction materializes into SSA value

**RuntimeValue** (runtime): Execution state in interpreter.

- Mutable, supports all runtime operations
- Variant includes `PointerValue` for pointer-typed SSA values

The separation follows LLVM's design where `llvm::Constant` is distinct from runtime values.

## Intrinsic Operations

Intrinsic operations (`.size()`, `.push_back()`, `.next()`) are compiler intrinsics with fixed semantics. They are NOT user-defined functions.

### Method vs Op

The key distinction is **identity** (reference vs value semantics):

| Category             | Semantics                | Example                         |
| -------------------- | ------------------------ | ------------------------------- |
| **Intrinsic Method** | Receiver has identity    | `array.size()`, `queue.push(x)` |
| **Intrinsic Op**     | Operands are pure values | `enum.next(v)`, `enum.name()`   |

### Dispatch

| Kind   | Resolved at      | LIR stores               | Interpreter |
| ------ | ---------------- | ------------------------ | ----------- |
| Method | MIR→LIR lowering | Function pointer         | Direct call |
| Op     | MIR→LIR lowering | Enum (`IntrinsicOpKind`) | Switch      |

Some ops fold to constants at lowering (`enum.first()`, `$bits(type)`) and never become runtime instructions.

### Supported Operations

| Receiver    | Operations                                                             |
| ----------- | ---------------------------------------------------------------------- |
| array/queue | `size()`, `delete()`                                                   |
| queue       | `push_back()`, `push_front()`, `pop_back()`, `pop_front()`, `insert()` |
| enum        | `first()`, `last()`, `next()`, `prev()`, `name()`                      |

## Elaboration Models

| Path                    | Elaboration                    | Variable Access                           |
| ----------------------- | ------------------------------ | ----------------------------------------- |
| C++ codegen             | Runtime (C++ builds hierarchy) | `this->u_child_.value` - member traversal |
| MIR → LIR → Interpreter | Compile-time (slang resolves)  | `$symbol` - flat lookup by unique pointer |

## Design Guidance

- **Variable access**: symbol is a unique compile-time resolved address; `ResolveVar` produces a pointer-typed SSA value
- **Operations** (arithmetic, control flow): register-based, explicit data flow
- **Method calls on complex types**: object pointer as receiver, call instruction for methods
