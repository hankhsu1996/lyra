# LIR Design

LIR (Low-level IR) is a register-based IR using Static Single Assignment (SSA) form, designed for interpretation and analysis.

## SSA Principles

LIR follows LLVM-style SSA semantics:

- **SSA values** are symbolic IDs (names like `%t0`) representing exactly one definition, mapped to runtime values via a table
- **Labels** are control-flow metadata, not SSA values - stored in dedicated instruction fields, not operands
- **Constants** are compile-time entities - the `kConstant` instruction materializes them into SSA values
- **Operands are SSA values only** - no variables, constants, or labels in the operand list

## Reference Types

Reference-ness is a **type distinction**, not a separate SSA class. Both value-typed and reference-typed SSA values share the same ID space and temp table.

```
SSAValue (unified ID space)
    ├── type = int              → RuntimeValue = int64_t
    ├── type = logic[7:0]       → RuntimeValue = BitVector
    ├── type = Pointer<T>       → RuntimeValue = PointerValue (addressable storage)
    └── type = SliceRef<T>      → RuntimeValue = SliceRefValue (packed bit slice)
```

**Key distinction:**

- `Pointer<T>` - addressable storage, can read/write independently
- `SliceRef<T>` - packed bit slice, writes require read-modify-write of container

This follows LLVM's model where `getelementptr` produces an SSA value whose type is a pointer.

## Addressing and Access

Reference types enable clean separation of **addressing** (where) from **access** (what):

**Unpacked (addressable) data:**

| Operation      | Input                       | Output       | Purpose                  |
| -------------- | --------------------------- | ------------ | ------------------------ |
| `ResolveVar`   | SymbolRef                   | `Pointer<T>` | Address of variable      |
| `ResolveIndex` | `Pointer<Array<T>>`, index  | `Pointer<T>` | Address of array element |
| `ResolveField` | `Pointer<Struct>`, field_id | `Pointer<F>` | Address of struct field  |
| `Load`         | `Pointer<T>`                | `T`          | Read from location       |
| `Store`        | `Pointer<T>`, `T`           | void         | Write to location        |
| `StoreNBA`     | `Pointer<T>`, `T`           | void         | Non-blocking write       |

**Packed (bit-slice) data:**

| Operation      | Input                         | Output        | Purpose                     |
| -------------- | ----------------------------- | ------------- | --------------------------- |
| `ResolveIndex` | `Pointer<PackedArray>`, index | `SliceRef<T>` | Slice ref to packed element |
| `ResolveSlice` | `Pointer<T>`, offset, width   | `SliceRef<U>` | Slice ref to bit range      |
| `LoadSlice`    | `SliceRef<T>`                 | `T`           | Extract bits from storage   |
| `StoreSlice`   | `SliceRef<T>`, `T`            | void          | Read-modify-write bits      |
| `ExtractBits`  | value, offset, width          | `T`           | Rvalue bit extraction       |

Note: `ResolveIndex` returns `Pointer<T>` for unpacked and `SliceRef<T>` for packed. The type system enforces the distinction.

**Example: unpacked `arr[i].field = value`**

```
%p0 : Pointer<arr_t>   = resolve_var arr
%p1 : Pointer<elem_t>  = resolve_index %p0, %i
%p2 : Pointer<field_t> = resolve_field %p1, 2
store %p2, %value
```

**Example: packed `vec[7:4] = value`**

```
%p0 : Pointer<logic[7:0]> = resolve_var vec
%s0 : SliceRef<logic[3:0]> = resolve_slice %p0, 4, 4
store_slice %s0, %value
```

**Example: non-addressable `(a+b)[7:4]`**

```
%sum = add %a, %b
%val = extract_bits %sum, 4, 4
```

## Value Representation

Two value types with distinct purposes:

**Constant** (compile-time): Immutable values from source code.

- Used in MIR/LIR for constant expressions
- `kConstant` instruction materializes into SSA value

**RuntimeValue** (runtime): Execution state in interpreter.

- Mutable, supports all runtime operations
- Variant includes `PointerValue` for `Pointer<T>` and `SliceRefValue` for `SliceRef<T>`

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

## Design Decisions

**Why SliceRef instead of Pointer for packed access?**

Packed slices are not addressable storage - writes require read-modify-write of the enclosing container. Using a distinct type (`SliceRef`) preserves this semantic distinction and prevents later passes from incorrectly treating packed slices as independently addressable memory.

**Why separate LoadSlice/StoreSlice instead of extending Load/Store?**

The complexity of read-modify-write belongs in dedicated instructions, not hidden inside a generic `Store` with type-based switching. This makes the IR's semantics explicit and visible for correctness checking and optimization.

**Why ExtractBits for non-addressable expressions?**

Expressions like `(a+b)[7:4]` have no storage location. Using `ExtractBits` correctly represents this as an rvalue transformation. Manufacturing fake storage to reuse lvalue mechanisms would pollute the IR with bogus temporaries and create incorrect aliasing assumptions.

**Why one canonical form instead of keeping LoadPackedBits/StorePackedBits?**

Two semantic encodings for the same operation creates maintenance burden - every optimization and analysis must handle both forms. One canonical IR form; optimization can lower to specialized instructions in a later phase if needed.
