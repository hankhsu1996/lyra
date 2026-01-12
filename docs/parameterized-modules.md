# Parameterized Modules

How Lyra handles SystemVerilog parameterized modules: per-instance IR semantics and C++ template codegen.

## Per-Instance IR, Deduplicated Output

### slang's Symbol Model

slang creates a distinct `InstanceBodySymbol` for **each module instance**, even with identical parameters:

```
inner #(.VALUE(100)) inst1();  ->  inst1.body -> VariableSymbol("data") @ 0x1000
inner #(.VALUE(100)) inst2();  ->  inst2.body -> VariableSymbol("data") @ 0x2000
```

Each instance is a complete, independent scope with unique symbol pointers.

### Why Per-Instance IR Matters

MIR/LIR preserve slang's per-instance model. This is essential for:

- **Hierarchical references**: `inst2.data` must resolve using `inst2`'s specific symbols
- **Interpreter correctness**: Instance contexts use symbol pointers for O(1) lookup
- **Module linking**: `SubmoduleInstance` links to child modules by instance symbol pointer

If we deduplicated at IR construction time (keeping only `inst1`'s module), hierarchical access to `inst2` would fail because the symbol pointers wouldn't match.

### Signature-Based Deduplication at Emit Time

Each unique parameter combination generates one C++ class. A **signature** is a canonical string representation (`counter<8>`, `memory<"data.hex">`) that determines:

1. The C++ template specialization name
2. The output header filename
3. Whether a new class needs to be generated

Deduplication is a **codegen optimization**, applied at emit time by `Codegen::GenerateAllModules()`:

| Layer       | Model                              |
| ----------- | ---------------------------------- |
| MIR/LIR     | Per-instance (preserves semantics) |
| Codegen     | Per-signature (emit optimization)  |
| Interpreter | Per-instance (uses symbols)        |

## Template vs Constructor Parameters

### The Core Insight

Not all value parameters need to be C++ template parameters. The deciding factor: **can this parameter type affect signal data types?**

- **Packed/integral types** can appear in signal type definitions (e.g., `logic [WIDTH-1:0]`)
- **Unpacked types** (strings, unpacked structs, dynamic arrays) cannot

### SV Semantic Guarantee

SystemVerilog guarantees that packed types can only contain packed things:

- Packed structs/unions/arrays can only have integral/packed members
- No strings, no dynamic arrays, no unpacked structs inside packed types

This means: **no recursive type checking needed** - just classify the top-level type.

### Parameter Classification Rule

| Parameter Type Category    | Can Affect Signal Types? | C++ Strategy         |
| -------------------------- | ------------------------ | -------------------- |
| Integral (int, bit, logic) | Yes                      | Template parameter   |
| Packed struct/union/array  | Yes                      | Template parameter   |
| Real, shortreal            | No\*                     | Template parameter   |
| String                     | No                       | Constructor argument |
| Unpacked struct/union      | No                       | Constructor argument |
| Dynamic/associative array  | No                       | Constructor argument |

\*Real types can be template parameters (structural) but cannot be used in type expressions.

This aligns with `docs/philosophy.md`: "runtime parameters for values, templates only for type parameters."

## C++ Template Parameter Types

### The Structural Type Requirement

C++20 non-type template parameters must be **structural types**: scalar types, or literal class types with all public members. `std::string` fails this (private members).

### Type Mapping

`ToCppRawType()` maps SV parameter types to valid C++ template parameter types:

| SystemVerilog Type   | C++ Template Type        | Rationale               |
| -------------------- | ------------------------ | ----------------------- |
| `int`, `integer`     | `int32_t`                | Direct mapping          |
| `longint`            | `int64_t`                | Direct mapping          |
| `bit[N]`, `logic[N]` | `int32_t` or `int64_t`   | Fits in primitive       |
| `string`             | `lyra::sdk::FixedString` | Structural type wrapper |

Note: Different from `ToCppType()` which maps to SDK types (`Int`, `Bit<N>`) for variables.

## String Parameters: FixedString

`FixedString<N>` is a C++20 structural type wrapping a compile-time string. Key design decisions:

- **C-style array, not std::array**: `std::array` has private members, making it non-structural. C-style arrays are the only option.
- **All public members**: Required for structural type qualification.
- **CTAD**: Infers N from string literal, enabling `Module<"hello">` syntax.

This enables: `template <lyra::sdk::FixedString PROGRAM> class memory;`

## The String Parameter Pipeline

Slang stores string literals as bit-packed integers (`"hello"` → `bit[40]` = `0x68656C6C6F`). The pipeline must convert back to strings:

1. **AST→MIR**: Parameter value stored as `LiteralExpression` (with `is_string_literal` flag) or `ConversionExpression` (integral→string) wrapping a bit-packed literal
2. **Codegen**: `EmitConstantExpression()` detects these cases and emits proper C++ string literals

When parameters flow through module hierarchy (testbench → cpu → instr_memory), each level's specialization uses the propagated string value.

## Extending to Other Types

The pattern extends to non-primitive parameter types:

| Type            | Strategy                               |
| --------------- | -------------------------------------- |
| Packed structs  | Structural wrapper or integer encoding |
| Enums           | Underlying integer type                |
| Type parameters | C++ template type parameters           |

Key principle: map to a valid C++ NTTP type while preserving the semantic value.

## Related

- `docs/string-types.md` - How string literals are typed and tracked
- `docs/cpp-codegen.md` - General SV→C++ mapping
- `include/lyra/sdk/string_convert.hpp` - FixedString implementation
