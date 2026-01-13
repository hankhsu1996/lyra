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

### Future Direction: Usage-Based Classification

The current classification is **type-based**: integral types become template parameters because they _could_ affect signal types. However, a more precise approach would be **usage-based**: only parameters that _actually_ appear in type expressions need to be template parameters.

**The Variability Paradox**

In real hardware designs, there's an inverse relationship between parameter variability and type-affecting behavior:

| Parameter Category      | Examples               | Variability                   | Affects Types? |
| ----------------------- | ---------------------- | ----------------------------- | -------------- |
| Architectural constants | DATA_WIDTH, ADDR_WIDTH | Low (same across instances)   | Usually yes    |
| Instance configuration  | BANK_ID, SLOT_INDEX    | High (different per instance) | Usually no     |

Consider a memory system with 64 banks:

```systemverilog
module MemoryBank #(
  parameter int DATA_WIDTH = 64,  // Same for all banks, affects logic [DATA_WIDTH-1:0]
  parameter int ADDR_WIDTH = 48,  // Same for all banks, affects logic [ADDR_WIDTH-1:0]
  parameter int BANK_ID = 0       // Different per bank, only used for address comparison
);
```

With type-based classification, all three are template parameters → **64 template specializations** (one per BANK_ID).

With usage-based classification, only DATA_WIDTH and ADDR_WIDTH are templates → **1 template specialization**, 64 instances with different `bank_id_` constructor args.

**Why This Matters**

Parameters that vary most are often the ones that DON'T affect types. This means the current approach creates maximum template bloat for exactly the parameters that could be constructor arguments. This directly impacts the philosophy's #1 priority: minimizing compile time.

**Technical Note: Why Templates Exist**

A common misconception: template parameters are needed for type correctness in generated C++. Actually, slang resolves all types at elaboration time:

- `logic [WIDTH-1:0]` where WIDTH=8 → slang returns type with `bit_width=8`
- `LowerType()` extracts this concrete value
- `ToCppType()` emits `Bit<8>`, not `Bit<WIDTH>`

Templates serve a different purpose: **signature-based deduplication**. Different parameter values create different C++ classes because the resulting types differ. The template parameter is the signature key, enabling `Counter<8>` and `Counter<16>` to be distinct classes.

This means usage-based classification is about **which parameters belong in the signature**, not which parameters affect type correctness. All types are already concrete.

**Implementation Challenge**

Determining whether a parameter affects types requires analyzing:

- Direct usage in type expressions (`logic [WIDTH-1:0]`)
- Indirect usage through localparams (`localparam X = WIDTH; logic [X-1:0]`)
- Cross-module propagation (parameter passed to child module's type-affecting param)

**The Core Problem: Slang Folds Expressions Eagerly**

When slang resolves `logic [WIDTH-1:0]` where WIDTH=8, it evaluates `WIDTH-1` to `7` and stores only the constant `ConstantRange(7, 0)`. The original expression referencing the parameter is discarded. By the time Lyra sees the type, there's no way to know it came from `WIDTH`.

**Explored Approaches and Their Tradeoffs**

We explored multiple approaches to implement usage-based classification. Each has significant drawbacks:

| Approach                     | Description                                                   | Blocker                                                                     |
| ---------------------------- | ------------------------------------------------------------- | --------------------------------------------------------------------------- |
| Analyze slang Type           | Check if Type stores dimension expressions                    | Slang master only stores folded constants, not expressions                  |
| Use EvaluatedDimension       | Access `rangeLeftExpr`/`rangeRightExpr`                       | Would require adding expression fields to upstream slang                    |
| Re-bind from syntax          | Use `DeclaredType::getTypeSyntax()` + `Expression::bind()`    | Requires setting up correct `ASTContext`, essentially re-doing slang's work |
| Syntax name comparison       | Match identifier names in syntax tree against parameter names | Dangerous: misses package scope, imports, hierarchical references           |
| Instance variation heuristic | If parameter varies across instances → constructor arg        | Fragile: depends on specific design, doesn't generalize                     |
| User annotation              | Let users mark parameters as runtime-only                     | Non-standard, user burden, rejected                                         |
| Contribute to upstream slang | Add expression preservation to official slang                 | Uncertain acceptance, slang is a generic library                            |

**Why Each Approach Fails**

1. **Slang Type Analysis**: `PackedArrayType` only stores `ConstantRange range` - the folded value. No expression reference.

2. **EvaluatedDimension Expressions**: Slang's `EvaluatedDimension` could store `rangeLeftExpr`/`rangeRightExpr` fields, but upstream slang master only stores folded constants. Expression preservation would require changes to slang itself.

3. **Re-binding from Syntax**: `DeclaredType::getTypeSyntax()` gives us the parse tree, but syntax nodes have no semantic binding. We'd need to call `Expression::bind(syntax, context)` to get AST expressions we can analyze. This requires:
   - Correct `ASTContext` with proper scope
   - Understanding slang's binding internals
   - Essentially duplicating work slang already did

4. **Syntax Name Comparison**: Looking for identifier names like "WIDTH" in the syntax tree seems simple, but SystemVerilog has complex scoping:
   - Package imports: `import pkg::WIDTH;`
   - Hierarchical references: `parent.WIDTH`
   - Local shadowing
   - Proper binding handles all this; string matching doesn't.

5. **Instance Variation Heuristic**: Detect if a parameter has the same value across all instances (→ template) vs varies (→ constructor). This doesn't generalize: a parameter might vary in one design but not another, and the classification should be based on semantic usage, not coincidental values in a specific design.

**Current Decision: Accept Type-Based Classification**

Given these tradeoffs, Lyra uses **type-based classification** (all integral parameters → template, strings/unpacked → constructor). This is conservative and correct, but creates unnecessary template specializations for non-type-affecting parameters like `BANK_ID`.

The limitation is documented. If template bloat becomes a significant pain point in practice, the most viable path forward would be contributing expression preservation to upstream slang, enabling proper usage-based analysis.

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

Slang stores string literals as bit-packed integers (`"hello"` -> `bit[40]` = `0x68656C6C6F`). The pipeline must convert back to strings:

1. **AST->MIR**: Parameter value stored as `ConstantExpression` (with `is_string_literal` flag) or `ConversionExpression` (integral->string) wrapping a bit-packed constant
2. **Codegen**: `EmitConstantExpression()` detects these cases and emits proper C++ string literals

When parameters flow through module hierarchy (testbench -> cpu -> instr_memory), each level's specialization uses the propagated string value.

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
- `docs/cpp-codegen.md` - General SV->C++ mapping
- `include/lyra/sdk/string_convert.hpp` - FixedString implementation
