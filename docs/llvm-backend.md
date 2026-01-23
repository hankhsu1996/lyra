# LLVM Backend

Design decisions for MIR -> LLVM IR lowering.

## Pipeline Position

```
HIR -> MIR -> LLVM IR -> executable
              |
              +-> Runtime library (Lyra*)
```

MIR fixes all execution semantics. The LLVM backend only translates--it does not interpret SystemVerilog rules.

## Core Philosophy

Three principles govern all lowering decisions:

| Principle                              | Implication                                                  |
| -------------------------------------- | ------------------------------------------------------------ |
| LLVM does not understand SystemVerilog | MIR must resolve all SV semantics before lowering            |
| Lowering is mechanical, not semantic   | No interpretation, no decision-making in LLVM layer          |
| One MIR construct -> one LLVM shape    | No conditionals that produce different IR for same construct |

If LLVM needs to "understand" SV rules to run correctly, the design has failed.

## Responsibility Split

### MIR Guarantees

Before lowering, MIR has already fixed:

- Rvalue vs Place is explicit
- Bit-width, signedness, and 2-state vs 4-state
- Evaluation order
- Side effects (calls, stores, terminators)

### LLVM Lowering Rules

| Must Do                                | Must NOT Do                      |
| -------------------------------------- | -------------------------------- |
| Translate MIR faithfully               | Infer SV semantics               |
| Emit runtime calls for SV-specific ops | Re-decide evaluation order       |
| Derive layout from TypeId              | Encode language rules implicitly |
| Produce deterministic IR               | Create new types                 |

LLVM is a backend, not a language layer. See [pipeline-contract.md](pipeline-contract.md) for layer boundaries.

## 2-State Execution Contract

The LLVM backend operates in 2-state mode. It does not model X/Z propagation, unknown initialization, or any 4-state semantics. All storage is zeroinit'd (not X-filled). Operations on 4-state-typed values produce correct results only when those values are known 2-state at runtime.

This is a deliberate semantic limitation, not an incidental property.

**Enforcement boundary:** The 2-state contract is enforced at value entry points:

| Entry point    | Mechanism                                             |
| -------------- | ----------------------------------------------------- |
| Constants      | `LowerConstant` rejects integral constants with X/Z   |
| Initialization | LLVM zeroinits all storage (design state, frame)      |
| Future         | Backend entry point rejects 4-state processes/modules |

The `is_four_state` type annotation is irrelevant for computation correctness under this contract. All arithmetic, logic, and cast operations produce correct 2-state results on `iN` integers regardless of whether the type could theoretically hold X/Z.

## Lowering Decision Framework

For every MIR construct, answer three questions:

| Question                 | If Yes                | If No                 |
| ------------------------ | --------------------- | --------------------- |
| Does it produce a value? | LLVM SSA value        | Call / store / branch |
| Is it pure?              | LLVM instruction      | Runtime call          |
| Is it SV-specific?       | Runtime API (`Lyra*`) | Native LLVM op        |

This framework determines the LLVM shape mechanically.

## Lowering Categories

### Pure Arithmetic and Bitwise

Examples: `add`, `sub`, `and`, `xor`, shifts

- MIR ensures operands are normalized
- Use native LLVM instructions: `add`, `and`, `xor`, `shl`, `lshr`, `ashr`
- Signedness matters for shifts and comparisons
- Do not rely on C/C++ overflow semantics

### Comparisons and Conditions

- LLVM branches require 2-state 1-bit booleans
- 2-state comparisons -> `icmp`
- 4-state comparisons -> runtime call (never branch on 4-state directly)

### Constants

- Small scalars -> `ConstantInt`
- Wide 2-state -> global constant arrays + `LyraAssignBits2`
- Wide 4-state -> three globals (value, x_mask, z_mask)
- Layout fully decided by MIR; LLVM does not compute SV layout

### SV-Specific Operations

Always lower to runtime calls:

- `$display`, `$write` (I/O)
- 4-state arithmetic and propagation
- Dynamic arrays, queues
- Timing constructs

## Width Threshold

| Width       | Strategy                                  |
| ----------- | ----------------------------------------- |
| <= 128 bits | Native LLVM integers and operations       |
| > 128 bits  | Global constant blobs + runtime API calls |

The threshold may be tuned, but the principle is fixed: LLVM handles what it's good at; runtime handles the rest.

## Layout Calculation

Layout always derived from TypeId:

| Field         | Calculation            |
| ------------- | ---------------------- |
| bitwidth      | From type              |
| words         | `(bitwidth + 63) / 64` |
| is_four_state | From type              |

Padding bits are not tracked separately. The `bitwidth` defines semantic validity.

## Four-State Representation

Four-state values use three arrays:

| Array  | Purpose             |
| ------ | ------------------- |
| value  | The 0/1 bit pattern |
| x_mask | Unknown bits        |
| z_mask | High-impedance bits |

Invariant: `x_mask & z_mask == 0` (checked in debug builds).

**Branchless propagation** for hot paths--masks flow through bitwise operations so unknown bits contaminate results.

**case/casez/casex** inspect masks directly. Correctness over performance.

## Runtime APIs vs LLVM Intrinsics

**LLVM intrinsics** (`llvm.*`): Only for operations LLVM understands natively--`llvm.memcpy`, `llvm.ctpop`, etc.

**Runtime APIs** (`Lyra*`): For SystemVerilog semantics LLVM cannot express--wide arithmetic, 4-state propagation, SV-specific operations.

## What LLVM Backend Must NOT Do

| Violation                         | Why It's Wrong                 |
| --------------------------------- | ------------------------------ |
| Handle 4-state logic directly     | Runtime responsibility         |
| Decide evaluation order           | MIR already fixed it           |
| Produce different IR for same MIR | Leads to non-determinism       |
| Recompute bit offsets or slices   | MIR owns all address decisions |
| Interpret SV semantics            | MIR is the semantic endpoint   |

## Engineering Guidelines

- One lowering function per MIR construct
- Deterministic IR output (byte-for-byte reproducible)
- Use MIR interpreter as semantic oracle: same test, different result -> LLVM bug
