# HIR Design

HIR (High-level IR) is the semantic model of SystemVerilog.

## Pipeline Position

```
                        per compilation unit
                   +---------------------------------+
frontend output -> | HIR -> XIR -> MIR -> LLVM IR    | -> per-unit artifact
                   +---------------------------------+
```

The frontend (slang) elaborates the full design, but the compiler does not adopt that whole-design world. At the AST-to-HIR boundary, the compiler establishes its own per-specialization compilation units. HIR freezes _what the language means_, not _how it executes_.

## Scope

HIR is **per-compilation-unit**. The AST-to-HIR boundary is where the compiler breaks the frontend's whole-design output into per-specialization compilation units.

### Structure

HIR consists of two parts:

- **Design shell**: A thin container for unavoidable design-level facts (package/global state, type arenas, symbol tables). This is frozen shared read-only context, not a mutable whole-design compilation environment.
- **Per-specialization bodies**: The real compiler-owned compilation units. Each body contains the processes, functions, tasks, and local declarations for one specialization. Each body owns its own arena and constant pool.

The design shell exists because some facts (package functions, global types) are shared across compilation units. It does not drive lowering. All substantive compilation work operates on per-specialization bodies.

### AST Boundary

AST -> HIR lowering is both the **error boundary** for user-facing diagnostics and the **compilation unit boundary** where the compiler's per-specialization world begins:

| Responsibility                                     | Where                            |
| -------------------------------------------------- | -------------------------------- |
| User errors (unsupported features, invalid code)   | AST -> HIR only                  |
| Specialization grouping and unit establishment     | AST -> HIR boundary              |
| Source locations (slang ranges -> SourceSpan)      | Converted at boundary            |
| Data ownership (slang string_view -> owned string) | Copied at boundary               |
| Compiler bugs after HIR                            | Internal errors, not diagnostics |

After HIR construction, slang resources may be released. Any error in subsequent stages (HIR -> XIR, XIR -> MIR, codegen) indicates a compiler bug, not a user error.

See [error-handling.md](error-handling.md) for error type details.

Guiding question when designing HIR:

> Is this describing language semantics, or execution structure?

## Core Principles

These are hard rules, not guidelines:

| Rule                                | Rationale                                                       |
| ----------------------------------- | --------------------------------------------------------------- |
| No frontend pointers                | HIR owns all data; slang lifetime must not leak                 |
| No temporaries                      | Temporaries are execution artifacts                             |
| No SSA or basic blocks              | These model execution, not meaning                              |
| No mode flags for semantic variants | Use distinct node types, not flags on shared nodes              |
| Syntactic differences may disappear | `begin/end` vs `{}` are the same in HIR                         |
| Semantic differences must not       | Blocking vs non-blocking must remain distinct                   |
| Per-unit bodies are self-contained  | Each body is independently lowerable with frozen shared context |

HIR correctness is defined by invariants (typing, binding, scoping). If those invariants hold, HIR correctly represents the program's meaning.

### Execution-Relevant Semantic Shaping

HIR is primarily semantic, but some execution-relevant shaping is appropriate where it preserves language-level meaning that downstream layers need:

- **Capture classification**: Deferred assertion actions carry explicit value/ref/const-ref capture semantics. This is language meaning (LRM 16.4 defines capture timing), not execution strategy.
- **System subroutine classification**: Pure/Effect/State roles are language-level semantic facts.
- **Assignment target canonicalization**: Root + projection paths encode lvalue semantics, not execution layout.
- **Process kind preservation**: Distinct `always_comb/ff/latch` kinds carry semantic constraints (sensitivity rules, synthesis semantics).

These are not execution structure -- they are semantic facts that XIR and MIR need. The execution-model structure built from these facts belongs in XIR. See [xir-design.md](xir-design.md).

## Core IDs and Arenas

Three orthogonal axes identify every construct:

| Axis       | Purpose           | Mechanism                    |
| ---------- | ----------------- | ---------------------------- |
| **Type**   | What it is        | `TypeId` via `TypeArena`     |
| **Symbol** | Who it is         | `SymbolId` via `SymbolTable` |
| **Scope**  | Where it is valid | `ScopeId` via `Scope` tree   |

TypeArena and SymbolTable are shared across compilation units as frozen read-only context. They are part of the design shell, not per-unit mutable state. Per-unit bodies have their own arenas for expressions, statements, and constants.

### TypeId / TypeArena

Every language type has a corresponding HIR type. Types are interned for structural sharing and pointer equality comparison. These are _language types_, not LLVM or runtime types.

### SymbolId / SymbolTable

A symbol is a named language entity: variable, net, parameter, function, event. Symbols are appended at declaration time. No slang pointers are stored.

### ScopeId / Scope

Represents semantic visibility. Module, function, and block each introduce a new scope. ScopeId and SymbolId are independent -- a symbol exists in a scope, but the ID spaces don't overlap.

### ConstId / ConstantPool

Constants are frontend-known, deduplicable entities -- first-class citizens like TypeId. The ConstantPool interns constants for deduplication and consistent identity. Each per-specialization body owns its own constant pool.

Supported payload kinds:

- Integral (two-state or four-state bitvectors)
- String
- Real (double-precision float)
- Struct (vector of ConstIds)
- Array (vector of ConstIds)

Constants only provide data; types determine layout. A constant's identity (ConstId) is fixed in HIR; XIR, MIR, and backends only reference or materialize it.

## Type System

HIR must represent all SystemVerilog type semantics:

| Category  | Types                                               |
| --------- | --------------------------------------------------- |
| Integral  | bit, logic, reg, integer, etc. (width + signedness) |
| Arrays    | packed, unpacked, dynamic, associative, queue       |
| Aggregate | struct, union (with packed qualifier)               |
| Other     | string, event, real, time                           |

**Qualifiers** (signed, unsigned, packed, const) are canonical type attributes, part of the TypeArena interning key. This is distinct from "mode flags" -- qualifiers don't represent semantic variants; they define distinct types.

## Constant vs Expression

This distinction is fundamental:

| Concept        | Definition                      | Examples                                            |
| -------------- | ------------------------------- | --------------------------------------------------- |
| **Constant**   | Compile-time known atomic value | integral literal, string, real, null                |
| **Expression** | Semantic construction           | binary op, cast, index, member access, initializers |

Rule: _A value is a Constant; any construction process is an Expression._

Struct initialization and array initialization are expressions, not constants -- even if all elements are constant. The construction process is semantic information.

## Statement Model

HIR statements preserve source structure:

| Category     | Statements                                                                      |
| ------------ | ------------------------------------------------------------------------------- |
| Basic        | VariableDeclaration, BlockingAssignment, NonBlockingAssignment                  |
| Control flow | If (nested, not cascaded), Case (distinct from if), While, DoWhile, For, Repeat |
| Flow control | Break, Continue                                                                 |
| Block        | List of statements + scope                                                      |

`if-else if-else` chains are nested if statements, not a special cascade node. Case statements are semantically distinct from if chains and remain so in HIR.

## Process Model

HIR represents all process forms with a single `Process` construct and a `ProcessKind`:

| Kind                        | Corresponds to | Behavior                            |
| --------------------------- | -------------- | ----------------------------------- |
| `ProcessKind::kInitial`     | `initial`      | Runs once                           |
| `ProcessKind::kAlways`      | `always`       | Repeats forever                     |
| `ProcessKind::kAlwaysComb`  | `always_comb`  | Combinational, implicit sensitivity |
| `ProcessKind::kAlwaysFf`    | `always_ff`    | Sequential, clock-edge triggered    |
| `ProcessKind::kAlwaysLatch` | `always_latch` | Latch inference                     |
| `ProcessKind::kFinal`       | `final`        | Runs once at simulation end         |

HIR preserves distinct kinds because `always_comb/ff/latch` have semantic constraints beyond repetition (sensitivity rules, synthesis semantics). The distinction matters for validation and downstream lowering.

## System Subroutine Classification

HIR must classify system subroutines into exactly three semantic roles:

| Role   | Description                      | Examples                         |
| ------ | -------------------------------- | -------------------------------- |
| Pure   | No side effects, pure evaluation | `$clog2`, `$bits`                |
| Effect | Immediate observable effect      | `$display`, `$write`, `$monitor` |
| State  | Terminates or suspends process   | `$finish`, `$stop`, `$fatal`     |

This classification is fixed and does not grow with the number of system APIs. XIR and MIR mirror these roles as distinct categories.

**Unknown syscalls**: Unrecognized system subroutines are rejected at AST->HIR with a user-facing diagnostic. This is not a runtime error -- it's a compile-time "unsupported" error.

Rule: Classify by _what the subroutine does semantically_, not by its name or argument count.

## Time and Event Semantics

All timing constructs unify as "wait for something":

- **Delay**: `wait(duration_expr)` -- pause for a time value
- **Event wait**: `wait(trigger_spec)` -- pause until condition occurs

Trigger specifications include: posedge, negedge, level, named event, OR-list.

HIR expresses: _"Execution pauses here until this semantic condition occurs."_

HIR does **not** express execution-model structure for waits. The execution model (late-bound subscriptions, rebinding plans, observation narrowing) belongs in XIR. See [xir-design.md](xir-design.md).

## What Must NOT Appear in HIR

| Excluded                     | Belongs in                                  |
| ---------------------------- | ------------------------------------------- |
| Temporaries                  | MIR                                         |
| Place / Operand              | MIR                                         |
| SSA / basic blocks           | MIR                                         |
| Execution-model structure    | XIR                                         |
| Closure dispatch / thunk ABI | XIR or MIR                                  |
| Runtime values               | Runtime                                     |
| LLVM intrinsics              | LLVM lowering                               |
| Execution flags              | MIR                                         |
| Design-wide lowering state   | Never -- per-unit bodies are self-contained |

If you find yourself adding execution-model structure to HIR, step back -- you should be modeling it in XIR. If you're adding CFG plumbing, that belongs in MIR. If you're consulting design-global state during per-unit body lowering, the compilation-unit boundary is violated.

## Source Information

HIR maintains source locations for diagnostics:

- Use HIR-owned `SourceSpan` (file ID + range)
- Never depend on frontend lifetime
- Only for error messages and debugging

## Summary

**HIR is SystemVerilog with syntax sugar stripped away, leaving only semantic facts.** It is the layer where the compiler establishes its per-specialization compilation units from the frontend's whole-design output. Each per-unit body is independently lowerable. The design shell provides frozen shared context but does not drive lowering.

If you're thinking about "how this executes" while designing HIR, you've crossed the boundary into XIR territory. If you're thinking about CFG plumbing, you've crossed into MIR territory.
