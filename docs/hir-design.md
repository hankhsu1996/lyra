# HIR Design

HIR (High-level IR) is the semantic model of SystemVerilog.

## Pipeline Position

```
SystemVerilog -> Slang AST -> HIR -> MIR -> LLVM IR
                               |
                               +-> C++ (secondary)
```

The frontend (slang) is used only during lowering, then discarded. HIR freezes _what the language means_, not _how it runs_.

### AST Boundary

AST -> HIR lowering is the **error boundary** for user-facing diagnostics:

| Responsibility                                     | Where                            |
| -------------------------------------------------- | -------------------------------- |
| User errors (unsupported features, invalid code)   | AST -> HIR only                  |
| Source locations (slang ranges -> SourceSpan)      | Converted at boundary            |
| Data ownership (slang string_view -> owned string) | Copied at boundary               |
| Compiler bugs after HIR                            | Internal errors, not diagnostics |

After HIR construction, slang resources may be released. Any error in subsequent stages (HIR -> MIR, codegen) indicates a compiler bug, not a user error.

See [error-handling.md](error-handling.md) for error type details.

Guiding question when designing HIR:

> Is this describing language semantics, or execution strategy?

## Core Principles

These are hard rules, not guidelines:

| Rule                                | Rationale                                          |
| ----------------------------------- | -------------------------------------------------- |
| No frontend pointers                | HIR owns all data; slang lifetime must not leak    |
| No temporaries                      | Temporaries are execution artifacts                |
| No SSA or basic blocks              | These model execution, not meaning                 |
| No mode flags for semantic variants | Use distinct node types, not flags on shared nodes |
| Syntactic differences may disappear | `begin/end` vs `{}` are the same in HIR            |
| Semantic differences must not       | Blocking vs non-blocking must remain distinct      |

HIR correctness is defined by invariants (typing, binding, scoping). If those invariants hold, HIR correctly represents the program's meaning.

## Core IDs and Arenas

Three orthogonal axes identify every construct:

| Axis       | Purpose           | Mechanism                    |
| ---------- | ----------------- | ---------------------------- |
| **Type**   | What it is        | `TypeId` via `TypeArena`     |
| **Symbol** | Who it is         | `SymbolId` via `SymbolTable` |
| **Scope**  | Where it is valid | `ScopeId` via `Scope` tree   |

### TypeId / TypeArena

Every language type has a corresponding HIR type. Types are interned for structural sharing and pointer equality comparison. These are _language types_, not LLVM or runtime types.

### SymbolId / SymbolTable

A symbol is a named language entity: variable, net, parameter, function, event. Symbols are appended at declaration time. No slang pointers are stored.

### ScopeId / Scope

Represents semantic visibility. Module, function, and block each introduce a new scope. ScopeId and SymbolId are independent—a symbol exists in a scope, but the ID spaces don't overlap.

### ConstId / ConstantPool

Constants are frontend-known, deduplicable entities—first-class citizens like TypeId. The ConstantPool interns constants for deduplication and consistent identity.

Supported payload kinds:

- Integral (two-state or four-state bitvectors)
- String
- Real (double-precision float)
- Struct (vector of ConstIds)
- Array (vector of ConstIds)

Constants only provide data; types determine layout. A constant's identity (ConstId) is fixed in HIR; MIR and backends only reference or materialize it.

## Type System

HIR must represent all SystemVerilog type semantics:

| Category  | Types                                               |
| --------- | --------------------------------------------------- |
| Integral  | bit, logic, reg, integer, etc. (width + signedness) |
| Arrays    | packed, unpacked, dynamic, associative, queue       |
| Aggregate | struct, union (with packed qualifier)               |
| Other     | string, event, real, time                           |

**Qualifiers** (signed, unsigned, packed, const) are canonical type attributes, part of the TypeArena interning key. This is distinct from "mode flags"—qualifiers don't represent semantic variants; they define distinct types.

## Constant vs Expression

This distinction is fundamental:

| Concept        | Definition                      | Examples                                            |
| -------------- | ------------------------------- | --------------------------------------------------- |
| **Constant**   | Compile-time known atomic value | integral literal, string, real, null                |
| **Expression** | Semantic construction           | binary op, cast, index, member access, initializers |

Rule: _A value is a Constant; any construction process is an Expression._

Struct initialization and array initialization are expressions, not constants—even if all elements are constant. The construction process is semantic information.

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

HIR preserves distinct kinds because `always_comb/ff/latch` have semantic constraints beyond repetition (sensitivity rules, synthesis semantics). The distinction matters for validation and backend lowering.

## System Subroutine Classification

HIR must classify system subroutines into exactly three semantic roles:

| Role   | Description                      | Examples                         |
| ------ | -------------------------------- | -------------------------------- |
| Pure   | No side effects, pure evaluation | `$clog2`, `$bits`                |
| Effect | Immediate observable effect      | `$display`, `$write`, `$monitor` |
| State  | Terminates or suspends process   | `$finish`, `$stop`, `$fatal`     |

This classification is fixed and does not grow with the number of system APIs. MIR mirrors these roles as distinct statement categories.

**Unknown syscalls**: Unrecognized system subroutines are rejected at AST->HIR with a user-facing diagnostic. This is not a runtime error—it's a compile-time "unsupported" error.

Rule: Classify by _what the subroutine does semantically_, not by its name or argument count.

## Time and Event Semantics

All timing constructs unify as "wait for something":

- **Delay**: `wait(duration_expr)` — pause for a time value
- **Event wait**: `wait(trigger_spec)` — pause until condition occurs

Trigger specifications include: posedge, negedge, level, named event, OR-list.

HIR expresses: _"Execution pauses here until this semantic condition occurs."_

HIR does **not** express scheduling, queues, or execution order. Those are MIR concerns.

## What Must NOT Appear in HIR

| Excluded           | Belongs in    |
| ------------------ | ------------- |
| Temporaries        | MIR           |
| Place              | MIR           |
| SSA / basic blocks | MIR           |
| Runtime values     | Interpreter   |
| LLVM intrinsics    | LLVM lowering |
| Execution flags    | MIR           |

If you find yourself adding any of these to HIR, step back—you're modeling execution, not semantics.

## Source Information

HIR maintains source locations for diagnostics:

- Use HIR-owned `SourceSpan` (file ID + range)
- Never depend on frontend lifetime
- Only for error messages and debugging

## HIR to C++ Codegen

Direct HIR -> C++ codegen is allowed as a semantic validation tool. It is:

- Secondary to the MIR -> LLVM path
- Useful for debugging and readable output
- Disposable if it becomes a maintenance burden

C++ codegen must not influence HIR design decisions.

## Summary

**HIR is SystemVerilog with syntax sugar stripped away, leaving only semantic facts.**

If you're thinking about "how this executes" while designing HIR, you've crossed the boundary into MIR territory.
