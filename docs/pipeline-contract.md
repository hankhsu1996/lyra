# Pipeline Contract

This document defines the behavioral boundaries between pipeline stages. It is not an architecture doc (which describes structure)—it enforces rules and guardrails.

## Correctness Authority

**MIR is the semantic truth.**

If MIR is correct, the language semantics are correct. LLVM IR and other backends are translators, not interpreters of SystemVerilog.

## Layer Responsibilities

### AST -> HIR

| Must Resolve           | Must NOT Do              |
| ---------------------- | ------------------------ |
| Syntax normalization   | Execution semantics      |
| Type resolution        | Control flow lowering    |
| Symbol binding         | Temporary introduction   |
| Source span capture    | Basic block construction |
| User error diagnostics |                          |

HIR should still "look like SV", just cleaned and normalized.

### HIR -> MIR

| Must Resolve                     | Must NOT Do                |
| -------------------------------- | -------------------------- |
| Execution order                  | Re-interpret syntax        |
| Data flow (temporaries)          | Emit user diagnostics      |
| Control flow (basic blocks)      | Platform-specific lowering |
| Place/Value separation           |                            |
| System subroutine classification |                            |

MIR defines how the program executes. All semantic questions are answered here.

### MIR -> LLVM IR

| Must Do                            | Must NOT Do              |
| ---------------------------------- | ------------------------ |
| Translate MIR to LLVM              | Fix language semantics   |
| Emit runtime calls for complex ops | Re-interpret MIR meaning |
| Layout based on TypeId             | Create new types         |
|                                    | Emit user diagnostics    |

LLVM IR is not where language semantics live.

## Information Flow

These must flow end-to-end through the pipeline:

| Information | Created In | Used By                          |
| ----------- | ---------- | -------------------------------- |
| SourceSpan  | AST -> HIR | Diagnostics, debugging           |
| TypeId      | HIR        | All stages (read-only after HIR) |
| ConstId     | HIR        | MIR, LLVM (materialization)      |
| SymbolId    | HIR        | MIR (variable resolution)        |
| ValueId     | HIR -> MIR | MIR, LLVM (temp mapping)         |

## Forbidden Cross-Layer Behavior

| Violation                 | Why It's Wrong                                |
| ------------------------- | --------------------------------------------- |
| LLVM fixes SV semantics   | Semantics must be fixed in MIR                |
| MIR re-interprets syntax  | Syntax interpretation is HIR's job            |
| HIR encodes execution     | Execution semantics belong in MIR             |
| Post-HIR user diagnostics | All user errors caught at AST -> HIR boundary |
| Backend creates types     | Types are language-level, owned by HIR        |

## Error Boundaries

| Stage       | User Errors         | Compiler Bugs |
| ----------- | ------------------- | ------------- |
| AST -> HIR  | DiagnosticException | InternalError |
| HIR -> MIR  | N/A                 | InternalError |
| MIR -> LLVM | N/A                 | InternalError |

See [error-handling.md](error-handling.md) for details.

## Debugging Philosophy

- **Debug starts at MIR** - If MIR is correct, look at LLVM lowering
- **LLVM is assumed correct** unless proven otherwise
- **Always be able to dump**: HIR, MIR, LLVM IR

## Guiding Principle

> Build **one correct execution path first**.
> Everything else (C++ codegen, interpreter, optimizations) comes later.

This minimizes debugging surface area and prevents semantic drift.
