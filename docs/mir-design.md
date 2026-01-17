# MIR Design

MIR (Mid-level IR) fixes all SystemVerilog execution semantics.

## Pipeline Position

```
SystemVerilog -> Slang AST -> HIR -> MIR -> LLVM IR
                                      |
                                      +-> Interpreter
```

HIR freezes _what the language means_.
MIR freezes _how the language must be executed_, while remaining platform-independent.

Guiding question when designing MIR:

> If a backend still needs to "understand SystemVerilog rules" to run correctly, then MIR has failed to fully fix the semantics.

MIR is correct when execution behavior is no longer inferable, only executable.

## Core Principles

These are hard rules, not guidelines:

| Rule                                    | Rationale                                               |
| --------------------------------------- | ------------------------------------------------------- |
| Place and Operand are strictly separate | Computation uses Operand; writes occur through Place    |
| Every basic block has one terminator    | Control flow is explicit and complete                   |
| Suspension operations are terminators   | Delay/Wait yield control; they cannot be instructions   |
| System subroutines classified by role   | Three semantic categories, not per-API instructions     |
| TypeId shared from HIR unchanged        | MIR annotates Operands with types; does not create them |
| No frontend dependencies                | MIR owns all data; no lifetime leakage                  |
| No LLVM/ABI details                     | Platform-independent; backends handle lowering          |
| No scheduling or execution policy       | Runtime strategy is backend responsibility              |

## Structure

```
Design
  -> DesignElement* (Module | Package)
       -> Process* / Function*
            -> BasicBlock*
                 -> Instruction*
                 -> Terminator (exactly one)
```

### DesignElement

A variant over Module and Package. Currently supports:

- `Module` — contains processes and functions
- `Package` — placeholder for package contents

### Process

A coroutine unit that may suspend and resume. Composed of basic blocks with an entry block.

Two kinds (normalized in HIR, not MIR):

- `Once` — corresponds to `initial`
- Looping process kind — corresponds to `always`

Looping processes are **not** expanded to `while(true)` in MIR. Repetition is expressed via process semantics and the `Repeat` terminator.

### Function vs Process

Functions and Processes have different termination rules:

| Construct | Allowed Terminators          | Completion       |
| --------- | ---------------------------- | ---------------- |
| Function  | Control, Return              | Return only      |
| Process   | Control, Suspend, Completion | Finish or Repeat |

**Functions cannot suspend.** Delay and Wait terminators are forbidden in functions. This is a language-level constraint guaranteed by HIR; violations during lowering are internal errors.

### BasicBlock

Each basic block consists of:

- **Instructions**: ordered list, compute values and write to Places
- **Terminator**: exactly one, always last, determines next control state

## Design Heritage

MIR borrows **structural ideas** from Rust MIR:

- Place/Value separation
- CFG with basic blocks and terminators
- Explicit control flow

However, MIR **rejects** Rust's operand and temporary model. SystemVerilog has no ownership semantics (no Copy/Move distinction), and MIR does not use SSA or value numbering. All computation results are materialized into Places; there is no abstract "value identity" separate from storage.

## Place and Operand

MIR uses exactly two operand concepts. Do not introduce a unified operand that can be either.

### Place

A writable location. Structure:

- **PlaceRoot**: identifies the storage (Local, Temp, or Design)
- **Projection sequence**: field access, index access, slice access, dereference

Place is not a value and cannot participate in computation directly. Temporaries (Temp) are compiler-generated Places used to hold intermediate computation results.

### Operand

A readable operand. Three kinds only:

- **Const**: a constant value
- **Use**: read from a Place (implicit load)
- **Poison**: invalid / unreachable value

Place-read is implicit—there is no explicit Load instruction. Reading a Place produces an Operand directly. This is the **implicit read model**: `Operand::Use(place)` means "the current value stored at place."

All computation results are assigned to Places (often temporaries). To use a result, you read from that Place. This avoids the need for value numbering or SSA.

## Instructions

Instructions do not affect control flow. Three variants:

| Variant | Structure       | Purpose                           |
| ------- | --------------- | --------------------------------- |
| Assign  | Place = Operand | Data movement                     |
| Compute | Place = Rvalue  | Computation with result           |
| Effect  | EffectOp        | Side effect only, no result value |

**Rvalue kinds:** Unary, Binary, Cast, Call (pure system functions only).

**No Load instruction.** Reading a Place is implicit in Operand (Use kind). This keeps the operand model simple: Operands are inputs, Places are outputs.

### System Subroutine Classification

Three semantic roles only (fixed classification, does not grow with API count):

| Role   | Description                    | MIR Representation   | Examples              |
| ------ | ------------------------------ | -------------------- | --------------------- |
| Pure   | No side effects                | Rvalue (kCall)       | `$clog2`, `$bits`     |
| Effect | Immediate observable effect    | Effect instruction   | `$display`, `$write`  |
| State  | Mutation of simulation runtime | Effect or Terminator | `$monitor`, `$finish` |

No system task (void-returning subroutine) is represented as a value-producing Rvalue. Effect system tasks have fixed, backend-agnostic observable behavior.

## Terminator Categories

Terminators end a basic block and determine the next control state.

| Category   | Terminators                                           |
| ---------- | ----------------------------------------------------- |
| Control    | Jump, Branch (conditional), Switch (multi-way)        |
| Suspension | Delay (with resume target), Wait (with resume target) |
| Completion | Return, Finish, Repeat                                |

### Why Delay and Wait Are Terminators

Delay and Wait suspend execution and yield control to the simulation runtime. They cannot continue sequential execution.

Decision rule:

> If execution does not continue immediately, it must be a terminator.

### Suspend and Resume

A block ending with a suspension terminator (Delay or Wait) has **no CFG successors**. The runtime/scheduler re-enters execution at a **distinct resume block**. This resume block is a semantic continuation, not a CFG edge.

This means suspension terminators carry a resume target as data, but do not create a CFG edge to it.

## Loops and Repetition

### Language Loops

`while`, `for`, and `repeat` constructs are represented using control-flow graphs with branches and jumps.

### Process Repetition

Always-style repetition is represented by:

- Looping process kind on the process
- Repeat terminator to return to entry

Not expanded into a loop at MIR level. Backend decides the implementation strategy.

## What MIR Does NOT Contain

| Excluded             | Belongs in     |
| -------------------- | -------------- |
| LLVM instructions    | LLVM lowering  |
| ABI details          | Backend        |
| Scheduling/queues    | Runtime        |
| Pointer arithmetic   | Backend        |
| Frontend pointers    | Never          |
| SSA/phi nodes        | LLVM if needed |
| Elaborated instances | Runtime        |

## Invariants

These must hold for well-formed MIR:

**Structural:**

- Every basic block has exactly one terminator
- All control flow is explicit; no implicit fallthrough
- After lowering, no unsealed blocks remain

**Operand Discipline:**

- Place and Operand are strictly separated
- Operands are inputs (Const, Use, Poison)
- Writes occur only through Place
- All computation results materialize into Places (no abstract value identity)

**System Subroutines:**

- Classified only by semantic role (Pure/Effect/State)
- No system task produces a value (Effect instruction only)

**Process Semantics:**

- `initial` and `always` are normalized before MIR
- Looping behavior expressed via process semantics, not control-flow expansion
- Functions cannot suspend; only Processes may use Delay/Wait

**Error Policy:**

- Lowering assumes HIR invariants are satisfied
- Violations during lowering produce internal errors, not user diagnostics
- MIR does not emit user-facing error messages

## Summary

**MIR structure:** Design -> DesignElement (Module|Package) -> Process/Function -> BasicBlock -> Instruction + Terminator

**Operand model:** Place (writable location) and Operand (readable: Const/Use/Poison)

**Instruction model:** Assign, Compute, Effect

**Core fixed semantics:**

- Instruction writes Rvalue to Place (no explicit Load — Use is implicit)
- Delay and Wait as suspension terminators
- System subroutines as Pure, Effect, or State
- Looping behavior as process repetition
