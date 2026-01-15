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

| Rule                                  | Rationale                                             |
| ------------------------------------- | ----------------------------------------------------- |
| Place and Value are strictly separate | Computation uses Value; writes occur through Place    |
| Every basic block has one terminator  | Control flow is explicit and complete                 |
| Suspension operations are terminators | Delay/Wait yield control; they cannot be statements   |
| System subroutines classified by role | Three semantic categories, not per-API instructions   |
| TypeId shared from HIR unchanged      | MIR annotates Values with types; does not create them |
| No frontend dependencies              | MIR owns all data; no lifetime leakage                |
| No LLVM/ABI details                   | Platform-independent; backends handle lowering        |
| No scheduling or execution policy     | Runtime strategy is backend responsibility            |

## Structure

```
Design
  -> DesignElement*
       -> Process*
            -> BasicBlock*
                 -> Statement*
                 -> Terminator (exactly one)
```

### DesignElement

Uses LRM terminology. Module, program, interface, package, checker, primitive, and class are all DesignElement kinds.

Contains:

- Static declarations (net, variable, parameter, function, task, type)
- Processes (normalized from HIR)
- Generate constructs (if preserved)

### Process

A coroutine unit that may suspend and resume. Composed of basic blocks with an entry block.

Two kinds (normalized in HIR, not MIR):

- `ProcessKind::Once` — corresponds to `initial`
- `ProcessKind::Looping` — corresponds to `always`

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

- **Statements**: ordered list, perform actions but do not alter control flow
- **Terminator**: exactly one, always last, determines next control state

## Design Heritage

MIR borrows **structural ideas** from Rust MIR:

- Place/Value separation
- CFG with basic blocks and terminators
- Explicit control flow

However, MIR **rejects** Rust's operand and temporary model. SystemVerilog has no ownership semantics (no Copy/Move distinction), and MIR does not use SSA or value numbering. All computation results are materialized into Places; there is no abstract "value identity" separate from storage.

## Place and Value

MIR uses exactly two operand concepts. Do not introduce a unified operand that can be either.

### Place

A writable location. Structure:

- **PlaceRoot**: variable, parameter, or temporary
- **Projection sequence**: field access, index access, slice access, dereference

Place is not a value and cannot participate in computation directly. Temporaries are compiler-generated Places used to hold intermediate computation results.

### Value

A readable operand. Three kinds only:

- **Const**: a constant (inline or referenced from ConstantArena)
- **PlaceRead**: read from a Place (implicit load)
- **Undef**: undefined value

Place-read is implicit—there is no explicit Load statement. Reading a Place produces a Value directly. This is the **implicit read model**: `Value::Read(place)` means "the current value stored at place."

All computation results are assigned to Places (often temporaries). To use a result, you read from that Place. This avoids the need for value numbering or SSA.

**Example:** `x = 1 + 2` lowers to:

```
temp = BinaryOp(Add, Const(1), Const(2))   // result in temp
x = PlaceRead(temp)                         // copy to x
```

Or if the backend can fold:

```
x = BinaryOp(Add, Const(1), Const(2))      // result directly in x
```

## Statement Categories

Statements perform actions but do not alter control flow. Every statement that produces a result assigns to a destination Place.

| Category    | Statements                                           |
| ----------- | ---------------------------------------------------- |
| Assignment  | Assign (Value -> Place), EnqueueNBA                  |
| Computation | Unary op, Binary op, Cast (dest Place, Value)        |
| Call        | Function call, SystemPure, SystemEffect, SystemState |

**No Load statement.** Reading a Place is implicit in Value (PlaceRead kind). This keeps the operand model simple: Values are inputs, Places are outputs.

Statements must be strongly typed so that illegal combinations are rejected at construction.

### System Subroutine Statements

Three semantic roles only (fixed classification, does not grow with API count):

| Role   | Description                    | Examples              |
| ------ | ------------------------------ | --------------------- |
| Pure   | No side effects                | `$clog2`, `$bits`     |
| Effect | Immediate observable effect    | `$display`, `$write`  |
| State  | Mutation of simulation runtime | `$monitor`, `$finish` |

System subroutines take Value arguments only. Writable locations are never passed directly.

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

- `ProcessKind::Looping` on the process
- `Repeat` terminator to return to entry

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

- Place and Value are strictly separated
- Values are inputs (Const, PlaceRead, Undef)
- Writes occur only through Place
- All computation results materialize into Places (no abstract value identity)

**System Subroutines:**

- Classified only by semantic role (Pure/Effect/State)
- Implemented using operation descriptors, not per-API instructions

**Process Semantics:**

- `initial` and `always` are normalized before MIR
- Looping behavior expressed via process semantics, not control-flow expansion
- Functions cannot suspend; only Processes may use Delay/Wait

**Error Policy:**

- Lowering assumes HIR invariants are satisfied
- Violations during lowering produce internal errors, not user diagnostics
- MIR does not emit user-facing error messages

## Summary

**MIR structure:** Design -> DesignElement -> Process -> BasicBlock -> Statement + Terminator

**Operand model:** Place (writable location) and Value (readable operand: Const/PlaceRead/Undef)

**Core fixed semantics:**

- Assign, EnqueueNBA (no explicit Load—PlaceRead is implicit)
- Delay and Wait as suspension terminators
- System subroutines as Pure, Effect, or State
- Looping behavior as process repetition
