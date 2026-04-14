# MIR Design

MIR (Mid-level IR) lowers execution structure to control-flow plumbing: basic blocks, terminators, Place/Operand separation, and effect sequencing.

## Pipeline Position

```
                        per compilation unit
                   +---------------------------------+
frontend output -> | HIR -> XIR -> MIR -> LLVM IR    | -> per-unit artifact
                   +---------------------------------+
```

XIR freezes _what the execution model is_.
MIR freezes _how that model maps to control flow and data flow_, while remaining platform-independent.

## Scope

MIR is **per-compilation-unit**. Each MIR artifact represents one module specialization. All Places reference specialization-local storage. No instance paths, no design-global slot IDs, no design-global allocation. See [compilation-model.md](compilation-model.md).

MIR lowering proceeds per-unit without consulting design-global state. Frozen shared context (types, symbols) is read-only input, not a mutable design-wide environment.

Guiding question when designing MIR:

> Does the backend still need to understand execution-model concepts to run correctly?

If yes, those concepts should already be formalized in XIR. MIR's job is to lower XIR's execution structure into explicit control flow, not to invent new execution concepts. MIR is correct when all control flow and data flow are explicit and no execution-model interpretation remains.

"Explicit" means control flow is fully determined, not that constructs are expanded into implementation patterns. Language-level constructs with specific control-flow or diagnostic semantics should remain first-class MIR concepts. Implementation strategies -- boolean materialization, temporary allocation, branch structuring -- belong in backend lowering, not XIR-to-MIR.

## Core Principles

These are hard rules, not guidelines:

| Rule                                    | Rationale                                                               |
| --------------------------------------- | ----------------------------------------------------------------------- |
| Place and Operand are strictly separate | Computation uses Operand; writes occur through Place                    |
| Every basic block has one terminator    | Control flow is explicit and complete                                   |
| Suspension operations are terminators   | Delay/Wait yield control; they cannot be statements                     |
| System subroutines classified by role   | Three semantic categories, not per-API statements                       |
| TypeId shared with type arena           | MIR annotates Operands with types; does not create them                 |
| No frontend object dependencies         | No AST pointers; no slang lifetime leakage                              |
| No LLVM/ABI details                     | Platform-independent; backends handle lowering                          |
| No scheduling or execution policy       | Runtime strategy is backend responsibility                              |
| Semantic constructs are first-class     | Language control-flow semantics become MIR primitives                   |
| No execution-model invention            | Execution concepts must be formalized in XIR before MIR lowers them     |
| Per-unit artifact                       | Each MIR unit is independently producible; no design-wide MIR container |

## Structure

A MIR compilation unit contains:

```
CompilationUnit (one per specialization)
  -> Process* / Function*
       -> BasicBlock*
            -> Statement*
            -> Terminator (exactly one)
```

### Process

A coroutine unit that may suspend and resume. Composed of basic blocks with an entry block.

Two kinds (HIR's 6 process kinds normalize to these 2 during lowering):

- `kOnce` -- `initial`, `final`
- `kLooping` -- `always`, `always_comb`, `always_ff`, `always_latch`

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

- **Statements**: ordered list, compute values and write to Places
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
- **Projection sequence**: field access, index access, slice access

Place is not a value and cannot participate in computation directly. Temporaries (Temp) are compiler-generated Places used to hold intermediate computation results.

### Operand

A readable operand. Three kinds only:

- **Const**: a constant value
- **Use**: read from a Place (implicit load)
- **Poison**: invalid / unreachable value (trap policy: any use is an internal error)

Place-read is implicit -- there is no explicit Load statement. Reading a Place produces an Operand directly. This is the **implicit read model**: `Operand::Use(place)` means "the current value stored at place."

All computation results are assigned to Places (often temporaries). To use a result, you read from that Place. This avoids the need for value numbering or SSA.

## Statements

Statements do not affect control flow. Variants:

| Variant        | Structure                      | Purpose                           |
| -------------- | ------------------------------ | --------------------------------- |
| Assign         | Place = RightHandSide          | Data movement or computation      |
| GuardedAssign  | Place = RightHandSide if guard | Conditional write (OOB safety)    |
| DeferredAssign | Place <= RightHandSide         | Non-blocking assignment (NBA)     |
| Effect         | EffectOp                       | Side effect only, no result value |
| Call           | Place = callee(args)           | User function invocation          |
| BuiltinCall    | Place = receiver.method(args)  | Container-mutating builtins       |

**RightHandSide** can be either an Operand (simple value) or an Rvalue (computation).

**Rvalue kinds:** Unary, Binary, Cast, Aggregate, Concat, IndexValidity, GuardedUse, etc.

**No Load statement.** Reading a Place is implicit in Operand (Use kind). This keeps the operand model simple: Operands are inputs, Places are outputs.

### Guarded Access Operations

SystemVerilog specifies that out-of-bounds (OOB) or unknown (X/Z) indexing is not a runtime error:

- **Read**: returns an "unknown" default (X-filled for 4-state; 0 for 2-state)
- **Write**: becomes a no-op (design state must not change)

MIR represents these semantics explicitly rather than synthesizing them with generic control flow:

| Operation     | Type      | Semantics                                           |
| ------------- | --------- | --------------------------------------------------- |
| IndexValidity | Rvalue    | Computes validity predicate: in_bounds AND is_known |
| GuardedUse    | Rvalue    | `validity ? Use(place) : oob_default`               |
| GuardedAssign | Statement | `if (guard) Assign(dest, rhs); else no-op`          |

**IndexValidity** takes an index operand and bounds, returning a 1-bit 2-state bool. Bounds are stored as logical bounds (always `lower <= upper`); direction handling happens during lowering when computing bit offsets.

**GuardedUse** is the one Rvalue that explicitly names a Place rather than taking it as an Operand. This is necessary because we cannot express "conditionally read" with `Use(place)` alone. The Place is the "where," the predicate is the "whether." OOB default is determined by the result type (X for 4-state, 0 for 2-state).

**GuardedAssign** evaluates its rhs unconditionally; only the write is guarded. For short-circuit semantics (rhs has side effects), the lowering should emit explicit control flow instead.

This design ensures backends do not need to understand SystemVerilog OOB rules; they are explicit in the MIR.

### System Subroutine Classification

Three semantic roles only (fixed classification, does not grow with API count):

| Role   | Description                    | MIR Representation | Examples                         |
| ------ | ------------------------------ | ------------------ | -------------------------------- |
| Pure   | No side effects                | Rvalue (kCall)     | `$clog2`, `$bits`                |
| Effect | Immediate observable effect    | Effect statement   | `$display`, `$write`, `$monitor` |
| State  | Terminates or suspends process | Terminator         | `$finish`, `$stop`, `$fatal`     |

Hard rule: if a syscall changes control state (terminate/suspend), it must be a Terminator. Otherwise it must be an Effect statement. No system task is represented as a value-producing Rvalue.

### Rvalue Info Promotion Rule

System functions that produce values use Rvalue info structs. Classification:

**Keep in `SystemTfRvalueInfo { opcode }` when:**

- The payload is fully represented by opcode plus `Rvalue::operands`
- No extra invariants beyond operand count
- No type-driven coercion or special lifetime/ownership rules

**Promote to dedicated `*RvalueInfo` when any of these apply:**

- Type-driven coercion is required (e.g., packed-to-string conversion)
- Optional/structured arguments should be represented semantically
- Additional invariants must be enforced beyond operand count
- Special resource/lifetime handling is involved

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

**State across suspension**: Because all values are materialized into Places, suspension only needs a resume target. State is carried by storage (locals/temps frame + design slots), not SSA liveness.

## Loops and Repetition

### Language Loops

`while`, `for`, and `repeat` constructs are represented using control-flow graphs with branches and jumps.

### Process Repetition

Always-style repetition is represented by:

- Looping process kind on the process
- `Repeat` terminator to return to entry

Not expanded into a loop at MIR level. Backend decides the implementation strategy.

**Terminology**: The `Repeat` terminator (process-level) is distinct from the `repeat` statement (language-level loop). The former restarts a process; the latter lowers to Branch/Jump like other loops.

## What MIR Does NOT Contain

| Excluded                                      | Belongs in               |
| --------------------------------------------- | ------------------------ |
| LLVM instructions                             | LLVM lowering            |
| ABI details                                   | Backend                  |
| Scheduling/queues                             | Runtime                  |
| Pointer arithmetic                            | Backend                  |
| Frontend pointers                             | Never                    |
| SSA/phi nodes                                 | LLVM if needed           |
| Elaborated instances                          | Runtime                  |
| Synthesized implementation                    | Backend                  |
| Structured control flow                       | XIR                      |
| Execution-model objects (closures, observers) | XIR                      |
| Design-wide container                         | Never -- MIR is per-unit |

"Synthesized implementation" means expanding a semantic construct into a pattern of primitives. If a language feature has specific evaluation or diagnostic rules, those rules should be expressed as MIR metadata or a dedicated construct -- not as a procedural recipe of boolean operations, temporaries, and branches.

## Invariants

These must hold for well-formed MIR:

**Structural:**

- Every basic block has exactly one terminator
- All control flow is explicit; no implicit fallthrough
- After lowering, no unsealed blocks remain
- Each MIR unit is per-compilation-unit; no design-wide MIR container

**Operand Discipline:**

- Place and Operand are strictly separated
- Operands are inputs (Const, Use, Poison)
- Writes occur only through Place
- All computation results materialize into Places (no abstract value identity)

**Index Projections:**

- IndexProjection stores 0-based storage offset, not declaration-space index
- Lowering normalizes array indices (e.g., `arr[2]` for `int arr[2:5]` becomes offset 0)
- IndexValidity retains logical bounds for diagnostic purposes

**System Subroutines:**

- Classified only by semantic role (Pure/Effect/State)
- No system task produces a value (Effect statement only)

**Process Semantics:**

- `initial` and `always` are normalized before MIR
- Looping behavior expressed via process semantics, not control-flow expansion
- Functions cannot suspend; only Processes may use Delay/Wait

**Abstraction Level:**

- Language constructs with specific semantics remain first-class MIR concepts
- Implementation strategy is backend responsibility, not lowering
- Execution-model concepts should already be formalized in XIR; MIR lowers them

**Error Policy:**

- Lowering assumes input invariants are satisfied
- Violations during lowering produce internal errors, not user diagnostics
- MIR does not emit user-facing error messages

## Summary

**MIR structure:** CompilationUnit -> Process/Function -> BasicBlock -> Statement + Terminator

**Operand model:** Place (writable location) and Operand (readable: Const/Use/Poison)

**Statement model:** Assign, GuardedAssign, DeferredAssign, Effect, Call, BuiltinCall

**Core fixed semantics:**

- Statement writes RightHandSide to Place (no explicit Load -- Use is implicit)
- Delay and Wait as suspension terminators
- System subroutines as Pure, Effect, or State
- Looping behavior as process repetition
- Execution-model structure formalized in XIR, lowered here to control flow
- Each MIR unit is a concrete per-compilation-unit artifact
