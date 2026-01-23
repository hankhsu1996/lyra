---
name: explore-mir
description: Plan how to implement a SystemVerilog feature end-to-end (SV -> HIR -> MIR -> Interpreter). Use when adding new language features.
---

# Plan Feature Lowering (SV -> HIR -> MIR)

Guide for implementing a SystemVerilog feature end-to-end, starting from LRM semantics and ending with passing tests in MIR interpreter (and optionally LLVM backend readiness).

## Prerequisites

Read before starting:

- `docs/pipeline-contract.md` - what each layer is responsible for
- `docs/mir-design.md` - MIR semantics + purity/effects model
- `docs/hir-design.md` - HIR node taxonomy + typing rules
- `docs/error-handling.md` - error types, when to use each
- `docs/type-system.md` - type interning, type kinds, 4-state representation

## Mental Model

> The LRM defines semantics. HIR makes semantics explicit and typed.
> MIR is the semantic endpoint executable by the interpreter.
> Backends (LLVM) are translation targets, not semantic decision-makers.

## Process

### Step 1: Pin Down the Semantic Contract (from LRM)

**Inputs can be:**

- "I want this behavior..."
- LRM excerpt text
- LRM PDF section + a named feature

**Outputs (must be written down):**

- Precisely what the feature does (including edge cases)
- Static errors vs runtime behavior
- Type rules (legal operand types, result type)
- Evaluation order / side effects / short-circuit rules
- Any "undefined / implementation-defined" parts and what Lyra chooses

### Step 2: Decide Where the Feature Lives (HIR vs MIR vs Runtime)

Classify the feature:

| Kind                                               | Typical destination                    |
| -------------------------------------------------- | -------------------------------------- |
| Pure expression semantics                          | HIR node (typed), then MIR `Compute`   |
| Control-flow semantics                             | MIR terminators / blocks               |
| Effectful behavior (I/O, scheduling, system tasks) | MIR `Effect` + interpreter runtime     |
| Data model / storage layout                        | Type system + runtime layout subsystem |
| Compile-time-only semantics                        | AST/HIR constant folding / elaboration |

Hard rule: **if lowering would need to "guess" semantics, you're missing a field or node.**

### Step 3: Pick a Representation Strategy

Try these in order:

1. **Reuse an existing HIR node** (e.g., cast/unary/binary/etc.)
2. **Desugar at AST->HIR** into existing HIR primitives
3. **Add a small new HIR node** only if semantics can't be expressed cleanly
4. Mirror into MIR in the smallest way:
   - Pure -> `Compute(dest, Rvalue{...})`
   - Effectful -> `Effect(...)`
   - Control-flow -> terminators

**Heuristic:** prefer _structural encoding_ (node kinds / fields) over "flags buried in metadata".

### Step 4: Define Typing + Coercions + Constant-Folding Rules

Write down:

- Result type computation
- Implicit conversions / sign rules / sizing rules
- Whether it participates in constant folding
- What happens in elaboration contexts (params, localparams, genvars, etc.)

Then implement in the right phase (usually AST->HIR typing or HIR validation).

### Step 5: Define Error Behavior and Diagnostics

For each invalid program form:

- Which phase reports it? (AST->HIR, HIR validation, HIR->MIR, interpreter runtime)
- Is it a hard error, warning, or runtime failure?
- What message format / error code conventions?

**Rule:** prefer catching invalid constructs _before MIR_, unless it's inherently runtime-only.

### Step 6: Implement End-to-End

Pipeline checklist:

- [ ] AST->HIR: parse + desugar + type
- [ ] HIR validation: reject invalid forms
- [ ] HIR->MIR: encode semantics in MIR construct(s)
- [ ] MIR interpreter: implement behavior (the semantic oracle)
- [ ] Tests: add SV tests + expected outputs
- [ ] (Optional) LLVM backend: add lowering or mark unsupported with a clear error

## Testing Methodology (TDD by Default)

Write tests first most of the time.

### Test ladder (fast -> slow)

1. **Unit tests for helpers** (type computation, classifier, formatting parser, etc.)
2. **MIR interpreter semantic tests** (primary oracle)
3. **End-to-end SV feature tests** in `tests/sv_features/...`
4. (Optional) **Cross-backend parity tests** - same SV test should pass on MIR interpreter and LLVM backend, or LLVM backend explicitly reports "unsupported feature" with a deterministic error

**Rule:** if you can't test it in the interpreter, you don't actually know the semantics are right.

## Scope Triage

Before coding, classify the feature's surface area:

| Surface area              | Complexity | What to read first                         |
| ------------------------- | ---------- | ------------------------------------------ |
| Expression-only           | Easiest    | HIR node + MIR rvalue + interp op          |
| Data type / storage       | Medium     | TypeArena + layout + load/store            |
| System functions/tasks    | Medium     | Registry + argument rules + runtime        |
| Control-flow / scheduling | Hard       | Terminators, coroutine/suspend, time wheel |

This tells you whether you need to read layout/interpreter internals up front.
