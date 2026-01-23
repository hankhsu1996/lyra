---
name: explore-llvm
description: Plan how to lower a MIR construct to LLVM IR. Use when adding new lowering or fixing existing lowering.
---

# Plan LLVM Lowering

Guide for planning MIR -> LLVM IR lowering for a specific construct.

## Prerequisites

Read before starting:

- `docs/llvm-backend.md` - lowering principles and decision framework
- `docs/mir-design.md` - MIR semantics (what you're translating)
- `docs/pipeline-contract.md` - layer boundaries

## Mental Model

> MIR is the semantic endpoint. LLVM is only an execution substrate.
> Lowering is not interpretation--it is faithful translation.

Key distinction: semantics are already fixed in MIR. This skill is about mechanical translation, not semantic design. If lowering needs to "decide" behavior, the MIR is underspecified--fix MIR first.

## Process

### Step 1: Identify the MIR Construct

What are you lowering?

| MIR Category | Examples                    |
| ------------ | --------------------------- |
| Rvalue       | Binary, Unary, Cast, Call   |
| Instruction  | Assign, Compute, Effect     |
| Terminator   | Jump, Branch, Delay, Return |

**Actions:**

1. Find the definition in `include/lyra/mir/`
2. Understand what semantic information it carries
3. Check if similar constructs already have lowering (use as reference)

### Step 2: Apply the Decision Framework

Answer four questions:

| Question                      | If No                                          |
| ----------------------------- | ---------------------------------------------- |
| Is behavior fully defined?    | Fix MIR first--do not decide semantics here    |
| Does it produce a value?      | Lower as call/store/branch (not SSA value)     |
| Is it pure?                   | Likely needs runtime call or memory operations |
| Which lowering class applies? | See three-way classification below             |

**Three-way lowering classification:**

| Class            | When                                          | Example                   |
| ---------------- | --------------------------------------------- | ------------------------- |
| Native LLVM op   | One instruction or near-1:1 mapping           | `add`, `icmp`, `br`       |
| Pattern lowering | Fixed recipe of LLVM ops (shifts, masks, RMW) | bit-range extraction      |
| Runtime ABI call | Needs runtime state or complex data           | `$display`, delay/suspend |

**Decision tree example:**

```
MIR Rvalue::Binary(Add, lhs, rhs)
  Q1: Fully defined? -> Yes (operand widths, signedness in MIR)
  Q2: Value? -> Yes (produces result)
  Q3: Pure? -> Yes (no side effects)
  Q4: Class? -> Native LLVM op (`add`)
```

### Step 3: Determine LLVM Shape

Based on Step 2, define the exact LLVM IR.

**For native operations:**

- Which LLVM instruction? (`add`, `icmp`, `br`, etc.)
- What types? (derived from TypeId via type lowering contract)
- Signedness considerations? (`lshr` vs `ashr`)

**For pattern lowering:**

- What sequence of LLVM instructions?
- Are intermediate values needed?
- Is the pattern fixed or parameterized by width/type?

**For runtime calls:**

- What function signature? (follow runtime ABI conventions below)
- What arguments? (operands, width, type info)
- Does the runtime function exist or need to be added?

### Step 4: Apply Backend Policies

These are **policy decisions**, not universal truths. They can evolve as the backend matures.

**Width policy:**

The backend chooses how to represent integers of various widths. Current policy:

- Width <= threshold: native LLVM `iN` types
- Width > threshold: pointer to storage + runtime ABI calls

The threshold is a tuning knob (compile-time vs optimization tradeoffs). Legalization ensures MIR satisfies whichever policy is active.

**4-state policy:**

The LLVM backend currently operates in **2-state mode**. This means:

- 4-state values must be canonicalized to 2-state before reaching lowering
- OR rejected with a diagnostic
- Do not mix modes ad-hoc per operation

**Metadata in MIR:**

Metadata exists only to make lowering mechanical and deterministic:

- Signedness where ambiguous
- Comparison predicate kind
- Policy decisions that must not be inferred at lowering time

If information is not needed to prevent ambiguity, it should not be in MIR metadata.

### Step 5: Type Lowering Contract

For each TypeId category, the backend must specify:

- LLVM storage type
- Value vs pointer representation
- Load/store behavior
- Pass-by-value vs pass-by-pointer
- Aggregate representation (struct vs opaque pointer + runtime)

This mapping must be deterministic: same TypeId -> same LLVM type, always.

### Step 6: Verify Invariants

Check these before proceeding:

- [ ] One MIR construct -> one LLVM shape (no conditionals in lowering logic)
- [ ] No semantic interpretation (MIR already decided everything)
- [ ] Deterministic structure (same MIR + same policy -> same CFG shape, instruction pattern, runtime calls)
- [ ] Layout derived from TypeId (never inferred from context)

**Red flags:**

- "If this is a comparison, we need to check..." -> MIR should distinguish
- "Depending on the context..." -> context should be in MIR
- "For this special case..." -> likely wrong abstraction
- "We need to decide whether..." -> behavior is underspecified in MIR

### Step 7: Implementation Plan

**Files to modify:**

| File                          | Purpose                 |
| ----------------------------- | ----------------------- |
| `src/lyra/llvm_backend/*.cpp` | Lowering logic          |
| `include/lyra/runtime/*.hpp`  | Runtime API (if needed) |
| `src/lyra/runtime/*.cpp`      | Runtime implementation  |

**Runtime ABI conventions** (if adding runtime calls):

- Naming: `Lyra<Operation>` (e.g., `LyraAddWide`, `LyraDisplay`)
- Parameters: destination pointer first, then operands, then metadata (width, flags)
- Return: void for in-place ops, value for simple results
- Width/type info: passed as integer parameters, not inferred

**Test strategy:**

- MIR interpreter is the semantic oracle
- Same test input, different backend -> results must match
- Add test cases in `tests/sv_features/`
- Pattern-based IR checks (verify key instruction sequences, not full IR equality)

## Deliverable

A brief specification covering:

1. **MIR construct**: What you're lowering
2. **Decision answers**: Defined/Value/Pure/Class
3. **LLVM shape**: Exact IR pattern or runtime call
4. **Policy application**: How width/4-state policies affect the shape
5. **Type mapping**: How TypeIds become LLVM types for this construct
6. **Files to modify**: Implementation locations
7. **Test plan**: How to verify correctness

## Template

```markdown
## Lowering: <MIR Construct Name>

### MIR Definition

- Location: `include/lyra/mir/<file>.hpp`
- Semantic: <what it represents>

### Decision Framework

- Fully defined? <yes/no> -> <if no, what's missing>
- Value? <yes/no> -> <implication>
- Pure? <yes/no> -> <implication>
- Class: <native / pattern / runtime>

### LLVM Shape

<LLVM IR snippet or runtime call signature>

### Policy Considerations

- Width: <how width policy affects this>
- 4-state: <how state policy affects this>

### Implementation

- Lowering: `src/lyra/llvm_backend/`
- Runtime: <if needed>

### Tests

- <test file or approach>
```

## Common Patterns

### Binary Operations

```
Native (within width policy):
  %result = <op> <type> %lhs, %rhs

Runtime (exceeds width policy):
  call void @Lyra<Op>(ptr %dst, ptr %lhs, ptr %rhs, i32 %width)
```

### Comparisons

```
Native:
  %cmp = icmp <pred> <type> %lhs, %rhs
  ; pred determined by MIR metadata (signed vs unsigned)
```

### Terminators

```
Jump:
  br label %target

Branch:
  br i1 %cond, label %then, label %else
  ; cond MUST be 2-state i1

Delay:
  call void @LyraDelay(i64 %time)
  ; followed by coroutine suspend pattern
```
