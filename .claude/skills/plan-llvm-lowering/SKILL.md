---
name: plan-llvm-lowering
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

Key difference from `explore-feature`: semantics are already fixed in MIR. This skill is about mechanical translation, not semantic design.

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

Answer three questions:

| Question                 | Answer                  | Implication             |
| ------------------------ | ----------------------- | ----------------------- |
| Does it produce a value? | Yes -> SSA value        | No -> call/store/branch |
| Is it pure?              | Yes -> LLVM instruction | No -> runtime call      |
| Is it SV-specific?       | Yes -> `Lyra*`          | No -> native LLVM op    |

**Decision tree example:**

```
MIR Rvalue::Binary(Add, lhs, rhs)
  Q1: Value? -> Yes (produces result)
  Q2: Pure?  -> Yes (no side effects)
  Q3: SV-specific? -> No (addition is universal)
  -> Native LLVM: `add`

  But: Width > 128?
  -> Yes: runtime call `LyraAddWide`
  -> No:  native `add i64 %lhs, %rhs`
```

### Step 3: Determine LLVM Shape

Based on Step 2, define the exact LLVM IR:

**For native operations:**

- Which LLVM instruction? (`add`, `icmp`, `br`, etc.)
- What types? (derived from TypeId)
- Signedness considerations? (`lshr` vs `ashr`)

**For runtime calls:**

- What function signature?
- What arguments? (operands, width, type info)
- Does it exist or need to be added?

**Width considerations:**

| Width       | Strategy                          |
| ----------- | --------------------------------- |
| <= 128 bits | Native LLVM integers              |
| > 128 bits  | Pointer to storage + runtime call |

**4-state considerations:**

| State   | Strategy                       |
| ------- | ------------------------------ |
| 2-state | Native operations              |
| 4-state | Runtime call (propagate masks) |

### Step 4: Verify Invariants

Check these before proceeding:

- [ ] One MIR construct -> one LLVM shape (no conditionals in lowering logic)
- [ ] No semantic interpretation (MIR already decided everything)
- [ ] Deterministic output (same MIR -> same LLVM IR, byte-for-byte)
- [ ] Layout derived from TypeId (never inferred)

**Red flags:**

- "If this is a comparison, we need to check..." -> MIR should distinguish
- "Depending on the context..." -> context should be in MIR
- "For this special case..." -> likely wrong abstraction

### Step 5: Implementation Plan

**Files to modify:**

| File                          | Purpose                 |
| ----------------------------- | ----------------------- |
| `src/lyra/llvm_backend/*.cpp` | Lowering logic          |
| `include/lyra/runtime/*.hpp`  | Runtime API (if needed) |
| `src/lyra/runtime/*.cpp`      | Runtime implementation  |

**Test strategy:**

- MIR interpreter is the semantic oracle
- Same test input, different result -> LLVM lowering bug
- Add test cases in `tests/sv_features/`

## Deliverable

A brief specification covering:

1. **MIR construct**: What you're lowering
2. **Decision answers**: Value/Pure/SV-specific
3. **LLVM shape**: Exact IR or runtime call
4. **Width/4-state handling**: How these affect the shape
5. **Files to modify**: Implementation locations
6. **Test plan**: How to verify correctness

## Template

```markdown
## Lowering: <MIR Construct Name>

### MIR Definition

- Location: `include/lyra/mir/<file>.hpp`
- Semantic: <what it represents>

### Decision Framework

- Value? <yes/no> -> <implication>
- Pure? <yes/no> -> <implication>
- SV-specific? <yes/no> -> <implication>

### LLVM Shape

**Width <= 128, 2-state:**
<LLVM IR snippet>

**Width > 128 or 4-state:**
<runtime call signature>

### Implementation

- Lowering: `src/lyra/llvm_backend/`
- Runtime: <if needed>

### Tests

- <test file or approach>
```

## Common Patterns

### Binary Operations

Most binary ops follow:

```
2-state, width <= 128:
  %result = <op> <type> %lhs, %rhs

4-state or width > 128:
  call void @Lyra<Op>(ptr %dst, ptr %lhs, ptr %rhs, i32 %width)
```

### Comparisons

```
2-state:
  %cmp = icmp <pred> <type> %lhs, %rhs

4-state:
  %result = call i8 @LyraCompare4State(ptr %lhs, ptr %rhs, i32 %op)
  ; result encodes: 0=false, 1=true, 2=unknown
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
  ; followed by coroutine suspend
```
