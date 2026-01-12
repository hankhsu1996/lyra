---
name: explore-feature
description: Explore how to implement a new SystemVerilog feature in the Lyra compiler. Use when investigating what changes are needed across the compilation pipeline.
---

# Explore Feature Implementation

Guide for investigating how to support a new SystemVerilog feature in Lyra.

## Prerequisites

### 1. Read Project Documentation First

Before doing anything, familiarize yourself with:

- `CLAUDE.md` - Project commands, architecture overview, code style
- `CLAUDE.local.md` - Personal workflow preferences (if present, not committed)
- `docs/architecture.md` - Component relationships, data flow
- `docs/design-principles.md` - Implementation guidelines

### 2. Require LRM Documentation

**STOP and ask the user** if they haven't provided:

- IEEE 1800 LRM section (PDF link or text)
- Or other official specification

Do NOT proceed without official documentation for the feature.

## Mental Model

```
Agents A-C: Discover what the system must be
Agent D:    Question whether that shape is healthy
Agent E:    Turn the final shape into safe, incremental action
```

## Agent Structure

### Agent A: LRM Semantic Contract

This agent does not summarize the LRM. Its responsibility is to extract a compiler-level semantic contract from the spec: execution phase, guarantees, observables, and implementation freedom. The output answers: what must be true at runtime, independent of AST, IR, or codegen. If this agent cannot turn prose into enforceable obligations, it has failed.

### Agent B: Frontend / AST Grounding

This agent anchors the feature in reality. It analyzes how the feature is represented in the frontend (Slang AST), what information already exists, what is implicit, and where the frontend stops. Its role is to prevent downstream layers from reinventing parsing, scoping, or typing logic.

### Agent C0: Runtime / C++ Abstraction Designer

This agent defines the high-level execution model and must run before any MIR/LIR design. Given the semantic contract, it answers: If this were implemented directly in C++ runtime code, what would it look like? Its output is a clean runtime abstraction (callbacks, closures, function pointers, registration, invocation timing). No IR is discussed here—only how execution naturally works.

### Agent C: IR Projection (MIR/LIR)

This agent projects the runtime abstraction into the compiler's IR world. It determines how the C++-level primitives from C0 map onto existing MIR/LIR constructs, or what minimal primitive is missing.

**LIR Design Mental Model:**

LIR uses named temporaries (`%t0`, `%t1`) in a register-based form that maps to LLVM IR's SSA.

Key distinction - two elaboration models:

| Path                    | Elaboration                    | Variable Access                           |
| ----------------------- | ------------------------------ | ----------------------------------------- |
| C++ codegen             | Runtime (C++ builds hierarchy) | `this->u_child_.value` - member traversal |
| MIR → LIR → Interpreter | Compile-time (slang resolves)  | `$symbol` - flat lookup by unique pointer |

**When to use which mental model:**

| Aspect                                | Mental Model                          | Example                                   |
| ------------------------------------- | ------------------------------------- | ----------------------------------------- |
| Variable access                       | Slang's flat model (symbol = address) | `load %t0, $symbol`                       |
| Operations (arithmetic, control flow) | RISC-V assembly style                 | `add %t2, %t0, %t1`                       |
| Method calls on complex types         | RISC-V function call style            | `arr.size()` → `call` with object pointer |

For **arrays, queues, and future classes**: think about C++ method dispatch compiled to RISC-V - object pointer as implicit `this`, function call for methods.

### Agent D: Challenge / Structural Critic

Agent D is explicitly responsible for detecting refactor triggers.

Its duties include:

- Identifying structural stress (switch explosion, feature-specific logic leakage, missing abstraction)
- Deciding whether the new feature fits existing structure, or reveals a required refactor
- Classifying any refactor as **mandatory** (feature should not proceed without it) or **optional** (can be deferred)

Agent D must clearly state one of:

- "This change requires refactor before feature work", or
- "This change does not require refactor."

### Agent E: Plan / Orchestrator

Agent E produces the implementation plan section of `<feature>.local.md`. It must enforce ordering, not just record suggestions.

If Agent D flags a refactor:

- Default plan = **Refactor first, Feature second**
- Explicitly forbid mixing refactor and feature unless refactor is mechanical and low-risk
- Split the work into:
  - Refactor plan (behavior-preserving)
  - Feature plan (semantic change)

The plan must justify any deviation from this order.

The plan specifies:

- Which stages are design-only vs runnable
- Where testing is meaningful (codegen first, full dual-path later)
- File-level scope per stage (small batches)

It does not implement code. Its job is to make execution safe, incremental, and restartable.

## Overall Flow

```
Semantic truth → frontend reality → runtime abstraction → IR projection → structural challenge → executable plan
```

## Sequencing

```
Phase 1 (parallel):   A (semantic contract) + B (AST grounding)
Phase 2 (sequential): C0 (runtime design) - needs A's semantic contract
Phase 3 (sequential): C (IR projection) - needs C0's runtime abstraction
Phase 4:              D (structural critique) - needs emerging design
Phase 5:              E (planning) - completes <feature>.local.md
```

## Experimentation

Use a local folder (e.g., `playground/`) for ad-hoc testing. Recommended setup:

```
playground/
├── test.sv        # Test source file
├── lyra.toml      # Lyra project config (top = "Test")
└── out/           # Lyra emit output
```

**Useful commands:**

```bash
# Build and run with Lyra
./bazel-bin/lyra -C playground run
./bazel-bin/lyra -C playground run --interpret

# Dump IR for inspection
./bazel-bin/lyra dump mir playground/test.sv
./bazel-bin/lyra dump lir playground/test.sv

# Compare with Verilator (reference implementation)
verilator --binary -o playground/vtest playground/test.sv
./playground/vtest

# Dump Slang AST (if jq available)
slang --ast-json playground/ast.json playground/test.sv
```

## Deliverable

One working document per feature: `<feature>.local.md` (gitignored).

### Document Structure

**1. Goal and Scope**

- What feature you are adding (one sentence)
- What is explicitly out of scope
- Acceptance criteria (what "done" means)

**2. Semantic Contract** (Agent A)

- Runtime-observable contract, not spec summary
- Phases (elaboration vs simulation), guarantees, observables
- Ambiguities / implementation freedom chosen

**3. Frontend Facts** (Agent B)

- Only what matters for implementation
- Key AST nodes, info you get "for free", edge cases

**4. Runtime Abstraction** (Agent C0)

- High-level C++ execution model
- Runtime primitives (registration, invocation, scheduling)
- What must be shared between Interpreter and Codegen
- No MIR/LIR discussion here

**5. IR Projection** (Agent C)

- How runtime abstraction maps to MIR/LIR
- New primitive needed? If so, why minimal
- How both paths (Interpreter, Codegen) stay consistent

**6. Refactor Decision** (Agent D)

- Explicit and binary: required or not
- If required: refactor-first ordering, justify any deviation

**7. Execution Stages** (Agent E)

Natural pipeline boundaries:

| Stage | Work                             | Testable?                      |
| ----- | -------------------------------- | ------------------------------ |
| 1     | MIR structure + AST→MIR lowering | Structural (dumps, invariants) |
| 2     | Codegen from MIR                 | Yes (codegen-only tests)       |
| 3     | LIR + MIR→LIR lowering           | Structural                     |
| 4     | Interpreter                      | Yes (full dual-path)           |

Per stage, specify:

- Purpose: what this stage proves
- Changes: files touched (2-3 files per batch)
- Exit criteria: what must be true to proceed

**8. Testing Strategy**

- Don't force tests every stage
- Structural checks (dumps, invariants) for early stages
- First runnable tests at codegen
- Final correctness: dual-path equivalence (Interpreter vs Codegen)

**9. Risk Notes**

- Likely failure modes
- Parts that may trigger redesign
- "Do not do X" guardrails

## Lyra Pipeline Reference

```
SystemVerilog -> Slang AST -> MIR -> LIR -> Interpreter
                                |
                             C++ Codegen
```

### Key Directories

| Layer       | Location                        |
| ----------- | ------------------------------- |
| MIR types   | `include/lyra/mir/`             |
| AST->MIR    | `src/lyra/lowering/ast_to_mir/` |
| MIR->LIR    | `src/lyra/lowering/mir_to_lir/` |
| Codegen     | `src/lyra/compiler/codegen.cpp` |
| Interpreter | `src/lyra/interpreter/`         |
| SDK         | `include/lyra/sdk/`             |
| Tests       | `tests/sv_features/`            |
