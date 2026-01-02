# Architecture

## Tool Structure

LYRA is the platform. The primary user-facing binary is:

```
lyra <subcommand>
```

Examples: `lyra build`, `lyra run`, `lyra emit cpp`, `lyra check`

Externally: one binary for usability.
Internally: compiler logic is a library, orchestrator handles config, caching, and invocation.

## Compilation Pipeline

```
SystemVerilog -> Slang -> MIR -> LIR -> Backend
```

### Slang (Frontend)

- Parses SystemVerilog, produces AST
- Performs legality checks, name resolution, type checking
- Does NOT emit fully elaborated netlist (see Runtime Elaboration below)

### MIR (Middle IR)

- Preserves high-level structure: statements, expressions, control flow
- Represents module templates, not elaborated instances
- Encodes parameter interfaces

### LIR (Low-level IR)

- SSA-style with basic blocks and linear instructions
- Scheduling semantics (comb/ff/NBA)
- Backend-agnostic representation

### Backend

Current: Interpreter (for development/testing)
Target: C++ code generation

## Runtime Elaboration Model

Key insight: hierarchy is constructed at runtime, not compile-time.

Traditional flow (Verilator-style):
- Full elaboration at compile time
- Entire design flattened
- Static instance graph

LYRA flow:
- Slang validates semantics
- MIR/LIR represent module templates
- Generated C++ constructs hierarchy at runtime
- Parameters become constructor arguments
- `generate for/if` becomes constructor logic

This trades some runtime initialization cost for faster compilation and better debuggability.

## Component Responsibilities

| Component | Responsibility |
|-----------|----------------|
| `frontend/` | Slang wrapper, produces AST |
| `mir/` | Module template IR, high-level semantics |
| `lir/` | Linear IR, scheduling, backend input |
| `lowering/` | AST->MIR, MIR->LIR transformations |
| `interpreter/` | Development backend, event-driven execution |
| `driver/` | Orchestration, pipeline coordination |
| `common/` | Shared types, utilities |

## Data Flow

1. Source files -> `SlangFrontend` -> AST
2. AST -> `AstToMir` -> MIR Module (template)
3. MIR -> `MirToLir` -> LIR Module (linearized)
4. LIR -> Backend (interpreter or C++ codegen)

Each stage is independent and testable.
