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
SystemVerilog -> Slang -> MIR -> C++ codegen -> compile -> run
```

### Slang (Frontend)

- Parses SystemVerilog, produces AST
- Performs legality checks, name resolution, type checking
- Does NOT emit fully elaborated netlist (see Runtime Elaboration below)

### MIR (Middle IR)

- Preserves high-level structure: statements, expressions, control flow
- Represents module templates, not elaborated instances
- Encodes parameter interfaces
- Primary input for C++ code generation

### C++ Codegen

MIR maps directly to C++ because:

- Control flow (if/while/do-while) maps 1:1
- Avoids reconstructing structure from linearized form
- Produces readable output (a core requirement)

Generated code links against SDK for simulation runtime.

## Runtime Elaboration Model

Key insight: hierarchy is constructed at runtime, not compile-time.

Traditional flow (Verilator-style):

- Full elaboration at compile time
- Entire design flattened
- Static instance graph

LYRA flow:

- Slang validates semantics
- MIR represents module templates
- Generated C++ constructs hierarchy at runtime
- Parameters become constructor arguments
- `generate for/if` becomes constructor logic

This trades some runtime initialization cost for faster compilation and better debuggability.

## Project Structure

```
include/lyra/
  common/       # shared types, utilities
  frontend/     # Slang wrapper, produces AST
  mir/          # module template IR
  codegen/      # MIR -> C++ generator
  sdk/          # runtime library (Task, Scheduler, Signal)
  cli/          # lyra subcommands (build, run, emit)
```

Deprecated (legacy interpreter path):

- `lir/` - linearized IR, not needed for C++ backend
- `lowering/mir_to_lir/` - LIR transformation
- `interpreter/` - replaced by generated C++ + SDK

## Data Flow

1. Source files -> `SlangFrontend` -> AST
2. AST -> `AstToMir` -> MIR Module (template)
3. MIR -> `Codegen` -> C++ source files
4. C++ compiler -> executable (linked with SDK)
5. Run executable

Each stage is independent and testable.
