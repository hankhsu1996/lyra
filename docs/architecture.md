# Architecture

## Tool Structure

LYRA is the platform. The primary user-facing binary is:

```
lyra <subcommand>
```

Examples: `lyra run`, `lyra check`, `lyra dump hir`

Externally: one binary for usability.
Internally: compiler logic is a library, orchestrator handles config, caching, and invocation.

## Compilation Pipeline

```
AST (slang) -> HIR -> MIR -> LLVM IR -> executable
```

### Slang (Frontend)

- Parses SystemVerilog, produces AST
- Performs legality checks, name resolution, type checking
- Does NOT emit fully elaborated netlist (see Runtime Elaboration below)

### HIR (Language Semantic Layer)

- Decouples from slang AST
- Preserves SystemVerilog semantics faithfully
- Owns all data (strings, types) - no lifetime dependency on slang
- Represents module templates, not elaborated instances
- Primary boundary between slang and the rest of the compiler

### MIR (Executable Semantic Layer)

- Place/Value model (strict separation: Place for writes, Value for computation)
- Control flow graphs with basic blocks and terminators
- Suspension (Delay/Wait) as terminators, not statements
- Target-independent; fixes all execution semantics
- Primary input for LLVM backend and interpreter

### LLVM IR (Machine Layer)

- SSA form, three-address code
- Runtime library calls for simulation primitives
- Primary compilation path for performance

## Elaboration Model

Slang performs full elaboration at compile time:

- Slang validates semantics and elaborates hierarchy
- HIR/MIR represent the elaborated design
- Each instance has resolved parameters and types

## Project Structure

```
include/lyra/
  common/        # shared types, utilities, diagnostics
  hir/           # language semantic IR (decoupled from slang)
  mir/           # executable semantic IR (Place/Value, basic blocks)
  llvm_backend/  # MIR -> LLVM IR
  runtime/       # simulation runtime (scheduler, signals)
  lowering/      # AST->HIR, HIR->MIR lowering
  semantic/      # semantic utilities
```

### Key Components

- **Diagnostic** (`common/diagnostic.hpp`): Error reporting with source locations. Uses `std::expected<T, Diagnostic>` (aliased as `Result<T>`) for error propagation. Produces colorful terminal output with file:line:col and source context.

### MIR Interpreter

The MIR interpreter provides a reference implementation for debugging:

```
MIR -> Interpreter
```

Useful for validating MIR semantics without LLVM compilation overhead.

## Data Flow

1. Source files -> `SlangFrontend` -> AST
2. AST -> `AstToHir` -> HIR (owns all data, slang can be released)
3. HIR -> `HirToMir` -> MIR (executable semantics)
4. MIR -> `MirToLlvm` -> LLVM IR -> executable

Each stage is independent and testable. The HIR stage creates a clean boundary where slang resources can be released.

## Slang Data Ownership

Slang's `SourceManager` owns the memory backing source text, and many slang types (including `symbol->name`) return `string_view` into this memory.

**Solution**: HIR owns all data. At the slang -> HIR boundary (`AstToHir`), we copy all needed information into our own types with owned `std::string` names. This:

- Eliminates hidden lifetime dependencies on slang
- Makes the slang boundary explicit and complete
- Allows slang resources to be released after HIR construction
