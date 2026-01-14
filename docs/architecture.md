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
AST (slang) → HIR → MIR → LLVM IR → executable
               ↘
                C++ (secondary exploration path)
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

- Rust-style Place/Operand/Rvalue model
- Explicit memory semantics
- Control flow graphs with basic blocks
- Target-independent executable representation
- Primary input for LLVM backend

### LLVM IR (Machine Layer)

- SSA form, three-address code
- Runtime library calls for simulation primitives
- Primary compilation path for performance

### C++ Codegen (Secondary Path)

C++ generation is a secondary exploration path:

- Produces readable output for debugging and understanding
- Links against SDK for simulation runtime
- Useful for rapid prototyping before LLVM path is complete

### Embedded SDK

The `lyra emit` command produces a standalone C++ project that includes SDK headers. To enable the binary to work from any directory without installation:

```
include/lyra/sdk/*.hpp  ->  [genrule]  ->  embedded_sdk.hpp  ->  lyra binary
```

- SDK headers are maintained as normal `.hpp` files in `include/lyra/sdk/`
- At build time, a Bazel genrule generates `embedded_sdk.hpp` with file contents as C++ raw string literals
- The lyra binary embeds this generated header
- `lyra emit` writes embedded SDK content to the output directory

This approach keeps SDK headers maintainable while producing a self-contained binary.

## Runtime Elaboration Model

Key insight: hierarchy is constructed at runtime, not compile-time.

Traditional flow (Verilator-style):

- Full elaboration at compile time
- Entire design flattened
- Static instance graph

LYRA flow:

- Slang validates semantics
- HIR/MIR represent module templates
- Generated code constructs hierarchy at runtime
- Parameters become constructor arguments
- `generate for/if` becomes constructor logic

This trades some runtime initialization cost for faster compilation and better debuggability.

## Project Structure

```
include/lyra/
  common/       # shared types, utilities, diagnostics
  frontend/     # Slang wrapper, produces AST
  hir/          # language semantic IR (decoupled from slang)
  mir/          # executable semantic IR (Place/Operand/Rvalue)
  codegen/      # HIR → C++ generator (secondary path)
  llvm/         # MIR → LLVM IR (primary path)
  sdk/          # runtime library (Task, Scheduler, Signal)
  cli/          # lyra subcommands (build, run, emit)
```

### Key Components

- **Diagnostic** (`common/diagnostic.hpp`): Error reporting with source locations. Uses `std::expected<T, Diagnostic>` (aliased as `Result<T>`) for error propagation. Produces colorful terminal output with file:line:col and source context.

### Interpreter (Future)

An interpreter path may be added in the future for development and debugging:

```
MIR → Interpreter
```

This would provide reference semantics validation without compilation overhead.

## Data Flow

1. Source files -> `SlangFrontend` -> AST
2. AST -> `AstToHir` -> HIR (owns all data, slang can be released)
3. HIR -> `HirToMir` -> MIR (executable semantics)
4. MIR -> `MirToLlvm` -> LLVM IR -> executable (primary path)
5. HIR -> `Codegen` -> C++ source files (secondary path)

Each stage is independent and testable. The HIR stage creates a clean boundary where slang resources can be released.

## Slang Data Ownership

Slang's `SourceManager` owns the memory backing source text, and many slang types (including `symbol->name`) return `string_view` into this memory.

**Solution**: HIR owns all data. At the slang -> HIR boundary (`AstToHir`), we copy all needed information into our own types with owned `std::string` names. This:

- Eliminates hidden lifetime dependencies on slang
- Makes the slang boundary explicit and complete
- Allows slang resources to be released after HIR construction
