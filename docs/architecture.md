# Architecture

## Tool Structure

LYRA is the platform. The primary user-facing binary is:

```
lyra <subcommand>
```

Examples: `lyra run`, `lyra check`, `lyra dump hir`

Externally: one binary for usability.
Internally: compiler logic is a library, orchestrator handles config, caching, and invocation.

## Compilation Model

Lyra uses specialization-based compilation. The compilation unit is a **module specialization** (`ModuleSpecId`), not an elaborated design. See [compilation-model.md](compilation-model.md) for the full data model.

```
SV -> slang -> Elaboration Discovery
                    |
         Specialization Compilation  (parallel per spec)
           AST -> HIR -> MIR -> LLVM
                    |
         Design Realization  (design-wide)
                    |
         Execution
```

### Elaboration Discovery

- slang parses SystemVerilog, produces AST
- Performs legality checks, name resolution, type checking
- Determines the instance graph, connectivity, and required specializations
- Does NOT produce compiled artifacts

### Specialization Compilation (Parallel)

Each specialization is compiled independently:

- **HIR**: Decouples from slang AST. Preserves SystemVerilog semantics. Specialization-scoped.
- **MIR**: Place/Value model with basic blocks. Fixes all execution semantics. Specialization-scoped.
- **LLVM IR**: Per-specialization LLVM module. Runtime library calls for simulation primitives.

HIR and MIR are internal to specialization compilation. They contain no instance paths, no design-global slot IDs, and no design-global allocation. See [hir-design.md](hir-design.md) and [mir-design.md](mir-design.md).

### Design Realization

**Target:** constructs the runtime object graph from compiled specializations + elaborated design topology:

- Instance construction (per-instance storage from body-shaped SpecLayout)
- Connectivity wiring (port bindings, connection descriptors)
- Per-instance constant blocks (value-only parameters)
- Debug tables (instance paths for `%m`)

**Current state:** realization produces a design-global arena with per-instance slot ranges, not per-instance objects. Constructor output dissolves instance identity into flat process frames and design-global metadata tables. Per-instance emitted IR (path strings, param payloads) still exists in the compiled object. See the [specialization queue](queues/specialization.md) for the remaining migration.

Realization does not recompile specialization code. See [compilation-model.md](compilation-model.md).

### Execution

**Target:** event-driven simulation engine. Processes run as behavior on instance objects with object-local state access. See [runtime.md](runtime.md).

**Current state:** shared-body processes already access owned-local slots via `this_base + offset`, which is close to the target. However, forwarded slots use design-global addressing, signal identity requires design-global renumbering, dirty tracking uses design-global slot IDs, and every process has access to the full design arena via `design_ptr`.

## Project Structure

```
include/lyra/
  common/        # shared types, utilities, diagnostics
  hir/           # language semantic IR (specialization-scoped)
  mir/           # executable semantic IR (specialization-scoped)
  llvm_backend/  # MIR -> LLVM IR (per-specialization)
  realization/   # design realization (bindings, metadata, main emission)
  runtime/       # simulation runtime (scheduler, signals)
  lowering/      # AST->HIR, HIR->MIR lowering
  semantic/      # semantic utilities
```

Headers in `include/lyra/`, implementations in `src/lyra/`.

### Key Components

- **Diagnostic** (`common/diagnostic.hpp`): Error reporting with source locations. Uses `std::expected<T, Diagnostic>` (aliased as `Result<T>`) for error propagation. Produces colorful terminal output with file:line:col and source context.

## Data Flow

1. Source files -> `SlangFrontend` -> AST
2. AST -> `AstToHir` -> HIR (owns all data, slang can be released)
3. HIR -> `HirToMir` -> MIR (executable semantics)
4. MIR -> `MirToLlvm` -> LLVM IR -> executable (per specialization)

Each stage is independent and testable. The HIR stage creates a clean boundary where slang resources can be released.

## Slang Data Ownership

Slang's `SourceManager` owns the memory backing source text, and many slang types (including `symbol->name`) return `string_view` into this memory.

**Solution**: HIR owns all data. At the slang -> HIR boundary (`AstToHir`), we copy all needed information into our own types with owned `std::string` names. This:

- Eliminates hidden lifetime dependencies on slang
- Makes the slang boundary explicit and complete
- Allows slang resources to be released after HIR construction
