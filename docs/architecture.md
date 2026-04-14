# Architecture

> Before editing, see [documentation-guidelines.md](documentation-guidelines.md). Architecture docs describe the target, not history. No "current state," migration plans, or queue references.

## Tool Structure

LYRA is the platform. The primary user-facing binary is:

```
lyra <subcommand>
```

Examples: `lyra run`, `lyra check`, `lyra dump hir`

Externally: one binary for usability.
Internally: compiler logic is a library, orchestrator handles config, caching, and invocation.

## Compilation Model

Lyra uses specialization-based compilation. The compilation unit is a **module specialization** (`ModuleSpecId`), not an elaborated design or an instance. See [compilation-model.md](compilation-model.md) for the full data model.

```
SV -> slang -> Elaboration Discovery
                    |
         Specialization Grouping
           (frontend world -> per-unit compiler world)
                    |
         Per-Unit Compilation  (parallel per specialization)
           HIR -> XIR -> MIR -> LLVM
                    |
         Artifact Composition  (compile-time, design-wide)
                    |
         Constructor / Realization  (runtime, per-instance)
                    |
         Execution
```

### Elaboration Discovery

The frontend library (slang) parses, elaborates, and type-checks the full design. This produces the frontend's view of the world: an instance graph, connectivity, and parameter assignments.

Elaboration output is **input to the compiler**, not the compiler's own compilation world. The compiler does not adopt the frontend's instance-centric model.

### Specialization Grouping

At the AST-to-HIR boundary, the compiler breaks the frontend's whole-design output into per-specialization compilation units. Instances with identical compile-owned facts (packed layout, compiled code shape) share a single compilation unit. The compiler's world after this point is per-unit, not per-design.

### Per-Unit Compilation (Parallel)

Each compilation unit is compiled independently through four IR layers:

- **HIR**: Semantic/source-oriented IR. Decouples from slang AST. Owns all data. See [hir-design.md](hir-design.md).
- **XIR**: Execution-model IR. Makes executable structure explicit (closures, observers, wait models, structured control flow, object composition). XIR exists because execution-model shape must be reviewable -- it is projectable into a C++ object/behavior model as a methodology tool. See [xir-design.md](xir-design.md).
- **MIR**: Control-flow/plumbing IR. Lowers execution structure to basic blocks, terminators, Place/Operand. See [mir-design.md](mir-design.md).
- **LLVM IR**: Per-specialization LLVM module. Runtime library calls for simulation primitives.

All IRs are per-compilation-unit. They contain no instance paths, no design-global slot IDs, and no design-global allocation. Lowering after HIR proceeds per-unit without consulting design-global state.

### Artifact Composition

After per-unit compilation, a compile-time design-wide pass packages the per-unit artifacts into a deliverable. This is artifact composition, not semantic elaboration. It combines compiled specialization artifacts with elaborated design topology metadata into the form the constructor needs.

Artifact composition does not recompile specialization code, re-run LLVM optimization, or return to semantic elaboration.

### Constructor / Realization

At runtime (or at load time for AOT), the constructor builds the object graph from compiled specialization artifacts and the elaborated design topology:

- Instance construction (per-instance storage from body-shaped SpecLayout)
- Connectivity wiring (port bindings, connection descriptors)
- Per-instance constant blocks (value-only parameters)
- Generate-driven structure selection (which artifacts to install per instance)
- Debug tables (instance paths for `%m`)

This is object construction, not compilation. See [compilation-model.md](compilation-model.md).

### Execution

Event-driven simulation engine. Processes run as behavior on instance objects with object-local state access. See [runtime.md](runtime.md).

## Project Structure

```
include/lyra/
  common/        # shared types, utilities, diagnostics
  hir/           # language semantic IR (per-compilation-unit)
  xir/           # execution-model IR (per-compilation-unit)
  mir/           # control-flow IR (per-compilation-unit)
  llvm_backend/  # MIR -> LLVM IR (per-specialization)
  realization/   # artifact composition and constructor data
  runtime/       # simulation runtime (scheduler, signals)
  lowering/      # AST->HIR, HIR->XIR, XIR->MIR lowering
  semantic/      # semantic utilities
```

Headers in `include/lyra/`, implementations in `src/lyra/`.

### Key Components

- **Diagnostic** (`common/diagnostic.hpp`): Error reporting with source locations. Uses `std::expected<T, Diagnostic>` (aliased as `Result<T>`) for error propagation. Produces colorful terminal output with file:line:col and source context.

## Data Flow

```
1. Source files -> SlangFrontend -> AST  (whole-design frontend output)
2. AST -> Specialization Grouping -> per-unit HIR bodies  (compiler unit boundary)
3. Per unit: HIR -> XIR -> MIR -> LLVM IR  (independent per compilation unit)
4. All units: Artifact Composition -> deliverable  (compile-time, design-wide)
5. Constructor -> runtime object graph  (runtime, per-instance)
```

Step 2 is the key boundary: the compiler leaves the frontend's whole-design world and enters its own per-compilation-unit world. Steps 3 onward do not return to design-global semantic elaboration.

## Slang Data Ownership

Slang's `SourceManager` owns the memory backing source text, and many slang types (including `symbol->name`) return `string_view` into this memory.

**Solution**: HIR owns all data. At the slang -> HIR boundary (`AstToHir`), we copy all needed information into our own types with owned `std::string` names. This:

- Eliminates hidden lifetime dependencies on slang
- Makes the slang boundary explicit and complete
- Allows slang resources to be released after HIR construction
