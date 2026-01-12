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

### Embedded SDK

The `lyra emit` command produces a standalone C++ project that includes SDK headers. To enable the binary to work from any directory without installation:

```
include/lyra/sdk/*.hpp  →  [genrule]  →  embedded_sdk.hpp  →  lyra binary
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
- MIR represents module templates
- Generated C++ constructs hierarchy at runtime
- Parameters become constructor arguments
- `generate for/if` becomes constructor logic

This trades some runtime initialization cost for faster compilation and better debuggability.

## Project Structure

```
include/lyra/
  common/       # shared types, utilities, diagnostics
  frontend/     # Slang wrapper, produces AST
  mir/          # module template IR
  codegen/      # MIR -> C++ generator
  sdk/          # runtime library (Task, Scheduler, Signal)
  cli/          # lyra subcommands (build, run, emit)
```

### Key Components

- **Diagnostic** (`common/diagnostic.hpp`): Error reporting with source locations. Uses `std::expected<T, Diagnostic>` (aliased as `Result<T>`) for error propagation. Produces colorful terminal output with file:line:col and source context.

Alternative execution path (interpreter):

- `lir/` - linearized IR for interpretation
- `lowering/mir_to_lir/` - MIR to LIR transformation
- `interpreter/` - direct execution without C++ compilation

The interpreter (`lyra run --interpret`) is useful for development and debugging.
It supports hierarchical modules via instance contexts (see `docs/interpreter-hierarchy.md`).

### Process vs Function

In the interpreter, **Process** and **Function** are distinct execution models reflecting SystemVerilog semantics:

| Aspect         | Process                                      | Function                                 |
| -------------- | -------------------------------------------- | ---------------------------------------- |
| **Lifetime**   | Long-lived, persists across simulation       | Ephemeral, exists during call            |
| **Suspension** | Can suspend (`@event`, `#delay`)             | Cannot suspend, runs to completion       |
| **Scheduling** | Scheduled independently by simulator         | Called synchronously within a process    |
| **Variables**  | Persist across suspensions in `ProcessFrame` | Local to call frame, destroyed on return |

**Implementation**: Both use the same block execution infrastructure. `RunProcess` manages a unified loop that switches between process blocks and function blocks via a call stack. When a function is called, its `CallFrame` is pushed; on return, execution resumes in the caller. This avoids duplicating block execution logic.

**Why separate concepts?** SystemVerilog processes (`initial`, `always`) are concurrent execution units with their own timeline. Functions are subroutines that borrow the caller's timeline. A function cannot contain delays because that would require the calling process to suspend mid-expression evaluation, which violates the language model.

**C++ analogy**: A Process is analogous to a C++ coroutine (can suspend and resume), while a Function is analogous to a regular C++ function (runs to completion). The `ProcessFrame` is the interpreter's equivalent of a coroutine frame.

### LIR Design Philosophy

LIR is a register-based IR using named temporaries (`%t0`, `%t1`) that maps to LLVM IR's SSA form.

**Two elaboration models:**

| Path                    | Elaboration                    | Variable Access                           |
| ----------------------- | ------------------------------ | ----------------------------------------- |
| C++ codegen             | Runtime (C++ builds hierarchy) | `this->u_child_.value` - member traversal |
| MIR → LIR → Interpreter | Compile-time (slang resolves)  | `$symbol` - flat lookup by unique pointer |

**Design guidance:**

- **Variable access**: follow slang's model - symbol is a unique compile-time resolved address
- **Operations** (arithmetic, control flow): think RISC-V assembly - register-based, explicit data flow
- **Method calls on complex types** (arrays, queues, classes): think RISC-V function calls - object pointer as `this`, call instruction for methods

## Data Flow

1. Source files -> `SlangFrontend` -> AST
2. AST -> `AstToMir` -> MIR Module (template)
3. MIR -> `Codegen` -> C++ source files
4. C++ compiler -> executable (linked with SDK)
5. Run executable

Each stage is independent and testable.

## Slang Data Ownership

Slang's `SourceManager` owns the memory backing source text, and many slang types (including `symbol->name`) return `string_view` into this memory.

**Current state**: We use `slang::ast::Symbol*` directly throughout MIR/LIR. This creates a hidden dependency: any code accessing symbol names requires the `SourceManager` to remain alive.

**Consequence**: `InterpreterResult` stores a `shared_ptr<SourceManager>` to keep this memory valid. This is a lifetime hack, not a conceptual fit.

**Future direction**: MIR/LIR should own their data. At the slang → MIR boundary (`AstToMir`), we should copy symbol information into our own types with owned `std::string` names. This would:

- Eliminate hidden lifetime dependencies on slang
- Make the slang boundary explicit and complete
- Allow slang resources to be released after lowering

## Interpreter Value Model

The interpreter uses `RuntimeValue` — a tagged union (`std::variant`) that can hold any SystemVerilog value (scalars, arrays, strings, etc.). Operations on values use centralized dispatch (switch on type tag).

**Contrast with C++ codegen**: In generated C++, types carry their own behavior. For example, `std::vector<T>` has a copy constructor that knows how to deep-copy itself. The compiler emits calls to type-specific methods. This is the OOP model — behavior is attached to types.

**Current interpreter model**: `RuntimeValue` is a variant, and operations like `DeepCopy()` switch on the type tag internally:

```cpp
RuntimeValue::DeepCopy() {
  if (IsArray()) { /* array copy logic */ }
  else { return *this; }
}
```

This is the procedural/tagged-union model — behavior is centralized, not attached to types.

**Consequence**: Adding new types with special semantics (queues, associative arrays, classes) requires modifying central switch statements rather than encapsulating behavior with each type. The interpreter doesn't mirror the codegen's OOP structure.

**Future direction**: Consider refactoring so that each SV runtime type (dynamic array, queue, class instance) has its own representation with encapsulated operations. This would:

- Mirror how C++ codegen works (types carry behavior)
- Make adding new types more modular (add a class, not modify switches)
- Improve separation of concerns
