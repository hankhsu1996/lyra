# Error Handling

This document defines the **one true** error-handling model in Lyra. The goals are:

- **User-facing errors are never thrown** — they are returned as `std::expected`.
- **Compiler bugs are the only exceptions** — `InternalError` only.
- **No information loss** — spans, categories, and notes must survive across all stages.
- **No internal handles leak** — `OriginId` never crosses a stage boundary; it must be resolved to `SourceSpan` at the point the `Diagnostic` is produced.
- **Smallest responsible node** — diagnostics must point to the most precise syntax that caused the issue (projection/select/operator), with structured fallback only when precision is unavailable.

---

## Core Principles

1. **`std::expected` for user-facing failures**
   Every compilation stage returns `std::expected<Output, Diagnostic>`. User-facing errors include:
   - Invalid SV (frontend semantics/type errors)
   - Unsupported (valid SV but not implemented)
   - Host failures (I/O, external input malformed, resource exhaustion)

2. **Exceptions only for compiler bugs**
   `common::InternalError` is the **only** exception type used in compiler stages. If an invariant violation occurs, throw `InternalError`.

3. **Lossless propagation**
   No stage may discard `Diagnostic` payload. Wrapping errors into `std::string` is forbidden.

4. **Resolve before returning**
   Any internal indirection (`OriginId`, `OriginMap`, HIR node IDs, etc.) must be resolved into a `SourceSpan` **before** a `Diagnostic` is returned across a stage boundary.

5. **Smallest responsible node**
   When producing a user-facing error, prefer the span of the exact responsible syntax node (e.g., `[31:20]` select expression / member access / operator token span if available). If not available, fall back structurally to an enclosing span.

---

## Decision Tree

```
Is this user-facing? (invalid SV / unsupported / host failure)
│
├─ Yes → return std::unexpected(Diagnostic{...})
│
└─ No (compiler bug / invariant violated)
    └─ throw common::InternalError("context", "message")
```

---

## Diagnostic (User-Facing Errors)

All user-facing errors are represented by a single type: `Diagnostic`.

### Structure

```cpp
enum class DiagKind : uint8_t {
  kError,        // invalid SV / semantic error
  kUnsupported,  // valid SV, not implemented
  kHostError,    // I/O, malformed external input, OOM, etc.
  kWarning,      // optional (non-fatal)
  kNote,         // auxiliary message (usually attached via notes)
};

struct DiagItem {
  DiagKind kind;
  std::optional<SourceSpan> span;  // required for SV-attributable items
  std::string message;
};

struct Diagnostic {
  DiagItem primary;                 // required: exactly one primary per failure
  std::vector<DiagItem> notes;      // optional: extra context (may have spans)
};
```

### Hard Rules

- For `primary.kind in {kError, kUnsupported}` → `primary.span` **must be present**.
- For `primary.kind == kHostError` → `primary.span` **may be absent**.
- Notes:
  - `notes[i].kind` should be `kNote` (or `kWarning` for warnings).
  - `notes[i].span` is optional but recommended when applicable.

- A stage must return **exactly one** fatal `Diagnostic` (one primary + notes).
  (Multi-error reporting is handled by a separate sink; see "Multi-Diagnostic Policy".)

### Constructors / Helpers (Recommended)

```cpp
struct Diagnostic {
  static auto Error(SourceSpan span, std::string msg) -> Diagnostic;
  static auto Unsupported(SourceSpan span, std::string msg) -> Diagnostic;
  static auto HostError(std::string msg) -> Diagnostic;

  auto WithNote(std::optional<SourceSpan> span, std::string msg) && -> Diagnostic;
};
```

### Output Format (Clang-Style)

- With location:

  ```
  file.sv:42:5: error: unknown identifier 'foo'
  file.sv:10:3: unsupported: 4-state types not yet supported
  file.sv:10:3: note: required by assignment here
  ```

- Without location (host-only):

  ```
  lyra: error: failed to open file: missing.txt
  ```

---

## InternalError (Compiler and Runtime Bugs)

`InternalError` indicates a Lyra bug or violated invariant and is the only exception allowed.

```cpp
#include "lyra/common/internal_error.hpp"
throw common::InternalError("codegen", "unexpected expression kind");
```

### Use When

- Impossible `switch` cases
- Corrupted IR invariants
- Unreachable code paths (in release too)
- Runtime library invariant violations (e.g., refcount underflow, null where non-null required)

### Never Use When

- Invalid SV (return `Diagnostic::Error`)
- Unsupported feature (return `Diagnostic::Unsupported`)
- Host failure (return `Diagnostic::HostError`)

### Never Use `assert`

Do not use `assert()` for invariant checks. Asserts only fire in debug builds; `InternalError` fires in all builds and provides consistent error reporting. This applies to both compiler stages and runtime library code.

---

## Stage APIs (Canonical)

All compilation stages return `std::expected<Output, Diagnostic>`.

```cpp
// AST -> HIR
auto LowerAstToHir(const slang::Compilation& compilation,
                   DiagnosticSink& sink)
    -> std::expected<HirResult, Diagnostic>;

// HIR -> MIR
auto LowerHirToMir(const LoweringInput& input,
                   DiagnosticSink& sink)
    -> std::expected<MirResult, Diagnostic>;

// MIR -> LLVM
auto LowerMirToLlvm(const MirInput& input,
                    DiagnosticSink& sink)
    -> std::expected<LlvmResult, Diagnostic>;
```

### Invariant

If a stage can attribute an error to SV source, it must return a `Diagnostic` whose primary has a valid `SourceSpan`.

---

## Multi-Diagnostic Policy (Sink)

Lyra supports optional **non-fatal** reporting (warnings, notes, additional context) via a sink. The **return value** remains a single fatal `Diagnostic` on failure.

### Rules

- The sink may collect:
  - warnings
  - informational notes
  - optional "secondary errors" only if you explicitly decide to support "continue after error" in that stage.

- The stage's returned `Diagnostic` remains the one authoritative failure.

### Recommended Sink API

```cpp
class DiagnosticSink {
 public:
  void Emit(DiagItem item);  // warnings/notes
  // Optional: tracking counts, etc.
};
```

---

## Origin and Span Resolution

### Rule: `OriginId` Never Appears in `Diagnostic`

`OriginId` / `OriginMap` are internal bookkeeping only.

- **Inside a stage**, you may throw or carry `OriginId` internally.
- **Before returning** a `Diagnostic`, resolve to `SourceSpan`.

### Canonical Resolution API

```cpp
auto ResolveOriginToSpan(common::OriginId id,
                         const lowering::OriginMap& origin_map,
                         const hir::Arena& hir_arena,
                         const SourceManager& sm)
    -> std::optional<SourceSpan>;
```

### Stage Responsibility

- AST→HIR: you already have slang source ranges → produce `SourceSpan` directly.
- HIR→MIR: you have HIR nodes + source manager → produce `SourceSpan` directly (preferred).
- MIR→LLVM / Execution: you must have `origin_map + hir_arena + source_manager` available in the stage context so you can resolve and return `Diagnostic`.

### Forbidden Pattern

Returning a Diagnostic that contains an unresolved handle (directly or indirectly).

---

## Precision Policy: Smallest Responsible Node

When producing `Diagnostic::Unsupported` / `Diagnostic::Error`, choose the most precise span available:

1. Exact projection/select/member/operator expression span
2. Enclosing expression span
3. Enclosing statement span
4. Enclosing function/process/module span (last resort)

This is a **structural dominance** rule: the stage should attach spans at the point constructs are created so later stages can reliably report precise locations.

---

## Runtime Semantics vs Errors

Many SystemVerilog "edge cases" are not errors; they have LRM-defined behavior. Do **not** emit diagnostics for these cases.

Examples (non-exhaustive):

- out-of-bounds read → X-filled value (with correct width)
- invalid select → X
- etc.

If the LRM defines behavior, implement it.

---

## Host Failures

Host failures are user-facing, but may not have a source span.

Examples:

- `$readmemh` file open failure
- malformed external data
- OOM / allocation failure (if caught and handled)

Return:

```cpp
return std::unexpected(Diagnostic::HostError("failed to open file: " + path));
```

If you can attribute host failure to a call site (e.g., `$readmemh("x")` span), attach that span and add a note for the OS error.

---

## Verification Functions

Internal IR validation is compiler-internal. Throw `InternalError` on failure:

```cpp
void VerifyFunction(const mir::Function& f, const mir::Arena& a);
```

No stage should convert verification failures into user-facing diagnostics.

---

## Forbidden Patterns

### Throwing for user-facing failures

```cpp
// BAD
throw UnsupportedErrorException(...);

// GOOD
return std::unexpected(Diagnostic::Unsupported(span, "..."));
```

### Converting diagnostics into strings

```cpp
// BAD
catch (...) { return std::unexpected(Diagnostic::HostError(e.what())); } // loses spans/category

// GOOD
return std::unexpected(result.error()); // preserve Diagnostic
```

### Returning SV-attributable error without span

```cpp
// BAD
return std::unexpected(Diagnostic::Unsupported(std::nullopt, "..."));

// GOOD
return std::unexpected(Diagnostic::Unsupported(expr.span, "..."));
```

### `std::unreachable()`

Use `InternalError` instead.

---

## Driver / CLI Contract

- The driver prints exactly one fatal diagnostic (primary + notes) on failure.
- Formatting is clang-style.
- Exit code is non-zero on failure.
- Warnings emitted through `DiagnosticSink` may be printed even on success.

---

## Summary Table

| Stage       | Invalid SV          | Unsupported         | Compiler Bug          | Host Failure        | SV Semantics |
| ----------- | ------------------- | ------------------- | --------------------- | ------------------- | ------------ |
| AST→HIR     | return `Diagnostic` | return `Diagnostic` | throw `InternalError` | N/A                 | N/A          |
| HIR→MIR     | N/A                 | return `Diagnostic` | throw `InternalError` | N/A                 | N/A          |
| MIR→LLVM    | N/A                 | return `Diagnostic` | throw `InternalError` | N/A                 | N/A          |
| Interpreter | N/A                 | return `Diagnostic` | throw `InternalError` | return `Diagnostic` | no error     |
