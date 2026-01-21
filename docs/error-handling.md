# Error Handling

This document describes error handling patterns in Lyra.

## Decision Tree

Use this flowchart to pick the right error type:

```
Where is the error?
│
├─ AST->HIR lowering (have source location from slang)
│   ├─ Invalid SV code (type mismatch, unknown identifier)
│   │   └─ DiagnosticException(source_range, "message")
│   ├─ Valid SV, not yet implemented
│   │   └─ UnsupportedError(layer=AstToHir, kind, origin, "detail")
│   └─ Compiler bug (invariant violated)
│       └─ InternalError("context", "message")
│
├─ HIR->MIR lowering
│   ├─ Valid SV, not yet implemented
│   │   └─ UnsupportedError(layer=HirToMir, kind, origin, "detail")
│   └─ Compiler bug (invariant violated)
│       └─ InternalError("context", "message")
│
├─ MIR->LLVM lowering
│   ├─ Valid SV, not yet implemented
│   │   └─ UnsupportedError(layer=MirToLlvm, kind, origin, "detail")
│   └─ Compiler bug (invariant violated)
│       └─ InternalError("context", "message")
│
├─ Interpreter runtime
│   ├─ Valid SV, not yet implemented
│   │   └─ UnsupportedError(layer=Execution, kind, origin, "detail")
│   ├─ Host/runtime failure (I/O, malformed external input)
│   │   └─ std::runtime_error("message")
│   ├─ SV-defined runtime semantics (4-state edge cases)
│   │   └─ No exception - produce LRM-defined value/update
│   └─ Invariant violated (should never happen)
│       └─ InternalError("context", "message")
│
├─ SDK runtime (generated C++ code)
│   └─ std::runtime_error (cannot use Lyra types)
│
└─ Shared code (interpreter + SDK)
    └─ std::expected<T, std::string> (caller decides policy)
```

## Error Types

### DiagnosticException (AST->HIR Only)

For **user errors** during frontend lowering where we have source location info.

```cpp
#include "lyra/common/diagnostic.hpp"

// ONLY use when you have a valid source_range from slang
throw DiagnosticException(
    Diagnostic::Error(source_range, "unknown identifier 'foo'"));
```

**Rules:**

- ONLY use in AST->HIR lowering
- NEVER use with empty source location `{}`
- If you don't have a source location, use a different error type

CLI commands catch and print these via `PrintDiagnostic()`.

### InternalError (Compiler Bugs)

For situations that **should never happen** - indicates a bug in Lyra itself.
Message includes "Please report this issue" prompt.

```cpp
#include "lyra/common/internal_error.hpp"

throw common::InternalError("codegen", "unexpected expression kind");
```

**Use when:**

- Switch statement hits an "impossible" case
- Invariant is violated
- Code path should be unreachable
- Any error in HIR->MIR or codegen (these stages should never fail)

### UnsupportedError (Valid SV, Not Yet Implemented)

For **valid SystemVerilog code** that Lyra doesn't support yet. Unlike `DiagnosticException`
(invalid SV) and `InternalError` (compiler bug), this indicates a known limitation.

```cpp
#include "lyra/common/unsupported_error.hpp"

throw common::UnsupportedErrorException(
    common::UnsupportedLayer::kMirToLlvm,
    common::UnsupportedKind::kType,
    context.GetCurrentOrigin(),
    "4-state types not yet supported");
```

**Fields:**

- `layer`: Where the limitation was hit (kAstToHir, kHirToMir, kMirToLlvm, kExecution)
- `kind`: Category of limitation (kType, kOperation, kFeature)
- `origin`: Opaque ID for tracing back to source (can be invalid)
- `detail`: Human-readable description

**Use when:**

- Wide integers (>64 bits) in LLVM backend
- 4-state types in LLVM backend
- Packed structs not yet implemented
- Unsupported operators or expressions

**Do NOT use for:**

- Invalid SV code (use `DiagnosticException`)
- Compiler bugs (use `InternalError`)
- Runtime I/O failures (use `std::runtime_error`)

**Origin Tracking:**

The `origin` field enables tracing errors back to source code locations. During HIR-to-MIR lowering,
the `OriginMap` records which HIR statement each MIR construct came from. When an error occurs in
later stages (MIR-to-LLVM, execution), the origin can be resolved to a source file:line:col location.

Error output follows clang style:

- With location: `file:line:col: error: message`
- Without location: `lyra: error: message`

The driver resolves origins via `OriginMap::Resolve()` -> `hir::Statement::span` -> `FormatSourceLocation()`.

### std::runtime_error (Host/Runtime Failures)

For failures external to SV semantics during simulation (interpreter or SDK).
These are host environment or input format issues, not SV language behavior.

```cpp
// Interpreter runtime
throw std::runtime_error("failed to open memory file: " + path);

// SDK runtime (generated C++ code)
throw std::runtime_error("invalid hex digit in $readmemh input");
```

**Use when:**

- File I/O failures ($readmemh cannot open file)
- Malformed external input (bad hex digit in $readmemh data)
- Resource exhaustion (OOM, allocation failure)

**Do NOT use for SV semantic edge cases** (array OOB, invalid bit select, etc.) -
those have LRM-defined behavior and should not throw.

### SV-Defined Runtime Semantics (No Exception)

Many "edge cases" in SystemVerilog are not errors - they have LRM-defined behavior
that the interpreter must implement.

**Examples:**

- Array index out of bounds (read): return X-filled value with correct width
- Array index out of bounds (write): defined behavior (often no-op or X-propagate)
- Negative index / bit offset: invalid select → return X with correct width
- Bit slice exceeds container: invalid select → return X with correct width

**Rule:** If you can point to an LRM rule that defines the result as X/Z/unchanged,
implement that behavior. Do not throw an exception.

### std::expected (Shared Code)

When code is shared between interpreter and SDK (e.g., parsing logic):

1. **Shared code** returns `std::expected<T, std::string>` (pure logic, no policy)
2. **Caller** converts error to `std::runtime_error`

```cpp
// common/parser.hpp - Pure parsing, no error policy
auto ParseToken(std::string_view s) -> std::expected<Token, std::string>;

// Interpreter or SDK usage
auto result = ParseToken(input);
if (!result) {
  throw std::runtime_error(result.error());
}
```

This keeps shared code simple and lets callers decide error handling.

## Forbidden Patterns

### Never use std::unreachable()

`std::unreachable()` is undefined behavior. If reached, there's no error message,
no stack trace, just undefined behavior or a crash. Always use `InternalError` instead:

```cpp
// BAD - undefined behavior, no diagnostics
switch (kind) {
  case A: return handleA();
  case B: return handleB();
}
std::unreachable();

// GOOD - clear error message if reached
switch (kind) {
  case A: return handleA();
  case B: return handleB();
}
throw common::InternalError("context", "unhandled kind");
```

### Never use DiagnosticException with empty source location

If you don't have a source location, you're in the wrong stage:

```cpp
// BAD - no source info means wrong error type
throw DiagnosticException(Diagnostic::Error({}, "some error"));

// GOOD - use InternalError for post-AST stages
throw common::InternalError("context", "some error");

// GOOD - use runtime_error for interpreter runtime
throw std::runtime_error("some error");
```

## assert() vs InternalError

| Mechanism       | Debug Only? | Recovery? | When to Use                                 |
| --------------- | ----------- | --------- | ------------------------------------------- |
| `assert()`      | Yes         | No        | Expensive checks already guarded elsewhere  |
| `InternalError` | No          | Yes       | Default choice - any invariant that matters |

**Use `InternalError` for any invariant whose violation would corrupt IR, constants, symbols, or semantics.** These must be enforced in release builds, not just debug.

Use `assert()` **only** when ALL of these are true:

- The check is computationally expensive
- Failure would be caught immediately in debug testing
- The invariant is already guarded elsewhere in the code path

```cpp
// BAD - this invariant could corrupt data silently in release
assert(total_words % 2 == 0 && "storage assumption");

// GOOD - enforced in all builds, clear error message
if (total_words % 2 != 0) {
  throw common::InternalError("context", "storage assumption violated");
}

// OK - expensive check, failure would be caught by downstream validation
assert(expensive_graph_validation() && "graph invariant");
```

## Summary Table

| Stage       | Invalid SV            | Unsupported SV     | Compiler Bug    | Host Failure         | SV Semantics |
| ----------- | --------------------- | ------------------ | --------------- | -------------------- | ------------ |
| AST->HIR    | `DiagnosticException` | `UnsupportedError` | `InternalError` | N/A                  | N/A          |
| HIR->MIR    | N/A                   | `UnsupportedError` | `InternalError` | N/A                  | N/A          |
| MIR->LLVM   | N/A                   | `UnsupportedError` | `InternalError` | N/A                  | N/A          |
| Interpreter | N/A                   | `UnsupportedError` | `InternalError` | `std::runtime_error` | No exception |
| SDK         | N/A                   | N/A                | N/A             | `std::runtime_error` | No exception |
| Shared code | N/A                   | N/A                | N/A             | `std::expected`      | N/A          |
