# Error Handling

This document describes error handling patterns in Lyra.

## Decision Tree

Use this flowchart to pick the right error type:

```
Where is the error?
│
├─ AST→MIR lowering (have source location from slang)
│   ├─ User error (unsupported feature, invalid code)
│   │   └─ DiagnosticException(source_range, "message")
│   └─ Compiler bug (invariant violated)
│       └─ InternalError("context", "message")
│
├─ MIR→LIR lowering / Codegen (no source location)
│   └─ Always InternalError (any error here = compiler bug)
│
├─ Interpreter runtime
│   ├─ Expected failure (file not found, invalid input)
│   │   └─ std::runtime_error("message")
│   └─ Should never happen (invariant violated)
│       └─ InternalError("context", "message")
│
├─ SDK runtime (generated C++ code)
│   └─ std::runtime_error (cannot use Lyra types)
│
└─ Shared code (interpreter + SDK)
    └─ std::expected<T, std::string> (caller decides policy)
```

## Error Types

### DiagnosticException (AST→MIR Only)

For **user errors** during frontend lowering where we have source location info.

```cpp
#include "lyra/common/diagnostic.hpp"

// ONLY use when you have a valid source_range from slang
throw DiagnosticException(
    Diagnostic::Error(source_range, "unknown identifier 'foo'"));
```

**Rules:**

- ONLY use in AST→MIR lowering
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
- Any error in MIR→LIR or codegen (these stages should never fail)

### std::runtime_error (Runtime Errors)

For errors during simulation (interpreter or SDK). These occur after compilation
when source location info is no longer available.

```cpp
// Interpreter runtime
throw std::runtime_error("failed to open memory file: " + path);

// SDK runtime (generated C++ code)
throw std::runtime_error("readmem address out of bounds");
```

**Use when:**

- File I/O failures
- Invalid runtime input (bad hex digit in $readmemh, etc.)
- Resource exhaustion

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

| Mechanism       | Debug Only? | Recovery? | When to Use                         |
| --------------- | ----------- | --------- | ----------------------------------- |
| `assert()`      | Yes         | No        | Expensive checks in hot paths       |
| `InternalError` | No          | Yes       | Default choice - runs in all builds |

**Prefer `InternalError`** for most cases. Use `assert()` only when:

- The check is computationally expensive
- You're in a very hot path
- The invariant is well-established and failure would be caught elsewhere

```cpp
// assert - debug only, for expensive invariant checks
assert(googol_computation_validates() && "invariant check");

// InternalError - always runs, provides diagnostics
if (items.empty()) {
  throw common::InternalError("process", "unexpected empty items");
}
```

## Summary Table

| Stage       | User Error            | Compiler Bug    | Runtime Failure      |
| ----------- | --------------------- | --------------- | -------------------- |
| AST→MIR     | `DiagnosticException` | `InternalError` | N/A                  |
| MIR→LIR     | N/A                   | `InternalError` | N/A                  |
| Codegen     | N/A                   | `InternalError` | N/A                  |
| Interpreter | N/A                   | `InternalError` | `std::runtime_error` |
| SDK         | N/A                   | N/A             | `std::runtime_error` |
| Shared code | N/A                   | N/A             | `std::expected`      |
