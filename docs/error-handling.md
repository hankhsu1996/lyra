# Error Handling

This document describes error handling patterns in Lyra.

## Error Categories

Lyra has three distinct error categories based on **when** the error occurs:

| Stage               | Has Source Location? | Error Type            |
| ------------------- | -------------------- | --------------------- |
| AST→MIR lowering    | Yes (from slang)     | `DiagnosticException` |
| MIR→LIR, Codegen    | No                   | `InternalError` only  |
| Interpreter runtime | No                   | `std::runtime_error`  |
| SDK runtime         | No                   | `std::runtime_error`  |

**Key insight**: `DiagnosticException` should ONLY be used during AST→MIR lowering,
where we have source locations from slang. After that stage, source info is lost.

## Error Types

### DiagnosticException (AST→MIR Lowering Only)

For user errors during frontend lowering where we have source location info.

```cpp
#include "lyra/common/diagnostic.hpp"

// ONLY use when you have a valid source_range from slang
throw DiagnosticException(
    Diagnostic::Error(source_range, "unknown identifier 'foo'"));
```

**Never use with empty source location `{}`** - if you don't have a source
location, you're in the wrong stage and should use a different error type.

CLI commands catch and print these via `PrintDiagnostic()`.

### InternalError (Compiler Bugs)

For situations that should never happen - indicates a bug in Lyra itself.
Message includes "Please report this issue" prompt.

```cpp
#include "lyra/common/internal_error.hpp"

throw common::InternalError("codegen", "unexpected expression kind");
```

Use when:

- Switch statement hits an "impossible" case
- Invariant is violated
- Assertion would be appropriate but exception is needed

### std::runtime_error (Runtime Errors)

For errors during simulation (interpreter or SDK). These occur after compilation
when source location info is no longer available.

```cpp
// Interpreter runtime
throw std::runtime_error("failed to open memory file: " + path);

// SDK runtime (generated C++ code)
throw std::runtime_error("readmem address out of bounds");
```

## Shared Code Pattern

When code is shared between interpreter and SDK (e.g., parsing logic):

1. **Shared code** returns `std::expected<T, std::string>` (pure logic, no policy)
2. **Interpreter** converts error to `std::runtime_error`
3. **SDK** converts error to `std::runtime_error`

```cpp
// common/parser.hpp - Pure parsing, no error policy
auto ParseToken(std::string_view s) -> std::expected<Token, std::string>;

// Interpreter or SDK usage - both use runtime_error
auto result = ParseToken(input);
if (!result) {
  throw std::runtime_error(result.error());
}
```

This keeps shared code simple and lets callers decide error handling.

## Assertions vs Exceptions

| Mechanism       | Debug Only? | Recovery? | When to Use                       |
| --------------- | ----------- | --------- | --------------------------------- |
| `assert()`      | Yes         | No        | Sanity checks, fast-fail in debug |
| `InternalError` | No          | Yes       | Bugs that should be reported      |

**Use `assert()` when:**

- The check is expensive and only needed during development
- Failure means immediate termination is acceptable
- You're checking internal invariants that "can't possibly fail"

**Use `InternalError` when:**

- The check should run in release builds
- You want the error to propagate and be caught/logged
- The user should see a "please report this bug" message

```cpp
// assert - debug only, terminates
assert(!items.empty() && "items should never be empty here");

// InternalError - always runs, can be caught
if (items.empty()) {
  throw common::InternalError("process", "unexpected empty items");
}
```

In practice, prefer `InternalError` for most cases since it provides better
diagnostics. Use `assert` only for hot paths where the check is expensive.

## Guidelines

1. **Never use `InternalError` for user mistakes** - it tells users to file a bug report
2. **Prefer `std::expected` over exceptions** for recoverable errors in hot paths
3. **Include source location when available** - helps users find the problem
4. **Use specific messages** - "invalid hex digit 'g'" not "parse error"
5. **SDK code cannot use Lyra types** - only `std::` exceptions
6. **Prefer `InternalError` over `assert`** - better diagnostics in release builds
