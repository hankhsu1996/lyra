---
name: code-review
description: Reviews code changes for architectural fitness, extensibility, and alignment with design principles. Use when reviewing code, checking PRs, or validating implementations.
---

# Code Review

Early-stage C++ compiler project. Primary goal: keep architecture clean and extensible.

## Review Checklist

### 1. Architecture and Extensibility

- Does the change fit the intended structure and boundaries?
- If it doesn't fit existing abstractions, should we generalize the design?
- Will this scale as we add more features?

### 2. No Hacks or Special Cases

- Avoid patch-like solutions with feature-specific workarounds
- No scattered conditional logic ("just for this feature")
- If forcing something in, consider a better abstraction

### 3. Use Existing Utilities

- Don't reinvent what standard library or codebase already provides
- Use modern C++ when it improves clarity (not for fancy)

### 4. Error Handling

Check `docs/error-handling.md` for correct error type usage:

- `DiagnosticException` only in AST->HIR (with source location)
- `InternalError` for compiler bugs (HIR->MIR, LLVM lowering, unreachable code)
- `std::runtime_error` for runtime failures
- Never use `std::unreachable()` or `DiagnosticException({})`

### 5. Design Principles Alignment

Read `docs/design-principles.md` and check:

- No Workarounds
- Parameterize, Don't Specialize
- Capture Behavior at the Source
- Unify Before Multiplying
- Follow Established Patterns
- Use Domain Vocabulary
- Comments Explain Why, Not What

## Output Format

1. **Summary**: One sentence on what the change does
2. **Architecture**: Impact on system structure, potential issues
3. **Concerns**: Hacky patterns, violations of design principles
4. **Suggestions**: Better abstractions if needed
