# String Type Handling

How Lyra handles SystemVerilog string types, and why.

## The Problem

SystemVerilog strings have dual nature: they can be used as strings or as packed bit vectors. The LRM (6.16) specifies that strings convert to unsigned packed arrays (8 bits per character, MSB first).

Examples of dual use:

- String context: `string s = "Hello";`
- Bit context: `bit[40] b = "Hello";`
- Mixed: `$display("Value: %d", x);` (format string detection)

## Slang's Approach

Slang (our frontend) resolves this by typing string literals based on context:

| Context            | Slang's Type | Conversion                   |
| ------------------ | ------------ | ---------------------------- |
| `string s = "Hi"`  | `bit[16]`    | Inserts Conversion to string |
| `bit[16] b = "Hi"` | `bit[16]`    | None needed                  |
| `$display("Hi")`   | `bit[16]`    | None (system call)           |

Key insight: **Slang always types string literals as `bit[N]`**, never as `string`. It inserts `Conversion` nodes when the target is a string variable.

## The Challenge

For format string detection in `$display`, we need to know if an argument was originally a string literal. But:

1. Slang types `$display("Hello")` argument as `bit[40]`, not `string`
2. Slang does NOT insert a Conversion node for system call arguments
3. We cannot distinguish "string literal typed as bits" from "actual integral value"

Without tracking, `$display("x=%d", 42)` would not be recognized as having a format string.

## Our Solution

We preserve string literal origin via a flag:

- `is_string_literal` on `Literal` - marks literals that came from string literals
- `first_operand_is_string_literal` on LIR `Instruction` - propagates to runtime

This enables format string detection while respecting slang's type system.

## Alternatives Considered

**Override slang's types**: Always create string-typed MIR for string literals, handle conversion at emission. Rejected because it fights slang's type system and creates type mismatches throughout the pipeline.

**Dedicated MIR node**: `BitPackedStringExpression` that wraps string literals in bit context. Previously implemented, but removed as the flag-based approach is simpler and sufficient.

## Related

- LRM 6.16: String data type, conversion rules
- `common/literal.hpp`: `is_string_literal` flag definition
- `common/format_string.hpp`: Format string detection utilities
