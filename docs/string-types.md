#String Type Handling

How Lyra handles SystemVerilog string types,
and why.

        ##The Problem

            SystemVerilog strings have dual nature
    : they can be used as strings or
        as packed bit vectors
            .The LRM(6.16) specifies that strings convert to unsigned packed
        arrays(8 bits per character, MSB first)
            .

        Examples of dual use :

    -String context : `string s = "Hello";

`- Bit context :`bit[40] b = "Hello";
`- Mixed :`$display("Value: %d", x);` (format string detection)

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

## Current Solution (Incomplete)

We normalize string literals at the AST→MIR boundary, but the solution is incomplete.

### What We Do

**AST→MIR Normalization**: Bit-packed integral constants with `is_string_literal=true` are converted to string-typed constants. This is done ad-hoc at specific call sites (display tasks, mem_io tasks) rather than universally.

**MIR Level**: Display-like system calls have:

- `format_expr` - optional format string expression
- `arguments` - format arguments
- `display_props` - structured properties from `common/display_variant.hpp`

### Known Issues

1. **Normalization is ad-hoc**: `NormalizeFormatExpression()` is called only at specific call sites. The correct rule should be: "Any string literal becomes a string-typed constant in MIR, regardless of usage context."

2. **Semantic conflation**: We represent two distinct concepts as the same `String` type:
   - **String data** — bytes to print (e.g., a string variable)
   - **Format template** — a mini-language with `%d`, `%s` specifiers to parse

   The flag exists to disambiguate these, which is a design smell.

### Proper Fix (Not Yet Implemented)

The correct solution is to make formatting a first-class semantic operation:

- Either separate `FormatTemplate` from `StringValue` at the IR level
- Or lower display-like calls into explicit "format + apply" instructions

This would eliminate the flag because the distinction becomes structural, not annotated.

## Alternatives Considered

**Override slang's types**: Always create string-typed MIR for string literals, handle conversion at emission. Rejected because it fights slang's type system and creates type mismatches throughout the pipeline.

**Dedicated MIR node**: `BitPackedStringExpression` that wraps string literals in bit context. Previously implemented, but removed as the flag-based approach is simpler and sufficient.

## Related

- LRM 6.16: String data type, conversion rules
- `common/constant.hpp`: `is_string_literal` flag definition
- `common/display_variant.hpp`: Unified `DisplayVariantProps` for display-like calls
- `mir/expression.hpp`: `format_expr` and `display_props` in SystemCallExpression
- `common/format_string.hpp`: Format string detection utilities
