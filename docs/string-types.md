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

## Our Solution

We preserve string literal origin via explicit separation and a flag:

### MIR Level

For display-like system calls ($display, $monitor, $strobe, $error, etc.):

- `format_expr` - explicit optional field for the format string expression
- `arguments` - only the format arguments (values to format)
- `format_expr_is_literal` - true if format_expr came from a string literal

### LIR Level

- `format_operand` - lowered format string expression (optional)
- `operands` - only the format arguments
- `format_string_is_literal` - propagated from MIR

### Underlying Literal

- `is_string_literal` on `Literal` - marks literals that came from string literals

This approach:

1. Separates format string from arguments at IR level (mirrors C++ codegen / assembly)
2. Enables format string detection while respecting slang's type system
3. Eliminates need for runtime extraction - format is explicitly separated at lowering time

## Alternatives Considered

**Override slang's types**: Always create string-typed MIR for string literals, handle conversion at emission. Rejected because it fights slang's type system and creates type mismatches throughout the pipeline.

**Dedicated MIR node**: `BitPackedStringExpression` that wraps string literals in bit context. Previously implemented, but removed as the flag-based approach is simpler and sufficient.

## Related

- LRM 6.16: String data type, conversion rules
- `common/literal.hpp`: `is_string_literal` flag definition
- `mir/expression.hpp`: `format_expr` and `format_expr_is_literal` in SystemCallExpression
- `lir/instruction.hpp`: `format_operand` and `format_string_is_literal` in Instruction
- `common/format_string.hpp`: Format string detection utilities
