#Limitations

Current SystemVerilog features not yet supported.

## Statements

- `foreach` - array iteration
- `return` - function returns
- `wait(expr)` - wait statements
- `->` - event triggers
- `fork`/`join` - parallel blocks
- Assertions (`assert`, `assume`, `cover`)

## Expressions

- Hierarchical references (`top.sub.signal`)
- Concatenation (`{a, b, c}`)
- Replication (`{4{byte}}`)
- Struct member access (`.field`)
- `inside` operator
- Unsized literals (`'0`, `'1`, `'x`)

## Operators

- Case equality (`===`, `!==`)
- Wildcard equality (`==?`, `!=?`)

## System Tasks/Functions

Supported:

- `$display` - formatted output
- `$finish`, `$stop`, `$exit` - simulation control
- `$time`, `$stime`, `$realtime` - simulation time (without timescale scaling)

Not yet implemented: `$write`, `$monitor`, `$random`, `$readmemh`, `timescale directive.

## Runtime Behavior

- **Bounds checking**: Out-of-bounds array/vector accesses produce undefined behavior instead of X values. SystemVerilog specifies that out-of-bounds reads return X and out-of-bounds writes are ignored, but Lyra does not currently implement this check. This applies to:
  - Array element access (`arr[i]` where `i` is out of range)
  - Bit/part select (`vec[i]`, `vec[i+:w]`, `vec[i-:w]` where the selection extends beyond the vector bounds)
