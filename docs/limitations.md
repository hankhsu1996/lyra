#Limitations

Current SystemVerilog features not yet supported.

## Statements

- `case` / `casex` / `casez` - switch statements
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
- Indexed part select (`[base+:width]`, `[base-:width]`)
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
