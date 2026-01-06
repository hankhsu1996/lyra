# Limitations

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
- Range/part select (`[7:0]`, `[base+:width]`)
- Struct member access (`.field`)
- `inside` operator
- Unsized literals (`'0`, `'1`, `'x`)

## Operators

- Case equality (`===`, `!==`)
- Wildcard equality (`==?`, `!=?`)

## System Tasks

Only `$display` and `$finish` are supported. Others like `$write`, `$monitor`, `$random`, `$readmemh` are not yet implemented.
