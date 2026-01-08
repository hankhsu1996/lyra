# Limitations

Current SystemVerilog features not yet supported.

## Nets

Lyra uses a variable-only model (no `wire`/`net` types):

- No net types (`wire`, `tri`, etc.)
- No multi-driver resolution / strength
- `inout` ports not supported (require nets)

## Modules

- Parameterized modules (`parameter`, `localparam`, `#(...)`)
- Generate blocks (`generate`, `genvar`)

## Statements

- `case inside` - pattern matching
- `foreach` - multi-dimensional array iteration (1D supported)
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

- `$display` - formatted output (including `%t` format specifier)
- `$finish`, `$stop`, `$exit` - simulation control
- `$time`, `$stime`, `$realtime` - simulation time (with timescale scaling)
- `$timeformat` - configure `%t` output format
- `$timeunit`, `$timeprecision` - query module timescale
- `$timeunit($root)`, `$timeprecision($root)` - query global precision
- `$printtimescale`, `$printtimescale($root)` - print timescale info
- `$readmemh`, `$readmemb`, `$writememh`, `$writememb` - memory file I/O (2-state only)

Not yet supported:

- `$write`, `$monitor` - output variants
- `$random`, `$urandom` - random number generation
- `$printtimescale(path)` - hierarchical path variant (requires hierarchy)
- `$timeunit(path)`, `$timeprecision(path)` - hierarchical path variants (requires hierarchy)
- `$timeunit($unit)`, `$timeprecision($unit)` - compilation unit variants

## Timescale

Supported:

- `` `timescale `` directive with delay scaling
- Per-module time unit and precision

Not yet supported:

- `timeunit` / `timeprecision` statements (alternative to directive)
- Hierarchical designs with different timescales per instance
- `$unit` compilation unit timescale
- Real number delays (e.g., `#1.5`)

## Scheduling Regions

Per IEEE 1800-2023 Section 4.4, some regions are not yet implemented:

- `Preponed` - `#1step` sampling not supported
- `Observed` - assertion/property evaluation not supported
- `Reactive` - program blocks not supported
- `ReInactive` - `#0` in reactive context not supported
- `ReNBA` - `<=` in reactive context not supported

See [scheduling.md](scheduling.md) for implemented regions.

## Runtime Behavior

- **Bounds checking**: Out-of-bounds array/vector accesses produce undefined behavior instead of X values. SystemVerilog specifies that out-of-bounds reads return X and out-of-bounds writes are ignored, but Lyra does not currently implement this check. This applies to:
  - Array element access (`arr[i]` where `i` is out of range)
  - Bit/part select (`vec[i]`, `vec[i+:w]`, `vec[i-:w]` where the selection extends beyond the vector bounds)
