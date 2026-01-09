#Limitations

Current SystemVerilog features not yet supported.

## Data Types

Supported:

- Integral types (`bit`, `logic`, `reg`, `byte`, `shortint`, `int`, `longint`)
- Arbitrary-width bit vectors (1-bit to 200+ bits)
- Packed multi-dimensional arrays with non-zero lower bounds
- Unpacked fixed-size arrays
- `real`, `shortreal`, `string`
- `typedef` / type aliases
- `enum` types (named and anonymous)
- Enum methods: `first()`, `last()`, `next()`, `prev()`, `num()`, `name()` (IEEE 1800-2023 §6.19.5)

Not yet supported:

- `struct` (packed and unpacked)
- `union` (packed and unpacked)
- `class` types
- Dynamic arrays, associative arrays, queues

## Nets

Lyra uses a variable-only model (no `wire`/`net` types):

- No net types (`wire`, `tri`, etc.)
- No multi-driver resolution / strength
- `inout` ports not supported (require nets)

## Packages

Supported:

- `typedef` and `enum` declarations
- Functions in packages
- Variables in packages
- `import Pkg::*` (wildcard import)
- `Pkg::item` (qualified access)

Not yet supported:

- Tasks in packages
- Parameters/localparams in packages
- Package exports (`export pkg::*`)
- Classes in packages
- The `std` built-in package

## Constants

Supported:

- `parameter` and `localparam` declarations within modules
- Parameters with explicit types (`parameter int`, `parameter logic [7:0]`)
- Parameters with implicit types (inferred from value)
- Parameters used in expressions and display statements

Not yet supported:

- Parameterized modules with parameter port lists (`module m #(parameter int WIDTH = 8)`)
- Parameter override at instantiation (`mod #(16) u1()`, `mod #(.WIDTH(16)) u1()`)
- Type parameters (`parameter type T = int`)
- `defparam` statements
- `specparam` (timing parameters)

## Modules

- Generate blocks (`generate`, `genvar`)

## Subroutines (Tasks and Functions)

Supported:

- Function definitions inside modules
- Function definitions inside packages
- `automatic` lifetime (default for functions)
- `input` arguments (pass by value)
- Return values via `return` statement or function name assignment
- Void functions (called as statements)
- Recursive functions
- Nested function calls (function calling another function)
- Local variables inside function body

Not yet supported:

- `task` definitions (require timing controls)
- `output`, `inout`, `ref` arguments
- Default argument values (`arg = default`)
- Named argument binding (`.arg(value)`)
- `static` lifetime functions
- Class methods (classes not supported)
- Interface functions
- Constant functions (elaboration-time evaluation)
- DPI import/export functions

## Statements

- `case inside` - pattern matching
- `foreach` - dynamic arrays only (multi-dimensional and skipped dimensions supported)
- `wait(expr)` - wait statements
- `->` - event triggers
- `fork`/`join` - parallel blocks
- Assertions (`assert`, `assume`, `cover`)

## Expressions

- Struct member access (`.field`)
- `inside` operator

## Operators

- Case equality (`===`, `!==`)
- Wildcard equality (`==?`, `!=?`)
- Variable-count replication (`{n{expr}}` where `n` is not a constant)

## System Tasks/Functions

Supported:

- `$display`, `$displayb`, `$displayo`, `$displayh` - formatted output with newline (including `%t` format specifier)
- `$write`, `$writeb`, `$writeo`, `$writeh` - formatted output without newline
- `$strobe`, `$strobeb`, `$strobeo`, `$strobeh` - postponed region output
- `$monitor`, `$monitorb`, `$monitoro`, `$monitorh`, `$monitoron`, `$monitoroff` - value change monitoring
- `$finish`, `$stop`, `$exit` - simulation control
- `$time`, `$stime`, `$realtime` - simulation time (with timescale scaling)
- `$timeformat` - configure `%t` output format
- `$timeunit`, `$timeprecision` - query module timescale
- `$timeunit($root)`, `$timeprecision($root)` - query global precision
- `$printtimescale`, `$printtimescale($root)` - print timescale info
- `$readmemh`, `$readmemb`, `$writememh`, `$writememb` - memory file I/O (2-state only)

Not yet supported:

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

## $monitor Limitations

- **Same-time-slot operations**: When `$monitor` is replaced or `$monitoroff` is called in the same time slot as a value change, that change may not be detected. This is due to CheckMonitor running at the end of the time slot after all instructions have executed.
