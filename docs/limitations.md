# Limitations

Current SystemVerilog features not yet supported.

## Data Types

Supported:

- Integral types (`bit`, `logic`, `reg`, `byte`, `shortint`, `int`, `longint`)
- Arbitrary-width bit vectors (1-bit to 200+ bits)
- Packed multi-dimensional arrays with non-zero lower bounds
- Unpacked fixed-size arrays (multi-dimensional, initialization patterns)
- Dynamic arrays (`.size()`, `.delete()`, `new[size]`, multi-dimensional)
- Queues (bounded/unbounded, `.push_*()`, `.pop_*()`, `.insert()`, `.size()`, `.delete()`)
- `real`, `shortreal`, `string`
- `typedef` / type aliases
- `enum` types (named and anonymous)
- Enum methods: `first()`, `last()`, `next()`, `prev()`, `num()`, `name()` (IEEE 1800-2023 6.19.5)

Not yet supported:

- `class` types
- Associative arrays
- Unpacked unions with 4-state fields: default initialization uses zero-fill instead of X encoding

## Nets

Lyra supports net-backed ports in alias mode (single-driver assumed):

- `input wire` ports: parent expression drives child port
- `output wire` ports: child continuous assign drives parent net via alias

**Not supported (compile-time error):**

- `inout` ports (all directions) - requires tri-state semantics
- Net declaration assignments (`wire logic a = b;`)
- Procedural writes to nets (blocking/nonblocking assignment)

**Not supported (undefined behavior, not detected):**

- Multi-driver scenarios (no resolution, no strength modeling)
- If multiple sources drive the same net, behavior is undefined

**Not modeled:**

- Wired nets (`wand`, `wor`, `triand`, `trior`)
- Strength/drive specifications (`supply`, `pull`, `weak`, `highz`)

## Continuous Assignments

Supported:

- Basic `assign var = expr;` at module level
- Complex RHS expressions with automatic sensitivity detection

Not yet supported:

- Delays (`assign #10 a = b;`)
- Drive strengths (`assign (strong1, pull0) a = b;`)
- Complex LHS targets (element select, concatenation)
- Net declaration assignments (`wire logic a = b;`)

## Packages

Supported:

- `typedef` and `enum` declarations
- Functions in packages
- Variables in packages
- Parameters and localparams in packages
- `import Pkg::*` (wildcard import)
- `Pkg::item` (qualified access)

Not yet supported:

- Tasks in packages
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

Supported:

- Conditional generate (`if-generate`)
- Loop generate (`for-generate` with `genvar`)
- Case generate (`case-generate`)
- Nested generate blocks (arbitrary nesting depth)
- Hierarchical references through generate blocks (e.g., `outer[0].inner[1].signal`)

Not yet supported:

- Upward hierarchical references (child accessing parent/sibling via `Parent.var`)

## Subroutines (Tasks and Functions)

Supported:

- Function definitions inside modules
- Function definitions inside packages
- `automatic` lifetime (default for functions)
- `input` arguments (pass by value)
- `output` arguments (callee writes to caller's storage)
- `inout` arguments (read and modify caller's storage)
- Return values via `return` statement or function name assignment
- Return types: integral, `string`, dynamic arrays, queues
- Container parameters: `string`, dynamic arrays, queues as input/output/inout
- Void functions (called as statements)
- Recursive functions
- Nested function calls (function calling another function)
- Local variables inside function body

Not yet supported:

- `ref` arguments
- `task` definitions (require timing controls)
- Default argument values (`arg = default`)
- Named argument binding (`.arg(value)`)
- `static` lifetime functions
- Class methods (classes not supported)
- Interface functions
- Constant functions (elaboration-time evaluation)
- DPI import/export functions
- `real`, `shortreal` return types
- Struct/union return types

## Statements

- `case inside` - pattern matching
- `foreach` - unpacked arrays, dynamic arrays, queues (multi-dimensional and skipped dimensions supported)
- `wait(expr)` - wait statements
- `->` - event triggers
- `fork`/`join` - parallel blocks
- Assertions (`assert`, `assume`, `cover`)

## Expressions

- Struct member access (`.field`)
- Replication assignment patterns (`'{n{val}}`)
- Unpacked array slicing (`arr[a:b]`, `arr[i+:w]`, `arr[i-:w]` on unpacked arrays)

## Operators

- Case equality (`===`, `!==`)
- Variable-count replication (`{n{expr}}` where `n` is not a constant)

## Wildcard Equality and Inside Operator

The `==?` (wildcard equality), `!=?` (wildcard inequality), and `inside` operators are supported with the following known limitations:

**X/Z initialization not observable in LLVM backend**: Variables initialized with X/Z values (e.g., `logic a = 1'bx`) may not preserve 4-state values correctly in the LLVM backend. This affects X-taint propagation testing. The MIR interpreter has similar issues. Until this is fixed, X-result tests are not automated.

**Coercion uses zero-extend regardless of signedness**: Per IEEE 1800-2017 11.4.6, `==?` should use the same coercion rules as `==`, which sign-extends when both operands are signed. Current implementation always zero-extends to max width. This is an intentional deviation that matches the existing `==` implementation.

**Wildcard detection limited to literals**: In `inside` expressions, only literal constants with X/Z bits (e.g., `4'b10zz`, `4'bx011`) are detected as wildcard patterns and use `==?`. Other constant expressions that evaluate to X/Z values are not detected:

```systemverilog
localparam logic [3:0] PAT = 4'b10zz;
val inside {PAT}     // Uses == (incorrect), should use ==?
val inside {4'b10zz} // Uses ==? (correct)
```

Non-constant expressions (variables) always use `==`, which is correct per the LRM.

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
- `$fopen`, `$fclose`, `$fflush` - file I/O (MCD and FD modes)
- `$fdisplay`, `$fdisplayb`, `$fdisplayo`, `$fdisplayh` - file output with newline
- `$fwrite`, `$fwriteb`, `$fwriteo`, `$fwriteh` - file output without newline
- `$fstrobe`, `$fstrobeb`, `$fstrobeo`, `$fstrobeh` - postponed file output
- `$fmonitor`, `$fmonitorb`, `$fmonitoro`, `$fmonitorh` - file value change monitoring
- `$test$plusargs`, `$value$plusargs` - plusargs command-line arguments
- `$fatal`, `$error`, `$warning`, `$info` - severity system tasks
- `$signed`, `$unsigned` - sign casting
- `$itor`, `$rtoi` - integer/real conversion
- `$realtobits`, `$bitstoreal` - real to 64-bit bitcast
- `$shortrealtobits`, `$bitstoshortreal` - shortreal to 32-bit bitcast
- Math functions (20.8): `$clog2`, `$ln`, `$log10`, `$exp`, `$sqrt`, `$pow`, `$floor`, `$ceil`, `$sin`, `$cos`, `$tan`, `$asin`, `$acos`, `$atan`, `$atan2`, `$hypot`, `$sinh`, `$cosh`, `$tanh`, `$asinh`, `$acosh`, `$atanh`

Not yet supported:

- `$random`, `$urandom` - random number generation
- `$fgets`, `$fscanf`, `$fread` - file reading
- `$fseek`, `$ftell`, `$rewind`, `$feof` - file positioning
- `$printtimescale(path)` - hierarchical path variant (requires hierarchy)
- `$timeunit(path)`, `$timeprecision(path)` - hierarchical path variants (requires hierarchy)
- `$timeunit($unit)`, `$timeprecision($unit)` - compilation unit variants
- `$cast` - dynamic type casting
- Data query functions (20.6): `$bits`, `$isunbounded`, `$typename`
- Array query functions (20.7): `$unpacked_dimensions`, `$dimensions`, `$left`, `$right`, `$low`, `$high`, `$increment`, `$size`
- Bit vector functions (20.9): `$countbits`, `$countones`, `$onehot`, `$onehot0`, `$isunknown`

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

- **Unique/priority violation reporting**: LRM 12.4.2.1 specifies that `unique`/`priority`/`unique0` violation reports should be deferred to the Observed region with "zero-delay glitch immunity" - violations are flushed if the process re-triggers before the Observed region. Lyra reports violations immediately via `$warning`, which may produce spurious reports in `always_comb` blocks with combinational feedback loops. Per-process violation tracking and `$assertcontrol` are not supported.

## $monitor Limitations

- **Same-time-slot operations**: When `$monitor` is replaced or `$monitoroff` is called in the same time slot as a value change, that change may not be detected. This is due to CheckMonitor running at the end of the time slot after all instructions have executed.

## Display/File Output Limitations

- **Runtime format strings in $display/$fdisplay (codegen only)**: Dynamic format strings stored in string variables are not supported in $display/$fdisplay in the codegen path:

  ```systemverilog
  string fmt = "X=%0d";
  $display(fmt, 7);    // Works in interpreter, fails in codegen
  $fdisplay(1, fmt, 7); // Same limitation
  ```

  However, `$sformat`/`$sformatf` support runtime format strings in both paths:

  ```systemverilog
  string fmt = "x=%0d";
  string s;
  $sformat(s, fmt, 42);  // Works in both interpreter and codegen
  ```
