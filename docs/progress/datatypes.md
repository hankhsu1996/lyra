# Datatypes

Tracks SystemVerilog data type coverage on the current pipeline. The integral family (`int`, `byte`,
`shortint`, `longint`, `time`, `integer`, `bit [N:0]`, `logic [N:0]`) lives in `integral.md`. This
file covers every other family: `datatypes/enum`, `datatypes/string`, `datatypes/unpacked` (fixed
unpacked arrays here; the variable-size aggregate family in `aggregate.md`; unpacked struct / union
in `struct-union.md`), `datatypes/general` (chandle, parameters, typedef), `datatypes/real`,
`datatypes/default_init`, `datatypes/representation`.

## Actionable

Enum, real, string, fixed-size unpacked arrays, the integral-family declaration initializers, and
parameter references in expressions are complete. The variable-size aggregate family (dynamic array,
queue, associative array) is complete; see `aggregate.md`. Unpacked struct and union are tracked in
`struct-union.md`. The default-init and representation families remain.

| Item | Status                                                                                    |
| ---- | ----------------------------------------------------------------------------------------- |
| -    | Variable-size aggregates (array / queue / assoc) -- `aggregate.md` (complete).            |
| -    | Unpacked struct / union -- `struct-union.md` (open).                                      |
| -    | `datatypes/default_init`, `datatypes/representation` -- no progress sub-steps opened yet. |

## Enum

LRM 6.19: enum types are a labelled subset of an integral base; enum -> integral is implicit,
integral -> enum requires an explicit cast.

- [x] E1 -- Declarations, member references, comparisons, arithmetic, and conversions, including the
      explicit base type form and the `first` / `last` / `num` / `num_auto_values` methods.
- [x] E2 -- `next` / `prev` methods with optional step argument (LRM 6.19.5.3 / 6.19.5.4).
      Non-member receivers fall back to a zero default; LRM Table 6-7 4-state `'x` behaviour is
      tracked under `datatypes/default_init`.
- [x] E3 -- `name()` method (LRM 6.19.5.6); empty string for non-member values.

### Cross-references

- LRM 6.19 (Enumerations), 6.19.3 (conversions), 6.19.5 (methods).
- Archive items: `datatypes/enum/{enum,enum_implicit_conversion,enum_methods}`.
- Unblocks: `control-flow.md` C16 (enum-typed `case` selectors).

## Real

LRM 6.12: `real` is IEEE 754 double, `shortreal` is IEEE 754 single, `realtime` is synonymous with
`real`. Edge event controls on real variables are forbidden.

- [x] C1 -- Declarations, blocking assignment, and `$display` formatting (`%f` / `%e` / `%g` with
      LRM Table 21-2 default precision 6).
- [x] C2 -- Operators legal on real per LRM 11.3.1 (arithmetic, relational, equality, logical,
      conditional), plus `**` via LRM 11.4.3 result-typing, plus shortreal <-> real conversion.
- [x] C3 -- Cross-family conversions: integral -> real treats `'x` / `'z` as 0 (LRM 6.12.1); real ->
      integral rounds half-away-from-zero (LRM 6.12.1).
- [x] C4 -- LRM-illegal real forms (LRM 6.12 + 11.3.1 / Table 11-1) diagnose as an unsupported
      diagnostic with an LRM citation. Slang's frontend filters all but case equality (`===` /
      `!==`); the case-equality path is guarded at lowering.
- [x] C5 -- Observable `real` / `shortreal` / `realtime` signals. A module-scope floating-point
      signal is an observable cell (`Var<Real>`): `wait (sig == ...)`, `always_comb` / `@*` reads,
      continuous assignment, non-blocking writes, and the explicit any-change `@(sig)` event control
      all propagate change events. An edge form (`@(posedge sig)`) stays frontend-rejected per LRM
      9.4.2 (an edge requires an integral LSB).

### Cross-references

- LRM 6.12 (Real, shortreal, realtime), 6.12.1 (Conversion), 11.3.1 (Operators with real operands),
  Table 11-1 (Operators and data types).
- Archive items: `datatypes/real/{real_types,shortreal_types,realtime_types}`.

## Structural Initializers

LRM 6.8: a static-lifetime variable declaration may carry an initializer expression (`int i = 42;`,
`real r = 1.5;`, `logic [7:0] l = 8'hAA;`); the initial-value assignment runs before any `initial` /
`always` procedure starts.

- [x] SI1 -- Declaration initializers for the integral, enum, real, shortreal, and realtime
      families.
- [x] SI2 -- Parameter references in any expression context (LRM 6.20). A `NamedValueExpression`
      whose symbol is a `parameter` / `localparam` lowers to the parameter's resolved value as a
      literal, matching how enum-member references are handled. Covers integer, packed, real,
      shortreal, and string parameter values; aggregate parameter types (unpacked array, queue,
      packed struct) are blocked behind their respective type workstreams. Type parameters
      (`parameter type T = int;`) ride on the type lowering path, not the expression path.

### Cross-references

- LRM 6.8 (Variable declarations); Table 6-7 (Default initial values, tracked separately under
  `datatypes/default_init`).

## String

LRM 6.16: `string` is a dynamic-length sequence of bytes with `""` as the default (Table 6-7) and
the operator set in Table 6-9: equality, lexicographic comparison, concatenation, replication,
indexing, and a dedicated method family. String literals carry a bit-vector type by default; the
frontend inserts an implicit conversion when a literal participates in an expression involving a
`string`-typed operand.

- [x] SC1 -- Declaration with literal initializer, assignment from a string literal, equality (`==`,
      `!=`), and lexicographic relational comparison (`<`, `<=`, `>`, `>=`) on string-typed
      operands. The literal-vs-literal case keeps the integral path per the LRM exception.
- [x] SC2 -- Concatenation `{a, b, ...}` and replication `{N{s}}` in string mode (LRM 11.4.12.2):
      when the result type is `string` the entire expression evaluates to string. The all-literal
      form (`{"foo", "bar"}`) types as a packed bit vector (LRM 5.9) and converts to string on
      assignment to a string target.
- [x] SC3 -- Built-in methods listed in LRM 6.16.1 -- 6.16.15: `.len()`, `.substr(i, j)`,
      `.compare(s)` / `.icompare(s)`, `.toupper()` / `.tolower()`, `.putc(i, c)` / `.getc(i)`, and
      the numeric conversion family (`.atoi()` / `.atohex()` / `.atooct()` / `.atobin()` /
      `.atoreal()` and `.itoa()` / `.hextoa()` / `.octtoa()` / `.bintoa()` / `.realtoa()`).
      Out-of-range indices and zero-byte writes follow LRM no-op rules; `substr(i, j)` returns the
      empty string for `i < 0`, `j < i`, or `j >= len`.

- [x] SC4 -- Indexing operator `s[i]` (LRM 6.16, Table 6-9) as a byte read and byte write, sharing
      the out-of-range no-op semantics of the `getc` / `putc` methods, plus `foreach (s[i])` which
      walks the string by byte (LRM 6.16). Blocking, compound, and nonblocking writes are all
      covered; the nonblocking write defers the byte update to the NBA region (LRM 10.4.2).

- [x] SC5 -- A string literal carries its LRM 5.9 dual nature: slang types it as a packed bit vector
      in every context except a `string`-typed one, so it serves both as an integral constant (a
      direct operand of an integral operator such as `byte == "x"`, an integral assignment, or a
      `byte` element write `s[i] = "x"`) and as text (a `string` value or a format / `%s` argument).
      The empty literal `""` stays the empty string through a `string` conversion, where the packed
      encoding alone would lose it.

### Cross-references

- LRM 6.16 (String data type), Table 6-9 (String operators), 6.16.1 -- 6.16.15 (String methods),
  Table 6-7 (Default initial values).
- Archive items: `datatypes/string/{string_concat}`.
