# Format dispatch (`Formatter<T>` + `FormatArg`)

Date: 2026-06-04 Status: accepted

## Context

The runtime print pipeline (`$display` / `$write` / `$strobe` / `$sformat` / `$fdisplay` and the
test framework's `expect.variables` round-trip) needs to format operands of many SystemVerilog
types: integral (packed bit vectors with 2-state / 4-state semantics), string, real32 / real64,
fixed unpacked array, dynamic array, and -- planned -- queue, associative array, struct, union,
enum, class instance, chandle. Each type has its own per-spec format rules (LRM 21.2.1.1
"unformatted" radix, LRM 21.2.1.3 `%t` time rescale, LRM 21.2.1.6 `%p` assignment pattern, LRM 6.16
string, etc.).

Two constraints define the dispatch problem:

1. **Runtime queue**: `$display` evaluates its args eagerly, then enqueues a `PrintItem` sequence
   that the runtime walks at print time. Type information must travel with each operand across the
   queue boundary.
2. **Open type set**: the list of formattable types grows as the SV surface grows. Every new
   user-defined type (struct, class) is a new format operand.

## Decision

**Per-type `Formatter<T>` specialization + type-erased `FormatArg`**, mirroring `std::formatter<T>`
and `std::basic_format_arg::handle`.

```cpp
template <typename T> struct Formatter;        // primary, undefined
template <> struct Formatter<PackedArray> { static auto Format(spec, value, ctx) -> string; };
template <> struct Formatter<String>      { static auto Format(spec, value) -> string; };
template <> struct Formatter<double>      { static auto Format(spec, value, ctx) -> string; };
template <typename U> struct Formatter<UnpackedArray<U>> {
  static auto Format(spec, value) -> string;  // no ctx; recurses
};
// ... etc.

struct FormatArg {
  const void* ptr;
  std::string (*format_fn)(const FormatSpec&, const void*, const FormatContext&);
};
```

The value-layer format walk (`value::Format(items, time_format)`) calls `Format(spec, arg, ctx)`
once per `PrintValueItem`; the call resolves through `arg.format_fn` into the right
`Formatter<T>::Format` specialization. The dispatch table is the function pointer; there is no
central catalog of formattable types.

Each `Formatter<T>` declares the signature it actually needs. Formatters that consult design-wide
context (`%t` needs `TimeFormat`, future `%m` would need the scope) take `FormatContext`; those that
do not (string, aggregate -- which never carries a context-bound spec kind) take just
`(spec, value)`. `MakeFormatArg`'s lambda absorbs the variance via
`if constexpr (requires { Format(spec, v, ctx); })`, so `FormatArg.format_fn` stays uniform without
forcing every formatter to accept an unused parameter.

### Why not a central `RuntimeValueView` variant

A variant of leaf views (integral / string / real / aggregate) couples every callsite to the union
of all types:

- Adding a type widens the variant -- every `std::visit` site recompiles, sometimes silently falls
  through.
- The variant's `sizeof` grows with the largest arm: an aggregate arm forced every singular
  `RuntimeValueView` to carry vector overhead.
- The aggregate arm in particular had to choose between owning (a `vector<RuntimeValueView>` --
  contradicts "view = borrow") and borrowing (impossible because the recursive element views have
  nowhere stable to live).

Per-type `Formatter` decentralizes the dispatch: each type owns its format knowledge in one place,
the central pipeline knows nothing about the type set.

### Lifetime contract

`FormatArg` borrows. The arg never owns; the underlying value must outlive every use of the arg. In
the runtime callers, the emit path builds the `PrintItem` array inline as the argument to the
runtime function, so caller-side temporaries inside the array initializer (e.g. `pa.View()`) stay
alive through the runtime call by C++ full-expression lifetime extension.
`PrintValue(const T&, spec)` takes `const T&` for the same reason -- rvalue args bind to the
reference and extend.

`$strobe` / `$fstrobe` capture operands by value into the postponed lambda; the FormatArg builds at
fire-time inside the lambda and borrows the lambda capture, which survives until the print completes
inside the lambda body.

### FormatContext

Some specs depend on design-wide state the spec itself cannot carry. `%t` reads `$timeformat` (LRM
20.4.3) which is an Engine-owned mutable record. Adding it to `FormatSpec` would couple every spec
to a mutable global at emit time.

```cpp
struct FormatContext {
  const TimeFormat* time_format = nullptr;
};
```

The value-layer items-walk takes the `TimeFormat` by reference and builds one
`FormatContext{&time_format}` for the whole walk. The `time_format` is reached from the Engine-owned
record through a `TimeFormat` accessor on `services` and threaded into the walk as an explicit
operand at lowering, rather than pulled from inside the walk -- so the format step itself holds no
engine state. Formatters that consult context for the requested spec kind check the relevant pointer
and throw on `nullptr`; the test framework passes the default `{}` because its specs never reach
those paths.

### Test framework integration

`ExpectedValue::BuildFormatArg()` returns a `FormatArg` whose `format_fn` is a static dispatcher
keyed on `ExpectedValueKind`. For integral / SV literal kinds the dispatcher materializes a
`PackedArray` from the owned word storage and routes through `Formatter<PackedArray>::Format`; for
string it routes through `Formatter<String>::Format`; for aggregates it walks the owned `elements`
vector and recurses. The YAML-side expected text and the SV-side observed text go through the same
`Format(...)` entry, agreeing byte-for-byte by construction.

## Aggregate `%p` shape (LRM 21.2.1.6)

Decisions about the OUTPUT shape, independent of dispatch:

1. **Output is `'{<e0>, <e1>, ...}` with `, ` between elements.** Empty container prints `'{}`.
2. **Integral element radix is decimal, not hex.** LRM 21.2.1.6 says "as it would unformatted"; the
   unformatted `$display` radix is decimal (LRM 21.2.1.1 `%d` default). We diverge from Verilator's
   `'{'ha, 'h14, 'h1e}` form intentionally to match the LRM text and the VCS convention.
3. **`%p` and `%0p` produce identical text in this scope.** LRM 21.2.1.6 permits `%0p` to be a
   shorter implementation-specific form, but for an integer-leaf aggregate there is nothing
   meaningfully shorter; divergent text would add a divergence surface without behavioral
   motivation.
4. **No index labels on fixed-size or dynamic arrays.** LRM permits `'{0:e0, 1:e1, ...}` but does
   not require it. Labels matter for associative arrays where position is otherwise undefined; for
   indexed containers position is implicit. Matches Verilator / VCS conventions.
5. **Test framework `expect.variables` accepts YAML sequences.** `a: [10, 20, 30]` (nested
   `a: [[1, 2, 3], [4, 5, 6]]`) parses to a recursive `ExpectedValue` of kind `kAggregate`. The
   per-variable rewrite emits `$display("a=%p", a)` so the SV side and YAML side go through the same
   format pipeline. _Rejected:_ SV-grammar string syntax in YAML (`"'{10, 20, 30}"`) would force the
   framework to ship an SV assignment-pattern parser and force escape-laden YAML for nested forms.

## Consequences

- **Open extension.** Adding `struct` / `union` / `enum` / `class` / `chandle` is a single
  `Formatter<T>` specialization per type; nothing in the central dispatch changes.
- **`%p` for queue / associative array** drops in the same way once those containers land -- one
  `Formatter` specialization that walks the container's element shape.
- **Per-element string concatenation per `%p` call** is unchanged from any direct-walk approach; no
  intermediate aggregate value tree is built. Memory per call is proportional to output text length
  only.
- **The `View` name is reserved for actual borrowing.** Leaf type views (`std::string_view` inside
  `Formatter<String>`, `std::span` inside `PackedArray::ValueWords()`) and `FormatArg` itself all
  borrow; nothing in the format pipeline owns intermediate value storage.

## Cross-references

- `progress/display.md` DI7
- `progress/aggregate.md` DA4
- `decisions/unpacked-array-representation.md`
- `decisions/runtime-shape-and-default-value.md`
