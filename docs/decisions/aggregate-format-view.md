# Aggregate format view (`%p` / `%0p`)

Date: 2026-06-02 Status: accepted

## Context

LRM 21.2.1.6 `%p` / `%0p` prints any aggregate operand (unpacked array, struct, union) as an
assignment pattern: `'{<elem>, <elem>, ...}`. Each element follows a per-type rule (integral as
"would-be unformatted" -- i.e. decimal in `$display`; string quoted; struct named-field; etc.).
Multi-dim arrays and nested structs compose recursively.

Two layers needed to be reconciled:

1. The runtime value pipeline -- `lyra::value::RuntimeValueView`,
   `lyra::value::FormatValue(spec, view)` -- which up to this point modeled only singular types
   (integral / string / real). Every view variant was a borrowed shape with non-owning pointers into
   the operand's storage.
2. The test framework's `expect.variables` round-trip, which shares `FormatValue` with the runtime
   so the YAML-side expected text and the SV-side observed text are computed by the same code.

The forcing function for both: the first non-singular value type (fixed-size unpacked array of
integral) now has procedural test coverage and needs a way to assert against the whole-array value,
not per-element scalar probes.

## Decision

1. **`RuntimeValueView` gains an owning recursive arm.**
   `UnpackedArrayValueView { std::vector<RuntimeValueView> elements; }` joins the variant. The
   wrapper owns the element vector; each element view still borrows into the operand's per-element
   storage. The whole tree's lifetime matches the operand, same contract as the singular views.

   _Rejected:_ `std::span<const RuntimeValueView>` element list (forces a caller-managed temporary
   that must outlive the print call; awkward at every `$display` site). Type-erased
   `std::function<std::string(...)>` per element (drops the format-spec / view separation and blocks
   non-format consumers).

2. **`FormatValue` aggregate arm prints `'{<e0>, <e1>, ...}` with `, ` between elements.** Each
   element recurses through `FormatValue` with the _same_ `kAssignmentPattern` spec; the singular
   arms implement LRM 21.2.1.6 element rules (integral -> decimal, string -> quoted) and route the
   recursive element through the aggregate arm when itself an array.

3. **`%p` and `%0p` produce identical text in this scope.** LRM 21.2.1.6 permits `%0p` to be a
   shorter implementation-specific form, but for a fixed-size unpacked array of integral elements
   there is nothing meaningfully shorter; emitting different text for the same operand would add a
   divergence surface without a behavioral motivation.

4. **Integral element radix is decimal, not hex.** LRM 21.2.1.6 says "as it would unformatted"; the
   unformatted `$display` radix is decimal (LRM 21.2.1.1 `%d` default). Lyra prints `'{10, 20, 30}`
   where Verilator prints `'{'ha, 'h14, 'h1e}`. We match the LRM text and the VCS convention;
   diverging from Verilator is intentional.

5. **No index labels on fixed-size arrays.** LRM permits index labels (`'{0:e0, 1:e1, ...}`) but
   does not require them. Labels matter for associative arrays where position is otherwise
   undefined; for fixed-size the position is already implicit and the label adds noise. Matches
   Verilator / VCS conventions.

6. **Test framework `expect.variables` accepts YAML sequences.** `a: [10, 20, 30]` (and nested
   `a: [[1, 2, 3], [4, 5, 6]]`) parses to a recursive `ExpectedValue` of kind `kUnpackedArray`. The
   framework's per-variable rewrite emits `$display("a=%p", a)` so the SV-side output and the
   YAML-side `RenderExpectedFormatted` both go through the same
   `FormatValue(kAssignmentPattern, ...)` and agree byte-for-byte by construction.

   _Rejected:_ SV-grammar string syntax in YAML (`"'{10, 20, 30}"`). Requires the framework to ship
   an SV assignment-pattern parser and forces escape-laden YAML for nested forms; the user's spec
   language is YAML, so use YAML's sequence syntax.

## Consequences

- `RuntimeValueView` is no longer trivially copyable for the aggregate case (owns a
  `std::vector<RuntimeValueView>`). Per-call cost grows by one allocation per array level for `%p`
  paths; acceptable because `%p` is diagnostic / test surface, not a hot path.
- The recursive view shape generalises to the next aggregate type: a packed or unpacked struct
  lowers to a "named-field" variant arm that owns `(field_name, RuntimeValueView)` pairs. Same
  lifetime contract, same reuse of the singular arms.
- Multi-element array equality of formatted text (used by the test framework) is exact-string
  equality with no whitespace tolerance. If the runtime decides to drop a space or pretty-print,
  every existing whole-array test needs updating; the spec text is therefore load-bearing.
- The 30 unpacked tests that previously declared `int p0, p1, ..., pN` scalar probes for the sole
  purpose of `expect.variables` extraction now assert directly against the array. Future unpacked
  feature work follows the same shape -- per-element probes are an anti-pattern when the feature's
  contract is "the array now equals X".

## Cross-references

- `progress/display.md` DI7
- `progress/unpacked.md`
- `decisions/unpacked-array-representation.md`
