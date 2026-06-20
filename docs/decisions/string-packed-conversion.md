# String values, the no-NUL invariant, and packed conversion

## Date

2026-06-20

## Status

Accepted

## Why this decision matters

A SystemVerilog string literal and a string variable are different things (LRM 5.9 vs 6.16), and the
conversion between bits and a string value has precise, NUL-sensitive rules. The codebase had
collapsed two genuinely different operations -- building a string value from bits, and formatting
bits as `%s` -- onto one path that implemented neither rule, so the empty string, embedded NULs, and
`%s` of a packed value were all wrong. A first attempt fixed the empty string with an operand-aware
special-case in the conversion render, which both contradicted `conversion-folding.md` and stayed
wrong for embedded NULs. This record pins the LRM rules, names the two operations, and fixes the
boundary that actually owns the semantics.

## Findings that shaped the design

### F1. A string literal is a packed bit-vector constant (LRM 5.9, 11.10.3)

A string literal's type is `bit[N:0]` in every context except a `string`-typed one; slang only
inserts a conversion to `string` when the context demands it. The empty literal `""` is special: LRM
11.10.3 gives it the value `"\0"` -- 8 zero bits. The slang frontend encodes exactly this
(`source/ast/expressions/LiteralExpressions.cpp`: an empty literal takes `width = 8`,
`SVInt(8, 0)`), citing 11.10.3 directly.

**Consequence:** a string literal is an integral value whose bits are its bytes, MSB-first. For the
non-empty case the width is `8 * len`; the empty case is padded to one zero byte.

### F2. A string variable never contains NUL (LRM 6.16)

LRM 6.16 is explicit: "A string variable shall not contain the special character `"\0"`. Assigning
the value 0 to a string character shall be ignored." A string variable can hold `""` (length 0); its
default value is `""` (Table 7-1). Indexing reads a byte and equals `getc` (Table 6-9, LRM 6.16.3);
`putc(i, 0)` is ignored (LRM 6.16.2).

**Consequence:** "no NUL byte" is not an edge rule -- it is the defining invariant of the string
value type.

### F3. Building a string value from bits strips every NUL (LRM 6.16)

The conversion of a string literal, or of an integral value, to a string variable is defined
verbatim: all `"\0"` characters are removed; if nothing remains, the result is the empty string;
otherwise the remaining characters. Casting an integral value first left-extends it with zeros to a
multiple of 8 bits, then applies the same removal.

**Consequence:** `""` (the `8'h00` of F1) strips its one NUL and becomes the empty string;
`"hello\0world"` becomes `"helloworld"`; `string'(16'h0041)` strips the leading zero byte and
becomes `"A"`. The empty string is not a special case in the conversion -- it is the NUL-strip rule
applied to F1's encoding.

### F4. `%s` is a presentation operation with different NUL semantics (LRM 21.2.1.7)

`%s` prints a value's bytes as ASCII characters; "no termination character or value is required at
the end of a string, and leading zeros are never printed." This is a display rule, not the
string-value conversion of F3, and it differs on embedded and trailing NULs: for `16'h4100` the F3
conversion yields `"A"` (the trailing NUL stripped), while `%s` keeps the trailing byte. The exact
rendering of a non-leading NUL under `%s` is not pinned by the LRM and diverges across simulators.

**Consequence:** "format bits as `%s`" and "build a string value from bits" are two operations with
two rules. Neither subsumes the other.

### F5. The codebase conflated F3 and F4 onto one lossy path

Both the packed-to-string conversion render and `%s` of a non-string operand route through a single
`ConversionExpr` to `StringType`, emitted as `String::FromPackedArray`, which copies every byte
verbatim -- it implements neither F3's NUL strip nor F4's leading-zero rule. The integral formatter
refuses `kString` outright (it throws, forcing `%s` back through the conversion). So the empty
string and embedded NULs came out wrong, and `%s` of a packed value rendered a literal NUL byte
rather than following 21.2.1.7.

**Consequence:** one shared path cannot be correct, because F3 and F4 want different NUL handling.

## Decision

1. **A `value::String` never contains a NUL byte (F2).** It is the runtime model of an SV string
   variable, and the no-NUL property is its invariant. Every boundary that constructs a `String`
   from arbitrary bits is responsible for enforcing it.
2. **Packed-to-string is one conversion that strips NUL (F3).** `String::FromPackedArray` -- the
   construction-from-bits boundary -- removes every NUL. It is used by the explicit cast, implicit
   assignment, a string literal in a string context, and `$sformat` / `$swrite` output. Because the
   NUL strip is the conversion's own semantics, the conversion render stays a pure
   `(source kind, destination kind)` map and never inspects its operand (it composes cleanly with
   `conversion-folding.md`). The `$sscanf` source lift of an unpacked byte array
   (`String::FromByteArray`) is a separate operation -- it views bytes as a scan stream where NUL is
   whitespace (LRM 21.3.4), so it preserves NUL and is not a string-value construction.
3. **HIR-to-MIR lowers a string literal to its decided MIR primitive (F1).** An integral-typed
   literal -- the default in every non-string context -- lowers to an `IntegerLiteral` carrying the
   integer constant of its bytes. A `mir::StringLiteral` is a software string literal (a plain
   `"text"`), not a built value: a `value::String` value is constructed from it through the generic
   `CallExpr` + `ConstructorCallee` shape (`String("text")`), the same construction path every other
   runtime value uses. The representation decision is in MIR's structure, so each backend renders
   mechanically by node kind and never re-derives it from the type, and never bakes a library
   constructor into a literal's rendering (`architecture/mir.md` invariant 10). The literal reaches
   a `String` only through the F3 conversion or this construction; the empty and embedded-NUL cases
   are handled by F3 in one place rather than by the literal's raw text.
4. **`%s` is presentation, not conversion (F4).** `%s` of a `String` prints its content (NUL-free by
   the invariant). `%s` of a packed value formats the packed value under LRM 21.2.1.7 through its
   own `Formatter` path, not by first converting to a string value. The LRM leaves a NUL byte's `%s`
   rendering unpinned; it renders as a space (the de-facto convention), so the empty literal prints
   one space and a byte sequence keeps its column count.

## Rejected

- **An operand-aware special-case in the conversion render** -- `Conversion(string, StringLiteral)`
  emitting the literal's raw text. It handles `""` only by accident (the literal has no characters),
  keeps embedded NULs (a C string literal truncates `"hello\0world"` to `"hello"`), and reintroduces
  the operand inspection `conversion-folding.md` forbids. The empty-string failure it targeted is
  really F3 not being implemented at the construction boundary.
- **One shared packed-to-string path for both the cast and `%s`.** F3 and F4 have different NUL
  semantics, so a single `FromPackedArray` can serve at most one of them correctly. Sharing it is
  the reason neither was right.

## Consequences

- The packed-to-string conversion is correct for every case in one place: `""`, `"hello\0world"`,
  and `string'(16'h0041)` all follow F3, and the empty string stops being a special case.
- The conversion render is a pure type map with no operand inspection; the literal-text special-case
  is removed.
- `value::String` is a faithful SV string value: downstream operations (concatenation, `substr`,
  comparison, `%s`) may assume it holds no NUL.
- `%s` and the string cast are decoupled, each matching its own LRM rule. `%s` of a packed value
  formats through a `Formatter` rather than fabricating a string value, so the integral formatter no
  longer has to refuse `kString`.

## Cross-references

- `conversion-folding.md` -- the conversion render is a pure `(source, destination)` map; this
  decision keeps that property by making the NUL strip the conversion's semantics rather than an
  operand-aware exception.
- `format-dispatch.md` -- `%s` resolves through `Formatter<T>`; this decision adds the
  packed-operand `%s` path (LRM 21.2.1.7) instead of routing it through a string conversion.
- `runtime-effects-as-generic-calls.md` -- value construction is a generic `CallExpr` +
  `ConstructorCallee`; a `value::String` is built that way rather than by a backend-baked literal.
- `value-type-concepts.md` -- the `value::String` no-NUL invariant joins the value-type contracts.
- LRM 5.9 (string literals), 11.10.3 (empty string literal value), 6.16 (string data type and
  conversion), 21.2.1.7 (`%s` string format), Table 6-9 (string operators), Table 7-1 (default
  value).
