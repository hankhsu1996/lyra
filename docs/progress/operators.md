# Operators

Tracks the operator surface. The sub-steps cover what sits outside the integral family: set
membership, wildcard / case equality, selectors (bit-select, part-select, indexed part-select) on
both read and write sides, concatenation, replication, the streaming operators, compound assignment,
and the `++` / `--` family.

Done when:

- Set membership, wildcard and case equality, concatenation, replication, the streaming operators,
  compound assignment, and `++` / `--` all run.
- The indexed part-select surface runs. The selector lives here rather than in `packed.md`,
  following the type-vs-operator boundary.

## Actionable

Every numbered item is closed. What stays open is the forms recorded as rejected under them.

## Sub-Steps

The numeric IDs (W1..W15) imply execution order; where a cut is independent the text says so.

### Membership and equality

- [x] W1 -- `inside` set-membership (LRM 11.4.13). Singular and range items; ranges use
      `(>= lo) && (<= hi)`. The LRM 11.4.13 four-state corner ("no match but some compare yields
      `1'bx`") is preserved through the existing logical-OR truth table.
  - [ ] The tolerance-range item form (`inside {[a:b]}` with a `+/-` tolerance, LRM 11.4.13) is
        rejected; only singular and plain `[lo:hi]` range items are accepted.
- [x] W2 -- Wildcard equality `==?` / `!=?` (LRM 11.4.6). Asymmetric: X / Z in the right operand are
      wildcards; X / Z in the left operand are not. Result is `1'bx` when the left operand carries X
      / Z that meets a known right-operand bit and no other bit definitely mismatches. W1's value
      items use `==?` so wildcard items in `inside` work.
- [x] W3 -- Case equality `===` / `!==` (LRM 11.4.5). Bit-exact 4-state compare (X matches X, Z
      matches Z, X does not match Z); deterministic bool.
  - [x] The conditional operator's `&&&` multi-condition form (LRM 12.4): the `&&&`-separated
        conditions form a conjunction, taken iff every condition is true, desugared to the same
        chained logical-AND the `if`-statement predicate uses.
  - [ ] The conditional operator's `matches` pattern form (LRM 11.4.11 / 12.6) is rejected; the
        plain `c ? a : b` ternary and its `&&&` predicate are supported.
  - [ ] An array query (`$size` / `$left` / `$right` / `$low` / `$high`, LRM 20.7) whose dimension
        index is a run-time value over an array with a run-time dimension is rejected; a constant
        dimension index over a fixed-size operand folds at elaboration.

### Selectors

LRM distinguishes two syntactically distinct selector operators with overlapping but not identical
operand domains:

- **Element-select** `expr[idx]` (LRM 7.4.5; LRM 11.5.1 names it "bit-select" when the operand is a
  1D vector).
- **Range-select** `expr[hi:lo]` / `expr[base +: w]` / `expr[base -: w]` (LRM 7.4.5; LRM 11.5.1
  names it "part-select" or "indexed part-select" when the operand is a 1D vector).

The two operators differ on operand validity (associative arrays and strings allow element-select
but not range-select; LRM 7.4.6 and 6.16), so the IR keeps them as separate variants rather than a
merged node.

- [x] W4 -- Element-select read `v[idx]`. Covers 1D bit-select and multi-dim element-select
      uniformly. LRM 7.4.5 / 11.5.1 OOB and X / Z propagation rules apply.
  - [ ] Element-select on a non-integral operand beyond the supported families (LRM 7.4.6) is
        rejected.
- [x] W5 -- Range-select read covering all three bounds forms: constant `v[msb:lsb]`, indexed-up
      `v[base +: w]`, indexed-down `v[base -: w]`.
  - [ ] Range-select on an operand that is neither integral nor an unpacked array (LRM 7.4.6) is
        rejected.
- [x] W6 -- Selector lvalue (write side, including NBA + selector). LRM 11.5.1 write rules
      (fully-OOB no-op, partial-OOB only in-range bits, X / Z position no-op) all hold, so the
      indexed part-select surface is complete on the write half.
- [x] W14 -- A selector whose bound or index comes from a differently typed expression reads
      correctly: a range-select `v[$bits(t)-1:1]` mixing a 4-state and a 2-state bound, or a
      bit-select on a non-zero-based range with a 4-state index. The selector lowering carries only
      the source-level selection; the selected value resolves a source coordinate to a storage
      position in its own wide, X/Z-preserving domain (decisions/selector-coordinate-resolution.md),
      so no coordinate arithmetic is synthesized on the selector's own -- possibly narrow, possibly
      four-state -- type, and no runtime comparison between the two bounds is emitted. Unblocks
      `$bits` in a part-select bound and the `ibex_top` parity reduction.

### Construction

- [x] W8 -- Concatenation read `{a, b, c}` (LRM 11.4.12). Result is unsigned; 4-state iff any
      operand is 4-state. Signed operands contribute raw bits; unsized literals are rejected at the
      frontend. Includes mixed widths, nested concat, signed operands, wide (>64-bit) results, and X
      / Z propagation.
- [x] W9 -- Replication `{N{x}}` (LRM 11.4.12.1). Multiplier is a constant non-negative non-X/Z
      literal. Zero multipliers are legal only inside a concat with at least one positive-sized
      sibling; the frontend's `void`-typed operand handling drops them naturally. Includes
      replication in concat, nested concat, nested replication, signed operands, wide results, and X
      / Z propagation. String replication lives in `datatypes.md` SC2.
- [x] W10 -- Destructuring assignment `{a, b, c} = rhs` / `<= rhs` (LRM 11.4.12 LHS form). RHS is
      evaluated once and bits are distributed MSB-first, so `{a, b} = {b, a}` swaps. Parts may be
      any writable lvalue (including W6 selector chains, e.g. `{a[7:4], b[7:4]} = rhs`). NBA form
      requires every part to be a structural target. Replication operands are rejected per LRM
      11.4.12.1. The LRM 10.9 assignment-pattern LHS is a separate construct and out of scope; the
      LRM 11.4.14.3 streaming-unpack LHS is W15 below.
- [x] W15 -- Streaming operators `{>> {...}}` / `{<< n {...}}` in both directions (LRM 11.4.14).
      Packing lays each operand's bits end to end with the first operand most significant, an
      unpacked array contributing its elements in `foreach` order and a structure its members in
      declaration order; `<<` then reverses the order of slice-sized blocks taken from the least
      significant bit up, leaving a short final block unpadded, while `>>` re-orders nothing and
      ignores any slice size. As an assignment source the stream is left-aligned in a wider
      fixed-size target by zero-filling on the right; as a target it is consumed from its most
      significant end and any surplus at the other end is dropped. Covers both directions in
      procedural and continuous-assignment position, the nonblocking form, a nested stream, a slice
      size written as a type or a constant, and a `with` clause naming which elements of a
      one-dimensional unpacked array take part (LRM 11.4.14.4) on either side of the assignment, in
      every range form a select admits.

  The four forms below are rejected, and they are not one gap. Each says what it waits on.
  - [ ] A dynamically sized value wherever it meets a stream -- packed into one (LRM 11.4.14.4),
        filled from one, or standing as the source an unpack consumes. What it waits on is a type
        naming a run of bits whose length the program fixes: the runtime already holds a packed
        value's width on the value rather than in its type, so the value side is ready and the type
        side is not. This is the only one of the four whose answer changes what is already built,
        because the stream's own type is what would move.
  - [ ] A class handle as a stream expression (LRM 11.4.14.1 streams the object's data members in
        declaration order, a base's before a derived's). What it waits on is a traversal over an
        object rather than over a value: a class object is reached through a managed reference and
        has no value-layer representation, so the product traversal every other aggregate shares
        does not reach it.
  - [ ] A streaming target on a continuous assignment. The concatenation target at that position
        does lower, because a packed concatenation of lvalues is itself one place; a stream is not,
        so it needs the distribution into several targets that the procedural form performs. What it
        waits on is that distribution becoming one component the destructuring assignment, the
        streaming unpack, and the continuous assignment all reach.
  - [ ] An unpacked union anywhere in a stream. LRM 11.4.14.1 streams its first-declared member
        whatever member is live, and the front end already resolves that much. What blocks it is the
        union's own storage: this pipeline keeps only the active member and reports a read of any
        other, so the first member's bits are not there to read when a later member is live --
        streaming a union needs the member overlay the storage model does not have. Worth knowing
        before it is taken up: LRM 6.24.3 sizes a union by its largest member while 11.4.14.1
        streams its first, and for `union { byte narrow; int wide; }` those are 32 bits and 8.

### Assignment families

- [x] W11 -- Compound assignment `+= -= *= /= %= &= |= ^= <<= >>= <<<= >>>=` (LRM 11.4.1), with the
      operation sized and signed by the ordinary expression rules. A nonblocking compound assignment
      is rejected at parsing per LRM A.6.2. Complete for whole-variable, selector, and mixed-state
      target shapes.
- [x] W12 -- `++` / `--` (prefix and postfix, LRM 11.4.2). Behave as blocking assignments; postfix
      yields the operand's prior value, prefix yields the new value. Integer and real operands;
      selector chains (`array[i]++`, `++a[15:8]`) and observable structural roots are covered. NBA
      contexts (`b <= a++`, `var[i++] <= rhs`) evaluate the inc / dec exactly once at submit time.
      Replication / concatenation operands are rejected as targets per LRM 11.4.12.1.
- [x] W13 -- Compound assignment evaluates the left-hand side exactly once (LRM 11.4.1) for every
      target, including a side-effecting subscript (`a[f()] op= b`) at any nesting. It holds for
      every target shape -- a whole variable, an element of an unpacked array or a queue, a bit or
      part select, a struct or union member, a string character -- and for every operator the clause
      admits, the shifts included.

## Cross-references

- LRM anchors: 7.4.5 (indexing and slicing: vocabulary, invalid-index rules), 11.4.5 (case
  equality), 11.4.6 (wildcard equality), 11.4.10 (shift), 11.4.12 / 11.4.12.1 (concatenation,
  replication), 11.4.13 (set membership), 11.5.1 (bit-select / part-select / indexed part-select on
  packed).
- Unblocks: `packed.md` P3..P5, which extend the addressable-expression set with packed-struct field
  access and packed-union variant access.
