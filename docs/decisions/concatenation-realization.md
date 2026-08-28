# A join means one operation, and reaches the machine as a chain

## Date

2026-08-27

## Status

Accepted. Supersedes the packed-and-string half of [queue-operators](queue-operators.md) decision 3;
the queue half of that decision stands unchanged.

## Context

`{a, b}` and `{n{a}}` (LRM 11.4.12) mean different operations depending on what they join. Over
packed operands they compose one bit plane. Over strings they compose characters, and the standard
even gives the two forms different rules: 11.4.12.1 requires the replication multiplier to be a
constant expression, while 11.4.12.2 allows a non-constant one and says the destination is resized
rather than truncated. Over an unpacked queue it is a third thing again, already settled.

MIR carried one node for the first two and left each consumer to pick which by reading the result
type. The C++ backend did:

```
if (result is packed) return "PackedArray::Concat({...})";
if (result is string) return "(a + b)";
```

That is the shape `backend_contract.md` invariant 2 names as the canonical render-side defect -- an
`if` in a value-emission entry whose arms produce different syntactic shapes -- and the same doc
says what it means: the missing statement is upstream, and any second backend meets the same
obstruction. It did. MIR-to-LIR could write no arm for the node at all, and every case that reached
one stopped there.

The replication node under-stated a second thing. Its multiplier was an operand, so the C++ render
read the operand's type to decide whether to insert a `.ToInt64()` -- a value reshape invented at
render, which `mir.md` requires to be a stated call.

## Decisions

1. **A join primitive means one operation: composing packed operands into one bit plane.** A join
   over any other operand family is a different operation and is stated as a call against the entry
   that performs it -- a string join composes characters, a queue join carries an element shape and
   spread semantics (unchanged from the decision this supersedes). No consumer picks a realization
   by reading the result type, so a mechanical backend can translate the node without deciding
   anything.

2. **A join carries two or more runs.** A source-level join of one operand is that operand seen at
   the join's own type, which LRM 11.8.1 fixes as unsigned however the operand was declared -- a
   conversion, not a join. HIR-to-MIR states it as one, so the degenerate case exists in one place
   instead of at every consumer. This is what lets the word "join" be true of the node.

3. **A replication's multiplier is a count on the node, not an operand.** LRM 11.4.12.1 makes it a
   constant expression, so the front end has already folded it and nothing at run time evaluates it.
   The string form keeps an operand, because 11.4.12.2 allows a non-constant multiplier there; that
   form is a call, and reading the multiplier as a machine count is a stated conversion in the
   argument list rather than one a backend inserts.

4. **An N-ary value build reaches the machine as a chain of two.** A C ABI names one entry per
   arity, and an operand list of arbitrary length has no arity, so MIR-to-LIR folds the join into
   two-run joins. Joining is associative over both the bit plane and the state domain -- total width
   is a sum, the result holds x and z when any run does, and each run lands at the same offset
   either way -- so the chain and the single N-way join hold the same value. Every step but the last
   reaches a width no declaration names, and MIR-to-LIR mints that type, which is its own to mint.

## Why the superseded decision does not survive

[queue-operators](queue-operators.md) decision 3 reads: "A packed or string `{...}` is the
`ConcatExpr` primitive realized per result type -- a bit join or a string join -- whose result shape
is carried entirely by the result type, so the operands are joined directly."

The premise is true and is kept: the result _shape_ is carried entirely by the result type, and
nothing here puts a shape payload back on the node. What does not follow is the conclusion. That a
node's result shape comes from its type says nothing about how many _operations_ the node stands
for, and "realized per result type" is precisely the per-consumer decision the backend contract
forbids. The entry was written while settling the queue case, where the argument was carried by what
a queue construction needs; the packed-and-string half rode along without being tested against the
render it implied.

## Rejected

- **Keep one node and give the backend a fourth entry kind** -- a per-type dispatch for "which
  library entry realizes this operation", sibling to type mapping and place access. It would make
  the C++ render legal again, but it adds contract surface to hold a fact the node could simply
  state, and it leaves MIR-to-LIR selecting a runtime entry from a result type with no receiver to
  read it from. A node that says what it is costs nothing and needs no new entry kind.

- **Make the node binary at MIR.** It removes the fold entirely, and the intermediate widths would
  be interned where the type knowledge already is. Rejected on emitted readability, which
  `compiler_overview.md` makes load-bearing: `{a, b, c, d}` would render as three nested calls in
  the surface a developer reads to check that MIR matches the source. The arity limit is a property
  of the machine, so it is paid where the machine appears.

- **Collect the runs into a sequence value and pass one span.** This is how the variadic runtime
  entries that already exist take their operands, and it needs no fold. It does not fit a join: a
  sequence is homogeneous and a join's runs have different widths, so the sequence's element type
  would have to name one of them and be wrong about the rest. Strings would fit, but splitting the
  two forms across two mechanisms buys nothing over one fold that serves both.

- **Fold at HIR-to-MIR instead.** Same result with no MIR-to-LIR work, and it was taken for the
  string form, whose runs carry no width to track. Rejected for the packed form because it moves
  arity -- a machine fact -- into the semantic layer and costs the same emitted readability as
  making the node binary.

## Consequences

- The C++ render of both nodes is a single form with no branch, and the execution backend realizes
  both: 19 conformance cases the record listed as refused now run on it.
- A string join and a string replication are ordinary calls, which is what every other string
  operation already was.
- One runtime entry serves each operator across value domains, because the domain a call's operands
  name is what selects the realization -- the identity rule `builtin-call-identity.md` already
  states, now reaching a value-build.
- A consumer that meets a join reads two or more runs and never a degenerate one.

## Cross-references

- `architecture/backend_contract.md` -- invariant 2 (decision logic in a value-emission entry is a
  MIR design failure) and the LLVM-backend cross-check that predicted this.
- `architecture/mir.md` -- invariant 10 (a backend reads a stated fact and never re-derives one);
  the Notes on why value-build primitives stay in the expression set.
- `architecture/lir.md` -- MIR-to-LIR owns arity flattening and mints its own types.
- [queue-operators](queue-operators.md) -- decision 3, whose packed-and-string half this replaces.
- [builtin-call-identity](builtin-call-identity.md) -- one flat entry per runtime function, with the
  receiver's type carrying the type-side context.
- LRM 11.4.12 (concatenation), 11.4.12.1 (replication, constant multiplier), 11.4.12.2 (string
  concatenation and replication, non-constant multiplier), 11.8.1 (a concatenation is unsigned).
