# A join is a call, and reaches a fixed-arity entry as a chain

## Date

2026-08-27, revised 2026-08-31

## Status

Accepted. Supersedes the packed-and-string half of [queue-operators](queue-operators.md) decision 3;
the queue half of that decision stands unchanged. The 2026-08-31 revision reverses this record's own
first decision, which made the packed form a MIR node; the reasoning it replaces is kept below.

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

1. **A join is a call against the entry that performs it, over every operand family.** Which
   operation a source-level `{...}` means follows its operands -- composing bits, characters, or
   elements is three operations -- and the layer holding the operands states it by naming the entry.
   No consumer picks a realization by reading a result type, and no consumer needs a node kind of
   its own to recognize.

   MIR's expression set is closed under what a generic programming-language AST needs to express,
   and none of MIR's peers has a concatenation node: Rust, C++ and Python all reach concatenation
   through a library call, and none of them has bit concatenation at all. A node here would be the
   source language's syntax shaping MIR's vocabulary, which `mir.md` forbids outright. An aggregate
   _literal_ is the opposite case and stays a node -- Rust and C++ both have one, because a literal
   spells a value rather than operating on values that already exist.

2. **A join carries two or more runs.** A source-level join of one operand is that operand seen at
   the join's own type, which LRM 11.8.1 fixes as unsigned however the operand was declared -- a
   conversion, not a join. HIR-to-MIR states it as one, so the degenerate case exists in one place
   instead of at every consumer.

3. **A replication's multiplier reaches the entry as a machine count.** LRM 11.4.12.1 makes it a
   constant expression over packed operands, so the front end has already folded it and HIR-to-MIR
   states the folded value as a literal argument. LRM 11.4.12.2 allows a non-constant multiplier
   over a string, so that form evaluates an integral expression and states the reshape to a machine
   count as a call in the argument list. Where the count comes from is the only difference between
   the two, and neither leaves a backend anything to insert.

4. **A join reaches MIR already folded to the arity its entries take.** Nothing composes an operand
   list of arbitrary length: a C ABI has no such entry, and neither does the runtime's C++ surface.
   So an N-operand source join is stated as the left-to-right chain of two-operand calls it stands
   for. Composing is associative over both the bit plane and the state domain -- total width is a
   sum, the result holds x and z when any run does, and each run lands at the same offset either way
   -- so the chain and the single N-operand call hold the same value.

   The fold happens where the join is built, because arity is not one target's fact: every entry
   that performs the operation takes two operands. A backend that folded instead would be deciding,
   in a value-emission entry, how many operations one node stands for -- the render-side defect this
   record opened with, reached from the other end. Each step's type follows from the runs it has
   joined so far, so the entry that performs the join is what fixes it and no caller states it.

5. **One entry, one call shape across value families.** Which realization a join reaches follows the
   value domain of its operands, and each backend answers that its own way: the execution backend
   derives the ABI symbol from the domain, and the C++ backend has one spelling per entry and lets
   the host compiler's overload resolution pick. That second form only holds if the runtime presents
   the same call shape for every family it realizes -- so a join is an instance method on each value
   type, as every other binary value operation already is. A family whose entry is spelled
   differently is not a runtime detail: it is a backend that cannot render the entry at all.

## Why the superseded decision does not survive

[queue-operators](queue-operators.md) decision 3 reads: "A packed or string `{...}` is the
`ConcatExpr` primitive realized per result type -- a bit join or a string join -- whose result shape
is carried entirely by the result type, so the operands are joined directly."

The premise is true and is kept: the result _shape_ is carried entirely by the result type. What
does not follow is the conclusion. That a node's result shape comes from its type says nothing about
how many _operations_ the node stands for, and "realized per result type" is precisely the
per-consumer decision the backend contract forbids.

## Why this record's own first decision did not survive

The 2026-08-27 text read: "A join primitive means one operation: composing packed operands into one
bit plane. A join over any other operand family is a different operation and is stated as a call
against the entry that performs it."

Its argument was that one node realized per result type is the render-side defect, and that
splitting the families fixes it. The argument is sound and is kept; the conclusion drawn from it is
not. Making all three families calls splits them just as completely -- each names its own entry, and
no consumer reads a result type -- so nothing in the argument reaches "and the packed one is a
node". The asymmetry was assumed rather than derived, and it cost what the record's own first
rejected alternative predicted it would.

## Rejected

- **Keep one node and give the backend a per-type dispatch for "which library entry realizes this
  operation"**, sibling to type mapping and place access. Rejected in the original record on the
  ground that it "adds contract surface to hold a fact the node could simply state". The prediction
  held: with the packed form a node and every other operation a call, the C++ backend grew exactly
  that dispatch, plus a two-valued selector recording that one of its two entries took a braced list
  while the other took an argument list. Making the join a call deletes all of it -- the existing
  builtin-call render already names the entry.

- **Collect the runs into a sequence value and pass one span.** This is how the variadic runtime
  entries that already exist take their operands, and it needs no fold. It does not fit a join: a
  sequence is homogeneous and a join's runs have different widths, so the sequence's element type
  would have to name one of them and be wrong about the rest.

- **Leave the fold to each backend.** Rejected, and it is what this record first decided, on the
  ground that the arity limit is one target's property so it should be paid where that target is.
  The premise was wrong: both realizations take two operands, so there is no target the limit is
  specific to. What the fold costs where a backend does it is a value-emission entry that reads how
  many operands a call carries and decides whether the operation is associative enough to chain --
  decision logic in render, which is what this whole record exists to remove. The price of the
  correct side is emitted readability, which `compiler_overview.md` makes load-bearing:
  `{a, b, c, d}` reads as three nested calls in the surface a developer checks MIR against. That is
  a real cost and it is paid.

## Consequences

- The C++ render of a join and of a replication is the ordinary builtin-call render, which already
  names both entries. The dispatch that existed to name them, and the operand-grouping selector
  inside it, are gone.
- MIR-to-LIR has no arm for either operation and mints no intermediate type: a call carries its
  operands, and every step of a folded chain is already a call.
- A string join and a string replication are ordinary calls, which is what every other string
  operation already was.
- One runtime entry serves each operator across value domains, because the domain a call's operands
  name is what selects the realization -- the identity rule `builtin-call-identity.md` already
  states, now reaching a value-build.
- A consumer that meets a join reads two or more runs and never a degenerate one.

## Cross-references

- `architecture/mir.md` -- the Purpose paragraph (MIR's peers are C++, Rust and Python, and the
  source language does not shape its vocabulary); the expression set is closed under what a generic
  programming-language AST needs to express.
- `architecture/backend_contract.md` -- invariant 2 (decision logic in a value-emission entry is a
  MIR design failure) and invariant 7 (no contract entry is specialized for one backend).
- [queue-operators](queue-operators.md) -- decision 3, whose packed-and-string half this replaces.
- [builtin-call-identity](builtin-call-identity.md) -- one flat entry per runtime function, with the
  receiver's type carrying the type-side context.
- LRM 11.4.12 (concatenation), 11.4.12.1 (replication, constant multiplier), 11.4.12.2 (string
  concatenation and replication, non-constant multiplier), 11.8.1 (a concatenation is unsigned).
