# A join is stated as the program a person writes for it

## Date

2026-08-27, revised 2026-08-31 and 2026-10-21

## Status

Accepted. Supersedes [queue-operators](queue-operators.md) decision 3. The 2026-08-31 revision
reverses this record's own first decision, which made the packed form a MIR node; the 2026-10-21
revision reverses decisions 2, 4 and 5, which made a join a call of two operands and folded a longer
one into a chain of such calls. The reasoning each replaces is kept below.

## Context

`{a, b}` and `{n{a}}` (LRM 11.4.12) mean different operations depending on what they join. Over
packed operands they compose one bit plane. Over strings they compose characters, and the standard
even gives the two forms different rules: 11.4.12.1 requires the replication multiplier to be a
constant expression, while 11.4.12.2 allows a non-constant one and says the destination is resized
rather than truncated. Over an unpacked queue it is a third thing again (LRM 10.10).

MIR carried one node for the first two and left each consumer to pick which by reading the result
type. The C++ backend did:

```
if (result is packed) return "PackedArray::Concat({...})";
if (result is string) return "(a + b)";
```

That is the shape `backend_contract.md` invariant 2 names as the canonical render-side defect -- a
value-emission branch whose arms differ in what the program does, bits composed one way or
characters another -- and the same doc says what it means: the missing statement is upstream, and
any second backend meets the same obstruction. It did. MIR-to-LIR could write no arm for the node at
all, and every case that reached one stopped there.

The replication node under-stated a second thing. Its multiplier was an operand, so the C++ render
read the operand's type to decide whether to insert a `.ToInt64()` -- a value reshape invented at
render, which `mir.md` requires to be a stated call.

## Decisions

1. **Which operation a join is follows its operands, and the layer holding them states it.**
   Composing bits, characters, or elements is three operations, and HIR-to-MIR, where the operand
   family is known, writes each as what it is. No consumer picks a realization by reading a result
   type, and no consumer needs a node kind of its own to recognize.

   MIR's expression set is closed under what a generic programming-language AST needs to express,
   and none of MIR's peers has a concatenation node. A node here would be the source language's
   syntax shaping MIR's vocabulary, which `mir.md` forbids outright.

2. **A join of strings of one part is that part.** A source-level join of one string is the string
   it names. A join of bits needs no such case: one part is one write.

3. **A replication's multiplier reaches the entry as a machine count.** The multiplier is lowered as
   the expression the source wrote and reshaped to a machine count by a call in the argument list,
   so a backend has nothing to insert. LRM 11.4.12.1 requires a constant expression over packed
   operands and 11.4.12.2 admits a non-constant one over a string, and that difference reaches
   nothing here: a constant multiplier is an expression that happens to have a known value, so both
   take the one path, and the entry cannot tell which it was handed.

   This entry used to say the packed form arrives already folded and is stated as a literal
   argument, with the string form as a second path beside it. Neither half held. Nothing in either
   lowering reads what an expression settled to, so there was one path all along -- and the sentence
   was reasoning from the clause to what the compiler must be doing, which is the inference
   `an-elaboration-time-value-is-an-input` exists to stop.

4. **Each join is what a person writes for it by hand, and stands as flat for thirty thousand parts
   as for two.** Every consumer walks an expression by its nesting and a target compiler bounds how
   deeply text may nest, so a list the source writes is never stated one level per part.
   - **Bits** are a local of the joined type and one write of each part into the bits it takes, in
     the order the source lists them: `t = 0; t[55:48] = a; t[47:32] = b; t[31:0] = c`. Each part's
     width is its type's (LRM 11.4.12 bans unsized constants "because the size of each operand is
     needed"), so where it lands is a constant of the program and nothing about it reaches the run.
     A write into bits is the existing write through the reference a packed value's `slice_mut`
     answers.
   - **Strings** are one call over one list of strings, sized once and each part copied once. Every
     part is of one type and its length is known only at run time, which is the condition every
     peer's list-taking join has.
   - **Queue elements and queues** (LRM 10.10) are a queue the steps append each part to -- the part
     itself, or every element of a part that spreads -- and where the destination is not a queue, it
     takes what was appended as an assignment between array kinds does.

## Survey

- clang (`clang/lib/CodeGen/CGExprAgg.cpp`, `AggExprEmitter::VisitInitListExpr`) keeps an
  initializer list as one flat `InitListExpr`, and code generation loops over the record's fields
  storing each initializer into its field, a bit-field by a store into its bits. rustc
  (`rustc_codegen_ssa/src/mir/rvalue.rs`, `Rvalue::Aggregate`) keeps one flat operand list in MIR
  and stores each operand into its field in a loop. LLVM's `APInt` (`llvm/ADT/APInt.h`) has a
  two-operand `concat`, and many parts are written by `insertBits(SubBits, bitPosition)` into a
  value sized up front. None of them hands a list of parts to a library.
- Both compilers make the per-part store in code generation because their only target is machine
  code. One of Lyra's two targets is C++ source, which has no syntax placing parts of different
  widths into one bit vector: its only spellings of a call over such parts are a type-erased list,
  which hands the run the widths the types fix, and a parameter pack, which clang bounds by the
  bracket depth for a fold expression (`TreeTransform.h`, `err_fold_expression_limit_exceeded`) and
  by 2^16 for a parameter list (`TypeBase.h`, `FunctionTypeNumParamsWidth`). So MIR states the
  stores, both targets translate them, and the execution backend does with them what clang does with
  the C++ rendering.
- abseil's `StrCat` hands `CatPieces` one list of views, sized once; Rust's `[a, b].concat()` takes
  a slice; CPython compiles `f"{a}{b}"` to one `BUILD_STRING`. CPython builds `[a, *b, c]` by
  `LIST_APPEND` and `LIST_EXTEND` into one list.

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
not. Stating each family as what it is splits them just as completely, so nothing in the argument
reaches "and the packed one is a node".

## Rejected

- **Keep one node and give the backend a per-type dispatch for "which library entry realizes this
  operation"**, sibling to type mapping and place access. Rejected in the original record on the
  ground that it "adds contract surface to hold a fact the node could simply state". The prediction
  held: with the packed form a node and every other operation a call, the C++ backend grew exactly
  that dispatch, plus a two-valued selector recording that one of its two entries took a braced list
  while the other took an argument list.

- **A bit join as a call over every part, each described to the library.** An entry taking a list
  and its count names any arity, but what it takes is a description of each part -- where it lies,
  how wide it is, whether it can hold x or z -- built at every call for facts the operand types had
  already fixed; the C++ side of it was a type-erased part per operand in a braced list. Both hand
  the run what the program had settled, the description-the-runtime-walks shape clang never takes,
  and neither is what a person writes for the same join.

- **Fold the join into the chain of two-operand calls it stands for.** This record's decisions 4 and
  5 until 2026-10-21, on the ground that "a C ABI has no such entry, and neither does the runtime's
  C++ surface". The chain nested one level per operand, so a concatenation of thirty thousand ended
  Lyra by stack overflow in a MIR rewrite and ended clang on the C++ backend; each step also made a
  value as wide as everything joined so far, so the work was the square of the parts.

- **A join of one operand set apart as a conversion before any join is built.** This record's
  decision 2 until 2026-10-21, so that no consumer met a one-operand join. A join of bits is one
  write per part, which one part does not change, so the case need not exist there; a join of
  strings keeps it, as decision 2 says, because one part is already the string.

- **Collect a bit join's operands into a sequence value and pass one span**, the parts as MIR
  values. A sequence is homogeneous, so the element type would have to name one width and be wrong
  about the rest. A join of strings is exactly this, because its parts are of one type.

- **Leave the fold to each backend.** Rejected, and it is what this record first decided, on the
  ground that the arity limit is one target's property so it should be paid where that target is.
  What the fold costs where a backend does it is a value-emission entry that reads how many operands
  a call carries and decides whether the operation is associative enough to chain -- decision logic
  in render, which is what this whole record exists to remove.

## Consequences

- The C++ render of a string join and of a replication is the ordinary builtin-call render, and a
  bit join renders as the local and the writes it is; no render entry knows a join exists.
- MIR-to-LIR has no arm for a bit join and mints no intermediate type: it lowers the writes as it
  lowers every write into bits.
- A join of thirty thousand parts builds on both backends, its MIR as deep as a join of two.
- An unoptimized build pays a store per part of a narrow join; an optimizing one merges them.

## Cross-references

- `architecture/mir.md` -- the Purpose paragraph (MIR's peers are C++, Rust and Python, and the
  source language does not shape its vocabulary); bits of a packed value are written through the
  reference its `slice_mut` answers.
- `architecture/backend_contract.md` -- invariant 2 (decision logic in a value-emission entry is a
  MIR design failure) and invariant 7 (no contract entry is specialized for one backend).
- [queue-operators](queue-operators.md) -- decision 3, which this replaces.
- [a-list-the-source-writes-stands-side-by-side](a-list-the-source-writes-stands-side-by-side.md) --
  every list the source writes, of which a join's parts are one.
- LRM 11.4.12 (concatenation), 11.4.12.1 (replication, constant multiplier), 11.4.12.2 (string
  concatenation and replication, non-constant multiplier), 11.8.1 (a concatenation is unsigned),
  10.10 (unpacked array concatenation).
