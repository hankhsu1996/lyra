# Arena reference lifetime: a pool never moves what it holds

Date: 2026-06-26 Status: accepted. Revised 2026-10-17: the pool's storage no longer moves, which
reverses the first decision; the id-first model it protected is unchanged.

## Context

A lowering builds an IR pool and reads it in the same steps: it adds a node, reads the type of one
it built a moment ago, and adds another. The pools (`Arena`, and `Registry`, `Interner` and
`SymbolTable` over the same storage, from
[generic-lowering-machinery](generic-lowering-machinery.md)) hand out a node as
`Get(id) -> const T&`. Two questions follow, and they are separate.

**What names a node.** North star invariants 4 and 5 make incremental and per-unit compilation
first-class, so whatever names a node has to survive leaving the process: it is hashed into keys,
cached, and read by another unit. An address does not survive that and an index does. So a node is
named by its id, and a node naming another holds that id.

**Whether the storage moves when it grows.** Nothing about naming decides this. It decides only
whether the reference `Get` returns is still good after an `Add`.

The first decision tied the two together. It kept the pool a `std::vector`, ruled that an `Add`
invalidates every reference a prior `Get` returned, and asked each lowering to copy what it needed
before adding. It was derived from the sites inspected on 2026-06-26, where every fact needed past
an `Add` was a small one that could be copied out, and it rejected storage that does not move
because "no site requires" a reference that survives an `Add`, and because such storage "would
invite treating a live `Get` reference as a passable, savable identity".

## What the first decision cost

C++ cannot refuse a reference read after the storage under it moved, so the rule was carried by
review and, from 2026-09-11, by a nightly run under the address sanitizer, which moved the storage
on every `Add` so that any such read in a run it reached was reported.

- Between 2026-09-11 and 2026-10-17 eight such reads stood on `main` until a nightly or a
  measurement found them, and a sweep on 2026-10-17 found four more the sanitizer had never
  reported. It reports only on paths the corpus takes and only for an `Add` that actually grows the
  pool there: a type already interned adds nothing, and a value that is not a place leaves a local
  declaration appending nothing.
- Several were not a reference anyone meant to hold. `f(Append(), pool.Get(id).type)` copies a
  `TypeId` in one argument while another appends. GCC 14 was measured taking the address of the
  field, evaluating the sibling argument, and copying from the old address only then, so the read is
  freed storage under GCC and sound under clang, which evaluates first to last. Lyra is built by
  both. The rule was being broken by ordinary expression composition, not by code that wanted to
  keep a node.
- No C++ tool closes the gap. clang's `-Wlifetime-safety-invalidation` analyzes one function at a
  time, knows only standard containers' methods by name, and is experimental; an append made inside
  a callee, which is where every one of the twelve was, is invisible to it. Crubit's lifetime
  analysis checks no exclusivity, which is the half that finds this. A borrow checker for C++ (Safe
  C++, P3390) is not being pursued in the standard.

## What established compilers do

| System          | Storage                                                                                                                  | What names a node                            | Why a read during construction is sound                                                                                                    |
| --------------- | ------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| rustc bodies    | `IndexVec { raw: Vec<T> }`, moves (`rustc_index/src/vec.rs`)                                                             | index                                        | the borrow checker refuses the read; a builder reads a node's type off the value before pushing it (`rustc_mir_build/src/thir/cx/expr.rs`) |
| rustc types     | `rustc_arena`, chunks that "each new chunk is twice as big as its predecessor" and never move (`rustc_arena/src/lib.rs`) | `Ty<'tcx>`, an address valid for one session | storage never moves                                                                                                                        |
| clang           | `llvm::BumpPtrAllocator` in `ASTContext`                                                                                 | address                                      | storage never moves                                                                                                                        |
| slang           | `BumpAllocator`, blocks never move                                                                                       | address                                      | storage never moves                                                                                                                        |
| WebKit, Mozilla | `WTF::SegmentedVector`, `mfbt::SegmentedVector`: a vector that adds segments and never moves an element                  | index or address                             | storage never moves                                                                                                                        |

Naming and storage are independent in the field too. The C++ compilers keep storage still and name
nodes by address, because their graph lives in one process. rustc names by index where a handle must
outlive the session, and lets a body pool move only because its language refuses the read that would
make that unsound. Lyra needs rustc's naming, for the north star's reasons, and has C++'s language,
so it takes rustc's naming and C++'s storage.

## Decision

1. **The id is the only thing that names a node.** A node naming another holds its id, and anything
   kept past the lowering step that read it is an id or a value of its own, never a reference.
   Unchanged from the first decision, and still the reason pools are indexed at all.
2. **A pool never moves what it holds.** Growing a pool leaves every element where it is, so a
   reference `Get` returned stays good for the pool's life, across any `Add`, `Declare` or `Intern`.
   The storage is a segmented vector whose segments double in size, as `rustc_arena`'s chunks do, so
   a small pool stays small and a large one is a few dozen allocations. A table a unit fills whole
   before anything reads it -- what MIR and LIR record of other units' classes, for one -- never
   grows under a reader, and stays a plain vector.
3. **A reference from `Get` reads a node and never names one.** What the first decision called a
   transient view is now a read that is sound wherever it appears, which is what makes ordinary
   expression composition safe. It is still not a handle: holding a reference where an id belongs is
   the same mistake it was, and nothing about the storage catches it, in either form -- a node field
   `const Expr&` compiled over the moving pool exactly as it does now, and failed only later and
   only sometimes.
4. **A node's value is final once added.** Unchanged: other nodes name it by id, and a later shard
   caches it without coordination.

## Rejected

- **Keeping a moving pool and the rule to copy before adding.** It rests on a rule C++ cannot check
  and that ordinary code breaks without meaning to; twelve sites in five weeks measured that.
- **Counting borrows at run time** (a `Get` returns a guard, and an `Add` with one alive is an
  internal error, as Rust's `RefCell`). It reports every reached execution on both compilers, and it
  still only detects a defect the storage can remove outright, at a cost on every read in every
  stage, while flagging sound code whose guard happens to outlive the copy.
- **Value-returning accessors for the common facts** (`TypeOf(id)` and a check refusing
  `exprs.Get(...).type`). Worth having where they read better, but they cover one field of one pool,
  and two of the twelve sites were references into the type pool.
- **`std::deque`.** Its references survive `push_back`, but libstdc++ allocates a map and a 512-byte
  block for a deque that holds nothing, again for one moved from, and an allocation per element for
  a node larger than the block (`bits/stl_deque.h`); most pools are tiny, and many nodes are large.
- **`Get` returns by value.** Copies a large node on every read when most reads touch one field.

What the first decision gave for rejecting storage that does not move is answered above: twelve
sites needed the capability it said no site required, and a moving pool never stopped anyone storing
a reference (decision 3).

## Consequences

- `Arena`, `Registry`, `Interner` and `SymbolTable` share one storage that never moves.
- The sanitizer nightly no longer moves a pool on every `Add`; it goes on running the merge gate's
  set for every other kind of memory error.
- A walk over a pool that appends to it goes by index, as over a vector: an iterator holds the
  segment table, which an append that opens a segment can move, and the sanitizer sees such a walk
  only when one does.
- Copies a lowering made only so that a fact survived an `Add`, and the comments that explained
  them, are gone; a builder still reads a fact off a value it holds where that is the natural shape.

## Cross-references

- [generic-lowering-machinery](generic-lowering-machinery.md) (the pools this governs).
- `architecture/north_star.md` invariants 4 and 5 (why a node is named by an id).
- `architecture/identity_and_ownership.md` (local entities are referenced by typed id).
- `architecture/incremental_build.md` (caching keys are ownership-based; an address is never a key).
