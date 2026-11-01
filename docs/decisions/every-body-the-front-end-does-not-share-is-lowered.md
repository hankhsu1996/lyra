# Every Body The Front End Does Not Share Is Lowered

## Date

2026-10-31

## Status

Accepted. Revises [specialization-identity](specialization-identity.md) decisions 1, 6 and 7 and
findings F4 and F6,
[a-unit-is-the-specializations-that-publish-alike](a-unit-is-the-specializations-that-publish-alike.md)
D1, D6's naming of a body, D8 and D9,
[a-unit-is-compiled-to-one-body-per-distinct-code](a-unit-is-compiled-to-one-body-per-distinct-code.md)
D1, D3's naming of a body and D6,
[the-lowering-reads-an-instance-only-through-its-identity](the-lowering-reads-an-instance-only-through-its-identity.md)
D4 and D5,
[a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md)
D3,
[a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
D2's check of the compile against itself, and
[the-front-end-has-one-reader](the-front-end-has-one-reader.md)'s rejection of lowering a duplicate
instance through the body it duplicates. What those records say of a class -- the specializations
whose published structure reads alike, named by the definition and the digest of what it publishes,
which this record calls a unit -- and of a body -- the lowerings of one unit whose HIR is equal,
compared whole -- stands. What they say of a key worked out before lowering, deciding that one
instance may run code lowered from another's body, is gone.

## Requirement

Every instance runs the code its own elaboration states, and two instances share compiled code only
where what they state comes out equal; nothing worked out before lowering decides that one instance
may run code lowered from another instance's body.

## Facts it rests on

1. The front end elaborates a body for every instance, and where it finds an instance alike an
   earlier one -- the same definition, the same parameter values, interface ports bound to instances
   alike -- it leaves the later instance pointing at the earlier one's body and does not visit its
   own (slang `InstanceCacheKey`, `DiagnosticVisitor::tryApplyFromCache` in
   `source/ast/ElabVisitors.h`). It shares no body a hierarchical name leaves upward, charging such
   a name to every body on the way out (`Compilation::noteUpwardReference`), none a defparam, a bind
   or a configuration reaches, and none whose interface port is bound to such an instance.
2. The front end's sharing has been wrong three times -- an interface port carrying a range bound to
   two interfaces of different widths, a real parameter compared as a number, a subroutine reached
   by an upward name -- and each was a defect of the front end's own, corrected there. Who says
   which elaborations are alike is settled: the front end, which has every scope open at once, and a
   wrong share is its defect, corrected upstream; Lyra keeps no second authority on it (the human,
   2026-10-31).
3. An instance the front end left pointing at another's body may still have its own body elaborated,
   lazily, where a hierarchical name reaches into it from outside. What stands below that body
   carries no pointer, though it elaborated as the pointed-at body did, member for member, under the
   same index the front end gives a member in its scope.
4. Nearly all of a build's time is after HIR: on real designs about one part in twenty is the front
   end and the lowering to HIR together. Lowering every body the front end did not share costs
   almost nothing beside it on real designs (HammerBlade and VeeR-EL2 measured), and a design
   written to be the worst case -- 2048 instances each writing one upward name, so that the front
   end shares none of them -- lowers in two seconds where it took one.
5. A key worked out before lowering, meant to name every fact that can make two bodies elaborate
   differently, was kept right one field at a time, missing four in one week, and each miss had an
   instance run code lowered for another instance, with no error (`specialization-identity` F6's
   lockstep pair is one; `~/wiki` holds the rest).
6. rustc identifies an application as `Instance { def, args }` and a body as that definition's MIR
   under those arguments (`rustc_middle/src/ty/instance.rs`), with no position in a hierarchy in it,
   because a Rust body reaches nothing outside its arguments. A SystemVerilog body reaches out of
   its instance (LRM 23.8, 9.6.2, 23.10.1), so what its elaboration states follows where it stands.
7. C++ and Rust compile an instance per argument set and fold equal code afterwards, by comparing it
   whole after a hash narrows the candidates (lld's identical code folding, LLVM's
   `MergeFunctions`); CIRCT's FIRRTL `Dedup` merges equal modules by content. None of them works
   out, before compiling, that two instances will come out equal.

## Decision

**D1. An instance's body is the one the front end elaborated for it, or the body of the earlier
instance it left the instance pointing at, and an instance standing below a pointed-at instance is
read through the member at the same site under the body pointed at.** The front end's word is taken
as it is given (fact 2); a site is found by the index the front end gave each member on the way down
and its kind, since both bodies elaborated alike (fact 3).

**D2. Every body so reached is lowered, once, for the instance it was elaborated for, under that
instance's environment.** No key is worked out before lowering to say that two instances may share
one lowering, and no instance runs code lowered from a body that is not its own by D1.

**D3. A unit is the lowerings whose published structure reads alike -- what the records this revises
call a class -- and is named by its definition alone where the definition has one unit in the
design, else by the definition and the digest of what the unit publishes.** The name moves only
where a second unit of the definition appears, or where what referrers compile against moves; adding
an instance elsewhere, or editing a body, renames no unit.

**D4. A body is the lowerings of one unit whose HIR is equal, compared whole after a digest of the
text narrows which are compared.** The first lowering of each body, in the design's order, compiles
it; the first body of a unit is named as the unit is, and each later one by the unit's name and
which body of the unit it is, in that order, joined by a character no simple identifier holds (LRM
5.6), so that no design element's name reads as a body of another's.

**D5. A lowering is named by the hierarchical path of the instance it was lowered for, and the
design root's construction tables are stated per lowering, each instance named by the lowering that
serves it.** The name is internal to a run and reaches no symbol: the root's tables and the values
each lowering publishes for other units' reads are keyed by it, and nothing compiled spells it.

**D6. Lowering every instance and comparing is no check of anything.** Every body the front end did
not share is lowered in the build, and a build that shared none of the front end's bodies would run
the same program; the count of bodies a design compiles to is asserted in both directions, one body
where lowerings come out equal and one per lowering where they do not.

## Limits, each with its cause

- An instance the front end shares wrongly runs the pointed-at body's code (D1; fact 2).
- A body is renamed where a new distinct body of its unit comes out earlier in the design's order,
  and what names it -- the root's tables, an emitted file -- is written again (D4). Cause: no naming
  state outlives a build, and a name taken from the body's own content moves on every edit of it,
  which is the common edit.
- A definition's one unit is renamed once, when a second unit of it first appears (D3).
- Every instance a hierarchical name leaves upward is a body of its own at the front end, so N such
  instances are N lowerings before they are found equal (D2; fact 4).
- Whether two bodies the front end did not share come out equal is known only once both are lowered
  (D4; fact 7).

## Survey

- **Which elaborations are alike.** slang's `InstanceCacheKey` and `tryApplyFromCache` (fact 1)
  answer it inside the one component that has every scope open at once; Verilator clones a module
  per parameter set and per instance path a `defparam` lies beneath (`V3Param.cpp`). Where the
  conditions differ: Lyra compiles after the front end has elaborated everything, so the answer is
  already there to read, and a second computation of it is a second authority on one meaning (fact
  2).
- **Identity of an application.** rustc's `Instance { def, args }` (fact 6) and a C++ template
  specialization by its arguments; neither has a position in a hierarchy, because neither language
  has a body that reaches out of its instance. D5 names a lowering by its instance's hierarchical
  path for that reason, and keeps the name inside a run.
- **Sharing equal code.** lld's identical code folding, LLVM's `MergeFunctions` and CIRCT's `Dedup`
  (fact 7) compare after making, and so does D4, on HIR, because lowering to HIR is cheap beside
  everything after it (fact 4).
- **What a query-based build would key on.** rustc memoizes a query per `Instance`; a query of
  Lyra's keyed on an instance's hierarchical path, reading its body and environment, is what D2 and
  D5 leave room for, and what the records this revises would have had to key on a composite nobody
  could keep right (fact 5).

## Rejected

- **A key worked out before lowering, naming every fact that can make two elaborations differ** (the
  records this revises). A third authority on which bodies are alike, beside the front end's and the
  comparison of what was lowered; kept right one field at a time, and each miss ran wrong code
  silently (fact 5).
- **A check of the front end's sharing.** Lowering every pointed-at instance through its own body
  and holding the result to the pointed-at one's. Two authorities on one meaning; the human ruled
  the front end's the authority and its defects upstream's (fact 2).
- **A body named by a digest of its HIR.** Built and taken out the same day: every edit of a body
  renamed it, so the root's tables and the emitted file were written again on the common edit.
- **A body named by the least key among its lowerings, or by the hierarchical path of its first.**
  The first renames the body when an edit hands an instance a smaller key; the second when an
  instance is added earlier in the design, which is the same change D4's ordinal already costs and
  nothing more.
- **A switch that turns the sharing of equal HIR off, so a test can hold the program equal with it
  on and off.** A product option only a test reads; the count asserted both ways (D6) and the
  conformance corpus state the same property.
- **Carrying an upward name as a typed capture of the lowering, as rustc's closure arguments hold
  its captured variables.** Where a lowering should go next, and not this change's: the environment
  already hands the lowering where each name lands, and the shape of what crosses is a subject of
  its own.

## Revises

- [specialization-identity](specialization-identity.md) decision 1 (a key of everything the design
  fixed, deciding which instances share an artifact), 6 (the front end's sharing not read at all)
  and 7 (a name worked out once and kept); F4 and F6 argued for them. What stands: F8's spelling of
  a definition, and decision 2's spelling of a value and a type, which still name a class
  specialization's arguments.
- [a-unit-is-the-specializations-that-publish-alike](a-unit-is-the-specializations-that-publish-alike.md)
  D1 (a specialization is its definition and everything fixed for it), D6's naming of a body for the
  first specialization that came out as it, D8 (every specialization held to its class in a compile
  that checks itself) and D9 (a specialization lowered once for what its objects are built as). D2
  to D7 otherwise stand.
- [a-unit-is-compiled-to-one-body-per-distinct-code](a-unit-is-compiled-to-one-body-per-distinct-code.md)
  D1 (a specialization named by a digest of every value handed), D3's naming of a body for a
  specialization, and D6 (a table per specialization). D2 to D5 otherwise stand.
- [the-lowering-reads-an-instance-only-through-its-identity](the-lowering-reads-an-instance-only-through-its-identity.md)
  D4 (lowering every instance stays a test) and D5's key: an instance is still its body and its
  environment, and the lowering still reads it through nothing else, but the environment is per
  instance and renders no key.
- [a-unit-is-named-by-the-generate-blocks-it-built](a-unit-is-named-by-the-generate-blocks-it-built.md)
  D3's check of the compile against itself.
- [a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
  D2's check of the compile against itself: nothing is lowered a second time to be held to its
  specialization.
- [the-front-end-has-one-reader](the-front-end-has-one-reader.md): its rejection of lowering a
  duplicate through the body it duplicates on the front end's word, which D1 does (fact 2).
