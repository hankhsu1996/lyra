# A lent part carries its variable

Date: 2026-09-28 Status: accepted. Supersedes the representation
[reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) chose (its D1, D2 and D5 and
invariants 1, 2 and 4); its D3 stands.

## Context

LRM 13.5.2 lets a member of an unpacked structure and an element of an unpacked array be passed by
reference, and says that "changes are seen outside the subroutine immediately (before the subroutine
returns)". LRM 4.3 makes such a change an update event on the variable the part belongs to. Two
things did not hold.

- A task that wrote through a `ref` bound to `arr[1]` or `s.b` and then waited woke `@(arr[1])` when
  it returned, not when it wrote, on both backends. The part was lent as the place a write lands on,
  and that write belonged to the caller's full-expression -- the call -- so it reported when the
  call ended. That is Swift's `inout` over an observed property ("its setter is called as part of
  the function return"), which the LRM sentence above is written against.
- On the execution backend a member or element of an automatic variable or a class property could
  not be lent at all. A reference was one word naming the storage and, in a tag bit, whether it was
  a subscribable variable's cell; a part of a variable is neither the whole cell nor storage nobody
  waits on, so no tag could say what a write through it owes.

What the reference was missing is the same in both: the variable the storage belongs to. A class
property is the same case one level out: a write to it is a change of the object it belongs to (LRM
9.4.2), and a property lent for the length of a call reported that change when the call ended.

## Decision

**A reference is where a value lies together with what holds that storage -- a variable, or the
object a property belongs to -- if anyone is told about it; a part is lent by a step taken on a
reference to its whole.**

- **MIR states a lent part as steps on a reference.** A reference is formed over the whole of what
  the source names, and `refer_element` / `refer_component` on a reference answer a reference to
  that part: `poke(Ref(*arr).refer_element(1).refer_component<1>())`. The owner may be a variable,
  an automatic variable, a class property, or the storage a `ref` formal already holds; the steps
  are the same. Each step is evaluated once, at the bind, and resolves the path once (the kept half
  of the record this supersedes). Only an element and a component may be passed by reference (LRM
  13.5.2), so there is no slice step.
- **The reference carries what holds the storage.** A reference is (a variable, an object, or
  nothing; the storage; whether the storage is the whole of the variable); storage nothing is told
  about -- an automatic variable -- carries nothing. A property is lent as the reference to its own
  storage, formed the way any other is, and handed to the object it belongs to, which becomes what
  the reference carries. A write through a reference to a whole variable and through one to a part
  of it is the same write: it asks the variable to admit it, compares the part where anything waits,
  writes, and has the variable report, at the moment of the write. A write through one to a
  property, or to a part of one, tells the object at the moment of the write as well; the object's
  waiters reevaluate what they reached, so it is told nothing about which bits moved, and nothing
  puts a property under a procedural continuous assignment (LRM 10.6). Only what acts on the
  variable itself -- its sampled value -- needs the whole, and whether a reference names the whole
  is said where it is formed, because a part can lie at the address its whole does (the last
  component of a C++ `std::tuple` does). A wait through a reference registers on the holder whatever
  part the reference names
  ([a-variable-a-body-declares-reports-its-writes](a-variable-a-body-declares-reports-its-writes.md)).
- **What a write asks of the variable needs no type, except where the variable is in a rare state,
  and then the variable carries it.** Whether a procedural continuous assignment holds it (LRM 10.6)
  and whether anything waits are yes-or-no facts of any variable. The one type-dependent duty is a
  sampled variable keeping its value from before the time slot's first change (LRM 16.5.1). So every
  variable is, whatever it holds, one type-free cell whose write rules are a null test, and a
  variable that is sampled or taken over grows a state behind it that does the typed work through an
  ordinary virtual call. A write the variable turns away lands in a copy the write itself keeps,
  since only the writer knows the type of what it reaches.
- **A step reports what forming the part did.** Binding a reference to an associative entry that
  does not exist creates it (LRM 7.8.7), which is a change of the variable at the bind, reported
  there. An index naming no element names storage that belongs to nothing (LRM 7.4.6).
- **Below MIR, each step is one library call on a reference, as the C++ library's
  `Ref::ReferElement` is for clang.** On the execution backend a reference is a library object of 32
  bytes, held by value like any other, reached by address, copied where it is kept.

## Rejected

- **Lend the landing of a write held for the call.** What there was, for a variable's part and for a
  property. It reports when the call ends, which 13.5.2 rules out.
- **The variable plus a coordinate, resolved at each access.** A queue element must stay the element
  it was across a `push_front` (LRM 7.10.3), so a coordinate names the wrong one; and it re-walks
  the path on every access.
- **One word pointing at a record of the variable and the storage, kept in the binding frame.** A
  `ref static` formal may be passed an element of a static variable and used in a
  `fork ... join_none` (LRM 9.3.2, 13.5.2), so the reference can outlive the frame that bound it.
- **A table of the variable's type carried in the reference** -- a Rust `&dyn`, where the reference
  is where the type is erased. It was built first. Every write through a reference then made two
  indirect calls to ask a variable questions whose answer, for almost every variable, is "nothing to
  do"; which variables have anything to do is known only to the variable, so the dispatch belongs to
  it and exists only once it has something to dispatch to. It also left each variable carrying a
  sampled copy of its value and a takeover pointer inline whether it was sampled or taken over or
  not.
- **Sampling every sampled variable eagerly at the start of each slot**, as Verilator refreshes its
  `__Vsampled_` copies at the top of each evaluation. It needs no write-time duty at all, and pays a
  copy of every sampled variable per slot rather than per change.
- **Overriding reads rather than writes under a takeover**, as Verilator's `VlForceVec::read` does.
  It puts a test on the read path of every signal, which is where simulation spends its time.

## Relation to existing decisions

- [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) -- its enumeration lists a
  member and an element as plain storage, which holds for a part of storage nobody waits on and not
  for a part of a variable something waits on; its own D5 says it cannot admit a form needing more
  than a pointer. The width its measurement charged a wider reference was measured on an array of
  4096 references, which the language cannot build. Its D3 -- resolve the path once at the bind,
  never re-walk it -- is what the steps do.
- [owner-transition-and-observation](owner-transition-and-observation.md) D2 -- "a write through one
  is visible to everything else reading that variable at the moment it happens" is what this
  realizes for a `ref` actual.
- [a-write-compares-what-it-reached](a-write-compares-what-it-reached.md) -- a designation borrows a
  write that reports when its full-expression ends, which is right for a write and why a lent part
  is not a designation.
- [object-is-an-event-source](object-is-an-event-source.md) -- a write to a property is a write
  opened on the object; a property lent by reference is the one form of write that outlasts the
  full-expression, so the reference carries the object instead.
- [call-scoped-borrow-registration](call-scoped-borrow-registration.md) -- unchanged: registration
  stays at the bind and at the invocation's end, and nothing reaches the access path.

## Consequences

- A write through a `ref` bound to a part, or to a class property or a part of one, wakes a waiter
  on it at the write, on both backends, and a part of an automatic variable or a class property is
  lent on the execution backend.
- A reference is 32 bytes on both backends, where the execution backend's was one word.
- A variable nothing samples or takes over carries one null pointer where it used to carry a sampled
  copy of its value, that copy's time stamp and a takeover pointer.
- A write the variable turns away lands in a copy the write keeps, so a write in progress on the
  execution backend is 288 bytes of stack where it was 160.
- Instruction counts against `main` at `a95e1e07`, `--release`: unpacked-array-write 1,310.8 M to
  1,314.2 M, compute-block 2,124.4 M to 2,136.8 M. The difference is a fixed cost per write --
  opening one asks the variable whether it admits the write and whether anything waits, and a whole
  store reaches the rare state through the variable's base -- and does not grow with the value.
- Still refused: a `ref` port connected to a part, because the port is bound before the variable's
  declaration installs what it holds, which moves the part. Waiting on a reference to a part is
  answered by
  [a-variable-a-body-declares-reports-its-writes](a-variable-a-body-declares-reports-its-writes.md),
  and a `ref` formal's sampled value is its current one; what stays refused is the sampled value of
  a `ref static` formal.
