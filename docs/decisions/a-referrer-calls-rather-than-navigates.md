# A Referrer Calls Rather Than Navigates

## Date

2026-09-16

## Status

Accepted. Spends the per-unit artifacts
[only-a-base-links-two-signatures](only-a-base-links-two-signatures.md) established, and reverses
[published-member-placement](published-member-placement.md) D1 and D5 together with
`../architecture/emission_model.md` invariant 8 -- the position arithmetic they settle exists so a
referrer can locate a published member, and nothing locates one after this.

## Why this decision matters

A unit publishes a promise, and a referrer is supposed to depend on that promise and nothing else.
The compiler states this at every layer above emission: a unit's emission is a function of its own
contents and the promises it consumed, and a fact absent from every promise cannot invalidate a
referrer.

The artifact did not keep it. What a referrer's translation unit compiled against was the declaring
unit's whole set of declarations, and those carried the members the unit never published, a
declaration for every body, every scope of the design hierarchy the unit opened, and every static
table those scopes need. So a change confined to what a unit kept to itself still moved the text its
referrers compile.

Measured on the 48-unit RISC-V core, emitted at `4741d558`:

- Every unit header is included by exactly one other translation unit, because the instantiation
  graph is a tree. So the coarse dependency is not a fan-out problem; it is a precision problem.
- What a module promises is its ports. Of the 2.99 MB of header text, 1.17 MB -- 39% -- is static
  table definitions that no referrer names.
- One parent parsing its six children's headers costs 0.31 s of compiler CPU with nothing using
  them, and emits a 297 KB object where an empty translation unit emits 1.1 KB. Across the design
  that is under 1% of a 2:59 build, so what this buys is precision for a build that recompiles less
  than everything, not a faster full build.
- Adding one internal variable to a child changes the child's header in three places: a new type
  description, the new member, and the renumbering of every synthesized member after it. The
  parent's own emitted text is unchanged.

## The question nobody had asked

The parent holds a typed pointer to the child's object and reads a published member through it, so
the type the parent compiles against has to describe enough of the child's storage to locate that
member. Everything else follows from that: a fixed prefix so both sides count the same positions, a
rule stating the prefix, a permutation in the lowering that builds it.

What the emitted text shows is that the parent names the child's storage **once per published member
per instance, while the design elaborates**, and never again:

```cpp
u       = AddOwnedChild(make_unique<Leaf>(this, seg));   // construct
field_4 = (Var*)(u->FindSignal("hidden"));               // past the promise, by name
field_5 = &u->a;                                         // promised, by layout
field_6 = &u->y;                                         // promised, by layout
```

Every simulation-time read and write goes through `field_5` and `field_6`, which are cells the
parent already holds. So the whole apparatus for locating a published member buys two lines that run
once, and costs a dependency on the declaring unit's storage.

The alternative is to ask rather than to locate: the unit that owns the storage is the one that
knows where it is.

## What other systems do

The systems that face this question split on **who computes where a published member is, and when**.

- **Modula-2.** An opaque type is restricted to a pointer, because an importing module must know a
  fixed size; the GNU Modula-2 manual states the restriction by naming the option that lifts it
  (`-fextended-opaque` "allows abstract data types to be any type, not just restricted to a pointer
  type"). A pointer needs no size, and everything else is reached through procedures the defining
  module provides.
- **Ada 2005's `limited with`.** Its rationale states the principle directly: tagged types are
  always passed by reference, so a tagged incomplete view suffices for a parameter without seeing
  the full declaration, which is what breaks the compilation dependency. Seven versions of AI-217
  were written before the shape settled, which is a measure of how narrow the answer is.
- **Objective-C's non-fragile ABI.** The declaring class exports a symbol per published instance
  variable and the offset is filled when the image loads, so a private addition to a base moves
  nothing in a client. It exports a symbol only for what a client may name.
- **Swift's library evolution.** A type that is not `@frozen` has an in-memory layout that is
  "opaque across a resilience boundary"; a client manipulates values indirectly through metadata.
  `@frozen` is the opt-out that "publish[es] its stored property layout to clients", and it trades
  the ability to evolve for direct access.
- **C++, which is one of our targets and has no mechanism.** A caller must know "the complete size
  and layout, including private data members", so the workaround is an indirection the programmer
  writes by hand.

**Where our conditions differ, and it is the load-bearing sentence:** every one of those systems
pays for the indirection on each access, because each access is where the client meets the member. A
cross-unit reference here resolves once while the design elaborates and seals into a cell the
referrer keeps, so the indirection is paid once per instance and never on the simulation path. That
is a property of the reference model rather than of anything unbuilt, so it does not change with
effort spent.

## Decisions

### D1. A unit promises what it offers, never how it is laid out

A promise states the operations a referrer may perform and nothing about where the storage behind
them sits. A referrer performs them; it does not navigate the declaring unit's object.

This is the rule the layers above already state, applied to the artifact: what a referrer compiles
against is what the unit promised, so a declaration the unit kept to itself cannot appear in it and
cannot move it.

### D2. The promise is a class of its own, and the unit's object realizes it

A unit that has an object declares two classes. The **promise** carries no storage and one behavior
per thing the unit published, each of them stated and not defined. The **realization** extends it,
declares every one of the unit's members in the order the source wrote them, and supplies a body for
each behavior.

The promise takes the name the source wrote, because a referrer holds nothing else to spell it with.
The realization is named by nothing outside the unit and takes the name any such class takes.

A unit with no object -- a package -- declares no promise class. Its promise is what it always was:
the declarations it publishes at namespace scope. That is this rule over a unit with no object
rather than a case of its own.

So the promise is the whole of what a design element offers, and the unit states which class that
is. Everything else it declares is its own, including a class of the source language declared inside
it: such a class is a type of that element's instance and nameable only there (LRM 23.9, 6.22), so
having a name the source wrote does not make it something another unit can reach. A consumer
deciding which artifact a declaration belongs in asks the unit rather than asking what a name means.

### D3. A published member is reached by calling the behavior that answers with its cell

The behavior answers with the member's storage, typed. The referrer keeps what comes back, exactly
as it keeps what a reach past the promise answers with today, and every later read and write names
that rather than the object.

Nothing counts a position in the object. A published member therefore sits where its own declaration
puts it, and the permutation that moved published members into a prefix is not needed: whether a
member was published stops being something that can move another one. What both sides still count is
which behavior they mean, out of the order the signature published its members in -- a coordinate
over the promise, not over the object, which is the same coordinate a behavior on any published
class is already reached by.

**A call answering with storage is not a call standing for a value.** What `mir.md` forbids is a
read of storage encoded as a call, and a call whose result a consumer must recognize to recover a
destination. This answers with a typed pointer that an ordinary dereference consumes, which is the
form the reach past a promise already takes.

### D4. A published subroutine is a behavior on the promise as well

A subroutine another unit published is reached by a symbol both sides compose today, and its first
parameter is the declaring unit's own object. A referrer holding the promise cannot spell that type,
so the subroutine joins the promise's behaviors rather than keeping a naming of its own. What a unit
publishes is then one kind of thing, not two.

The behaviors continue the order the members took: a promise states one per published member and
then one per published subroutine, so a coordinate over the promise is counted out of the signature
by both sides exactly as D3's is. Which subroutine sits at which of those positions is the
signature's own list, read by the declaring unit rather than re-walked -- two walks agreeing is not
the same as one order, and an identifier a signature minted for a view is published like any other.

### D4a. What the runtime is entered through belongs to the class standing in its tree

A target whose runtime reaches an object through function pointers needs a per-class record of them.
That record belongs to the promise, because the promise is what extends the runtime's tree class and
so what enters it. Putting it on the realization instead forces the realization to hand it upward
through the promise's own construction signature, which puts a record only one target reads into a
signature every target lowers.

The record names the realization's bodies, which is why it is a declaration in the artifact a
referrer compiles against and a definition in the one it does not: what it is built from is complete
only where the bodies are.

### D5. An instance comes into existence through the declaring unit's entry

A referrer that consumed a promise cannot know how much storage the object takes, on any target, so
it cannot allocate one. The declaring unit publishes an entry that makes one and answers with the
promise, and every referrer -- including the party that builds the design's own root, which has no
owner above it -- reaches an instance through it.

### D6. Standing in the runtime tree, and how the runtime drives an object, are the class's own

Both are stated by the class that has them, not carried on the reference to its base.

The reference to the runtime's tree class had carried the three bodies the runtime drives an object
through, and a reader found them by looking at a class's immediate base. That holds only while
nothing extends a scope, which was true while a unit's object was one class. A promise is extended
by its realization, so the three bodies would sit on the class that has no bodies, and the
realization would not be recognized as standing in the tree at all.

The correction is narrower than it looks: a reference to a class states which class it is, and every
other kind of class reference here carries exactly that. The runtime reference alone carried three
callables belonging to the class doing the referring. Moving them onto that class makes the three
kinds of reference say the same kind of thing.

## Rejected alternatives

- **A symbol per promised behavior, emitted by the declaring unit and called outright.** This is
  Swift's dispatch thunk, and the condition that forces one there is absent here: a resilient
  class's vtable index is deliberately not part of that ABI, so a caller must not hold one, while
  what a unit publishes here is an ordered list and the position in it is the coordinate both sides
  count. A thunk would hide a number that is already public, cost a symbol and an indirection per
  behavior, and still need a table behind it to find the body.

- **Calling the one implementation outright, because a promise has exactly one realization.** The
  reasoning is true and it is devirtualization: `../design-process.md` states that a call whose
  receiver's exact type is known is still dynamically bound and that devirtualizing it is an
  optimization, and `../architecture/north_star.md` 3 keeps correctness independent of optimization.
  A layer above the target stating it would put an optimization where a fact belongs, and the target
  that could not take it would have nothing to fall back on.

- **A promise that carries the published members' storage, with the unit's object extending it.**
  The first shape built. It puts half the layout in the artifact a referrer compiles against, which
  means the promise still moves whenever a published member does -- correct -- but also means
  something has to say which members those are. In the IR that is a field classifying a class's
  members outside the type system, which `mir.md` names as a forbidden shape, and in the backend it
  is a render inventing a class the IR has no node for. Both were built and both are what this entry
  replaces.

- **A promise stated as an interface class (LRM 8.26).** It reads as the natural spelling and it is
  not this: an interface class states a contract that sits on no lineage, while a promise is the
  base of exactly one class and carries that class's identity in the runtime tree. Measured as well
  -- the execution backend refuses dispatching on a behavior an interface class states, so this
  spelling would be blocked there today while the ordinary abstract class it needs already runs.

- **Exporting an offset per published member, as Objective-C's non-fragile ABI does.** It keeps a
  published member a place rather than a call, which is the one thing D3 gives up. Rejected because
  the offset would have to reach the referrer's emitted output, and a storage offset in the IR is a
  forbidden shape there: storage placement belongs to the execution IR and below.

- **Resolving a published member by name at elaboration, through the machinery a reach past a
  promise already uses.** It needs nothing new at all. Rejected for the reason that machinery is
  bounded to what a unit did not publish: it trades a check made where the referrer compiles for an
  unchecked cast, and a renamed member then fails while the design elaborates rather than while its
  referrer compiles.

- **Free entries over an opaque handle, which is Modula-2's own shape.** The dependency story is
  identical and it needs no dispatch at all. Rejected because the handle must also be the runtime's
  tree node -- a reach past the promise calls a runtime method on it -- so the handle cannot be a
  type with no contents, and once it is a class of the object model, a behavior on that class is how
  every other member of it is reached.

## Correcting earlier statements

**`published-member-placement.md` D1** states that a published member's position "is its position in
the signature" and that "a referrer counts the same order in the signature it consumed"; **D5**
places the published members in a prefix while the object's members are built. **`emission_model.md`
invariant 8** stated the prefix rule itself, and now states what replaced it.

All three exist to let a referrer locate a published member. The re-derivation does not dispute
their reasoning -- given that the referrer must locate the member, counting a shared order is the
right way and carrying a position is the wrong one. It disputes the premise: the referrer locates
nothing now, so there is no position for either side to compute, and the rules have nothing left to
decide. The prefix, the permutation that builds it, and the arithmetic on both sides go together.

**`../architecture/lir.md`'s reading of the tree base** is one level deep, and says why: "a scope is
sealed: nothing extends one, so a class either takes the tree as its base or stands outside it
entirely. Walking a lineage here would be looking for a shape the source language cannot write." The
premise was exact and is no longer true -- the shape is written by the compiler rather than by the
source. D6 replaces the reading rather than deepening it, because walking the lineage would find the
tree and still leave the three bodies on the wrong class.

## Consequences

- A change confined to what a unit kept to itself does not move the artifact its referrers compile
  against. That is the dependency the model always stated, now true of the build as well.
- A referrer pays one indirect call per published member per instance while the design elaborates,
  and nothing on the simulation path: every access afterwards names a cell the referrer holds, which
  is what it named before.
- The published prefix, the lowering's permutation, and the arithmetic over the object's storage on
  both sides are removed. A unit's members sit where their own declarations put them.
- A class holds its storage beside the behaviors it takes over, and on a target where both live in
  one name space the identifiers collide: the behavior answering for a cell carries that cell's own
  identifier, because that is what a referrer spells. So a cell's emitted identifier leads with its
  position and keeps the source identifier after it, the same rule a body local already takes, which
  makes the collision unspellable rather than something a render has to look for.
- A unit's object is two classes at every layer, so both backends see one shape: a class introducing
  behaviors and a class taking them over, which each already realizes.
- Each target realizes the dispatch with what it has: one emits a virtual function and lets the
  target find the body, the other reads the class's own table off the value. Making the second
  possible removed a split rather than adding one, on both sides of it: what a class is, is one
  record whatever kind of value holds it, and what a value of a class is, is one shape whatever
  lifecycle it joins -- an instance standing in the hierarchy is that shape plus how the runtime
  drives it. A storage schema stated in two records, and a class pointer held at two offsets, are
  each now stated once, so an operation performed through a class is one entry rather than one per
  kind of value.
- Two combinations the runtime's tree reference used to make unspellable become spellable, and they
  are not the same kind of loss. **A class rooted in the tree that states no way to run is now
  intended**: it is the promise, nothing constructs one, and its realization states the way to run
  -- so no object on the tree lacks one, which is the property the fusion was protecting. **A class
  that states a way to run and is rooted nowhere is the real loss**: nothing would ever enter those
  bodies, and where the fusion made that unwritable this leaves it merely unwritten. That is a
  weakening from "cannot be written" to "no lowering writes it", and it is the price of the promise
  existing at all.

## Cross-references

- [unit-signature](unit-signature.md) -- what each unit kind publishes, and the two ways a reference
  reaches into another unit, which D3 and D4 keep and make one shape.
- [published-member-placement](published-member-placement.md) -- the placement rules this entry
  removes, and the reasoning that was right for the premise it had.
- [only-a-base-links-two-signatures](only-a-base-links-two-signatures.md) -- the two artifacts this
  spends; D2 here is what finally makes the first of them carry only the promise.
- [reaching-past-a-published-class](reaching-past-a-published-class.md) -- a behavior named by the
  class that introduced it and an ordinal within it, which is the coordinate D3 and D4 reach a
  promise's behaviors by.
- [publishing-an-owned-instance](publishing-an-owned-instance.md) -- a published nested instance,
  which D3 answers with the same behavior form as any other published member.
- [a-settled-access-is-ordinary-operations](a-settled-access-is-ordinary-operations.md) -- the
  adjacent case, where the referrer has no name for the class at all rather than a promise it
  compiled against, and the three questions it may ask such a class.
- `../architecture/emission_model.md` -- invariant 8, whose content this entry replaces, and the
  artifact rules the rest of it serves.
- `../architecture/backend_contract.md` -- the mechanical-translation contract D2 restores: two
  classes in the IR become two classes in a target, with nothing for a render to work out.

## Sources

- GNU Modula-2 manual, extensions: `-fextended-opaque` and the pointer restriction it lifts --
  https://gcc.gnu.org/onlinedocs/gcc-14.2.0/gm2/Extensions.html
- Ada 2005 Rationale, mutually dependent types and `limited with` --
  https://www.adaic.org/resources/add_content/standards/05rat/html/Rat-4-2.html
- Swift, library evolution and `@frozen` -- https://www.swift.org/blog/library-evolution/
- Herb Sutter, GotW #100, on what a caller must know of a class -- https://herbsutter.com/gotw/_100/
- Objective-C's non-fragile ABI is cited from
  [reaching-past-a-published-class](reaching-past-a-published-class.md), which surveyed it.
