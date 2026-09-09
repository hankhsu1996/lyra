# Variable lifetime storage (static-lifetime body locals)

Date: 2026-06-08 Status: accepted; revised 2026-09-09 for a body outside the design hierarchy, where
"per instance" names nothing (see "What replicates the declaration", below).

## Context

A procedural body local (declared in an `initial` / `always` / `final` process or in a task /
function) has a SystemVerilog lifetime that fixes both its storage and its initialization:

- **static** (LRM 6.21, the module / package default): one storage location per module instance,
  existing for the whole simulation, default-initialized once. It retains its value across
  re-entries of its block and across activations, and it stays live after the enclosing body
  completes.
- **automatic**: a fresh location per activation, reinitialized on each entry, gone when the
  activation ends.

Two forces made the storage of static locals a real decision:

1. **A detached fork branch reads an enclosing process local after the process body completes.** The
   branch is a separate coroutine that may run after the parent `initial` has returned. For it to
   observe the local (LRM 9.3.2 reads the enclosing loop variable's final value after the loop), the
   local's storage must outlive the parent's activation -- i.e. it must be genuinely static, not a
   parent-frame local. This is the case that forced the issue (a detached fork branch capturing a
   parent loop variable).
2. **The frontend resolves a bare declaration (`int x = 0;`, no keyword) to static lifetime** and
   only warns (it does not reject). So static-lifetime body locals are pervasive, not rare, and they
   reach lowering already classified as static.

The hard part is that **SystemVerilog static-lifetime is per-instance, and C++ has no per-instance
variable that can be declared in place inside a method body**:

- C++ function-local `static` is whole-program (one shared instance), not per-module-instance. A
  module instantiated twice would share one copy -- wrong.
- C++ gives per-instance storage only through a class member, which must be declared at class scope,
  not at the variable's source position inside the method.

So per-instance + whole-sim storage is only expressible as a class member. There is no "per-instance
local declared in place" to map onto.

## Decision

**Honor the frontend's resolved lifetime; realize a static-lifetime body local as a per-instance
member of the class enclosing the body, default-initialized once; realize an automatic local as a
C++ local at its source position.** A process and a method use the identical mechanism.

- Per-instance is what the lifetime says, and the instance is the object the body already runs on,
  so the member sits there whichever block of the body declared it. A static inside a method lives
  on the SV class, one cell for the class rather than one per object (see the revision below). What
  the declaring block decides is the name a hierarchical path reaches the member by
  (`procedural-storage-scope.md`), never where the member sits.
- The body reaches the static local by the same scoped reference it uses for any local; the backend
  resolves that reference to the per-instance member.
- The per-instance member is a flat member on the owner class's member arena -- not a sub-struct
  grouping per callable. SystemVerilog's per-callable scoping is already settled by HIR name-binding
  before MIR sees the program; the cross-scope dimension the LRM exposes is the addressable scope (a
  named block, not a callable), and within one addressable scope's class MIR has no per-callable
  visibility to preserve. Modeling each static as a plain member collapses static-local storage and
  access onto the same paths the rest of MIR already uses for module-level signals (one type of
  decl, one MemberAccess shape, one render path, no static-frame walker state on the backend).
- Names are made unique **uniformly** by appending an id to every static member -- not "append only
  on collision". Sibling callables in the same owner class (`static int x;` in two processes) and
  nested blocks repeating an identifier can both claim the same source name; the suffix removes the
  collision without a presence decision. HIR-to-MIR mangles the name as
  `<callable_name>__<source_name>_<hir_var_id>` before adding the decl to the owner class, so the
  member name is unique within the class's member arena.

### Why honor the frontend, and not reclassify bare to automatic

The frontend collapses both an explicit `static` and a bare declaration to lifetime = static, and
exposes the "should have been explicit" only as a non-fatal warning -- the explicit-vs-bare
distinction is not on the symbol. Treating bare as automatic would mean recovering an intent the
frontend has already discarded, per declaration, from syntax. Honoring the resolved lifetime needs
no such recovery: static is static.

### Why not C++ function-local static

It is whole-program, not per-instance. The lifetime and in-place readability would be perfect, but
multiple instances of one module would alias a single copy. Per-instance is non-negotiable, so this
is out.

### Rejected alternatives

- **Escape-boxing** (keep every local in place; box only the ones a detached branch captures by
  reference into a shared cell). Preserves in-place readability, but adds a refcount / heap
  mechanism and, more importantly, fights the frontend: it only helps if bare stays a per-activation
  local, which contradicts the resolved static lifetime.
- **Keep the parent frame alive** (suspend the process instead of returning so its frame locals
  persist). Semantically wrong: it pins what is an automatic, activation-scoped frame and uses it to
  imitate static -- the opposite of what the lifetime says. It also cannot give once-initialization
  or cross-activation persistence.
- **By-value snapshot into the branch.** Loses by-reference semantics (later writes, reads of the
  final value).
- **Whole-program static indexed by instance id.** Global mutable state; rejected against the
  independently-compilable-unit invariant (`north_star.md`).

## What replicates the declaration (revision, 2026-09-09)

The decision above says "per-instance", and for a module body that is right: a module instance is an
elaboration of the declaration, so `u1.blk.x` and `u2.blk.x` are separately nameable cells. The
sentence extending it to a class -- "A static inside a method lives on the SV class for the same
reason" -- carried "per instance" across an analogy that does not hold, and the code took it to mean
one cell per object. That answers wrongly: with two objects each calling a method that keeps a
counter, each saw its own.

LRM 6.21 decides it. A variable explicitly declared `static` inside an automatic subroutine has
static lifetime and is "roughly equivalent to C static variables declared within a function" -- one
cell for the declaration. A class object is not an elaboration of the method's code, so nothing
about it replicates the declaration; a class method is automatic by LRM 8.6, which governs its
ordinary locals and says nothing about one it declares static. So the rule is not "per instance" but
**one cell per thing that replicates the declaration**, and the three declaration scopes answer it
differently:

| The body sits in     | Replicated by | The cell is                      |
| -------------------- | ------------- | -------------------------------- |
| the design hierarchy | the instance  | a field of that instance's class |
| a class              | nothing       | one cell the class owns          |
| a package (LRM 26.2) | nothing       | one program-global cell          |

The original reasoning is untouched where it was reasoning about modules, including every rejected
alternative: whole-program storage is still wrong for a module body, for exactly the reason given.
What is corrected is that "the object the body runs on" is the answer only where that object is what
replicates the code.

This is the same rule that places what a `disable` naming a block invalidates
(`disable-scope-invalidation.md`), which is static-lifetime state of the scope and nothing else --
so a class method's block has one target for the class, which is what LRM 9.6.2 requires when it
disables a block inside an automatic task for every concurrent execution of it.

## Consequences

- **A bare loop-body local initializes once, not per iteration.**
  `for (...) begin int k = 0; ... end` keeps `k` across iterations because it is static. Code that
  wants per-iteration reset must declare `automatic` -- which is also what the LRM 6.21 example
  shows. Tests that relied on the former per-activation behavior now declare `automatic` where reset
  is intended.
- **A detached fork branch can read an enclosing process variable after the body completes**,
  because the variable is per-instance and whole-sim. This is the case that motivated the work.
- **`$strobe` of a static local reads its end-of-timestep value** (LRM 21.2.2), because the local is
  still live in the postponed region; it is not snapshotted at the call. A later same-timestep write
  is therefore observed.
- **The emitted static local is not in place.** It appears as a per-instance member with an id
  suffix, away from its source position. This is inherent to per-instance static storage and is out
  of scope here.
- **An automatic local in a trace task (`$strobe` / `$monitor`) is rejected**, since it is not live
  in the postponed region.
