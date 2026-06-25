# Lifetime-extended automatic scope storage

Date: 2026-06-24 Status: accepted

## Context

A `fork ... join_none` (or `join_any`) branch is a concurrent process that can keep running after
the process / task that spawned it has returned. SystemVerilog LRM 6.21 says: "The lifetime of a
fork-join block shall encompass the execution of all processes spawned by the block. The lifetime of
a scope enclosing any fork-join block includes the lifetime of the fork-join block." So an automatic
variable of the enclosing scope must stay alive for the spawned branch to read or write it -- even
after the enclosing activation has returned.

In a value-semantic, no-GC compiler whose procedural automatic locals are plain locals in a C++
coroutine frame, that storage dies when the frame returns; a by-reference capture into the branch
would then dangle.

[`variable-lifetime-storage`](variable-lifetime-storage.md) already settled the adjacent cases:

- A **bare** declaration (`int x;`, no keyword) resolves to static lifetime and is realized as a
  per-instance member -- persistent, so a detached branch reading it is already safe.
- A **fork-scope** loop local is by-value snapshotted into each branch.

It left one case unhandled: an **explicitly `automatic`** local declared in the _enclosing_ scope
and captured **by reference** by a **detached** branch. It rejected the two mechanisms that would
handle it -- escape-boxing ("adds a refcount / heap mechanism and ... fights the frontend: it only
helps if bare stays a per-activation local, which contradicts the resolved static lifetime") and
keep-the- frame-alive ("semantically wrong: it pins what is an automatic, activation-scoped frame
and uses it to imitate static"). This decision revisits those rejections for the
explicitly-automatic case.

Two facts make the revision clean:

1. **Lifetime-extended automatic is not static.** A static local is one shared cell per instance,
   initialized once, persisting across activations. A lifetime-extended automatic gets a fresh,
   re-initialized storage per activation; only its _destruction_ is deferred until the spawned
   branches that borrow it finish. The "fights the frontend / imitate static" objection is about
   _bare_ locals (which the frontend makes static, so boxing is pointless). For an explicit
   `automatic`, the variable genuinely _is_ per-activation, so the objection does not apply.

2. **MIR already reserves the ownership mode.** A one-level indirection is
   `PointerType{pointee, ownership}` with `ownership ::= kUnique | kShared | kBorrowed`; plus a
   non-owning `RefType` value alias. `kUnique` (owned child instances), `kBorrowed` (`self`), and
   `RefType` (`ref` formals, by-reference captures) have producers; `kShared` already renders to
   `std::shared_ptr<T>` but has no producer yet. Shared ownership is an anticipated mode awaiting
   its first consumer, not a new primitive.

## Decision

**An automatic lexical scope that may be borrowed by a process able to outlive it is realized as an
explicit, shared-owned activation object rather than as coroutine-frame locals. The activation is
reached through a handle exactly as a callable reaches its enclosing object through `self`: the
handle is a pointer, and a promoted local is a member read as `handle->local`. The handle's type is
`PointerType{kShared}` -- a shared pointer to the activation, the first producer of `kShared`.**

The handle is the whole mechanism. In the scope that declares the locals it is an ordinary local
variable holding the shared pointer. A detached branch that borrows a promoted local captures that
handle by value -- a shared-pointer copy, exactly how `self` is captured (a by-value pointer copy)
and how a `move` closure in Rust captures an `Rc`. That single by-value capture does both jobs at
once: the copy keeps the activation alive, and member access through it (`handle->local`) reads and
writes the cell. There is no separate reference-into-the-cell and no separate lifetime token; the
captured shared handle is both, which is ordinary shared-pointer practice.

The model in detail:

- **Granularity is the lexical automatic scope** -- a begin-end block of automatics, or a task body
  -- not a single variable, and not automatically the whole task frame. When such a scope has a
  detached borrowed descendant, the **entire** scope's slots are promoted into one activation
  object. Promoting the whole scope (rather than only the escaping variable) keeps every reference
  to a slot resolving to one logical cell, so a parent write and a branch read can never target
  different backing. A nested automatic block whose lifetime differs gets its own activation owner.

- **A borrowing branch captures one handle per activation, by value.** Reaching the activation's
  members needs only the handle, so a branch that borrows several locals of one activation captures
  that activation's handle once and reads each local as `handle->local`; the dedup is the ordinary
  one-capture-per-captured-value, by activation identity, with nothing per variable. The capture
  propagates through nested closures exactly as `self` does: a nested branch that reaches an outer
  activation captures the handle at each closure boundary it crosses.

- **Lifetime is creation-to-terminal-disposal, by shared-pointer refcount.** The declaring scope
  holds the original handle; each borrowing branch holds a by-value copy taken when its closure is
  constructed (before the parent can return, and without reading the cell, so the branch still
  observes parent writes made after the fork). A holder drops its copy when its frame is destroyed
  by any path -- normal completion, `disable fork`, named-block disable, `$finish` teardown. The
  activation is freed when the last copy drops. This is plain RAII on the shared pointer; no
  explicit acquire / release step exists.

- **`join_any` needs no special ownership rule.** A completed branch drops its handle copy;
  remaining branches keep theirs; the parent may proceed and drop its own; the activation survives
  exactly while any holder retains a copy.

- **Shared-owned values are constructed by a generic MIR operation**, parallel to the `kUnique`
  owned-object construction, not by a backend-only `make_shared` trick. Scope-activation lowering is
  its first producer; the construction story is general so a future consumer reuses it.

- **The ownership graph is a refcounted DAG; no tracing GC.** The invariant that keeps it acyclic:
  the scheduler owns process objects; a process closure owns its by-value `Shared<activation>`
  handle copies; an activation owns **only data slots** -- never a process, a closure, or a join
  group. The join group stays a pure synchronization object and never owns scope storage, so no
  `activation -> process -> activation` cycle can form.

The implementation surface is minimal now: the activation object and its shared handle for the
detached-fork case. The `kShared` ownership contract is general (a copyable handle that retains on
copy, releases on destruction, has stable identity, and yields borrowed access) so the mode is not
fork-specific, but no broad "every runtime object is shared" facility is built ahead of a second
consumer.

## Rejected alternatives

- **By-value snapshot at spawn.** Silently diverges from LRM 6.21: if the parent mutates the local
  after spawning (LRM 9.3.2 starts the branch only once the parent blocks or terminates, by which
  time the parent may have written the cell), or if two branches share a mutable local, a spawn-time
  snapshot reads a stale or unshared value. Wrong answers, not a crash.

- **Per-variable boxing.** Promoting only the escaping local risks split-brain storage -- the parent
  keeps writing the frame slot while the branch reads the box -- and spreads the storage decision
  across every read, write, reference-construction, and projection of that local. Per-scope
  promotion keeps one backing per slot.

- **Keep the C++ coroutine frame alive.** Ties SV lifetime correctness to the C++ compiler's
  coroutine-frame placement, is not portable to a non-coroutine backend, and conflates the SV
  activation storage with the implementation frame. The activation is an explicit Lyra runtime
  object instead.

- **The join group owns the scope storage.** Would let an activation be reached through the join
  group that retains the branches that retain the activation -- the cycle the DAG invariant forbids
  -- and composes poorly when one branch borrows from several unrelated activations. Closure-held
  handles compose; the join group stays a synchronization object.

## Consequences

- The detached-borrow-of-enclosing-automatic case (LRM 6.21) is supported; the `unsupported`
  diagnostic Lyra emits for it is removed once the mechanism lands.
- A nested fork whose inner branch reaches an outer branch's automatic is no longer a separate
  problem: the handle propagates transitively exactly as `self` does.
- `kShared` gains its first producer. The general shared-ownership contract this establishes is
  available to later consumers (e.g. dynamically allocated, reference-counted runtime objects);
  cycle collection, if a future consumer needs it, is a separate decision revisited against that
  consumer's semantics, not assumed here.
- Only a scope with a detached borrowed descendant is promoted; the common path keeps frame locals,
  so ordinary procedural code is unchanged. A read or write in a promoted scope carries one pointer
  indirection.
- Backend proof obligations the mechanism must satisfy: every reference to a promoted local -- read,
  write, reference-construction, and member / index projection -- resolves to member access through
  the activation handle (`handle->local`), not a bare name; in the declaring scope the handle is a
  local, in a detached branch it is a by-value-captured copy; local declarations preserve their
  initialization order and side effects when realized as activation members; the activation is
  destroyed when the last handle copy drops, not when the parent returns; and no generated helper,
  capture wiring, system-task lowering, or debug mapping emits a promoted local's bare name outside
  this single resolution path.

## Cross-references

- [`variable-lifetime-storage`](variable-lifetime-storage.md) -- settled the bare/static and
  fork-scope cases; this decision revises its rejection of escape-boxing and frame-retention for the
  explicitly-automatic case.
- `architecture/callable.md` -- a capture is an owned field whose snapshot-versus-alias is its type;
  the activation handle is a by-value capture of a `PointerType{kShared}`, reached like `self`,
  needing no new capture-kind axis.
- `architecture/scheduling.md` -- closure capture lifetime: a lowering capturing a procedural lvalue
  must establish the fire-before-frame-dies guarantee or refuse; this decision is the "establish the
  guarantee" path for the detached-branch case.
