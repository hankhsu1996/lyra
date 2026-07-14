# Fork-join parallel blocks

Tracks the `fork` ... `join` / `join_any` / `join_none` parallel-block construct (LRM 9.3.2) and the
process-control statements that act on the threads it spawns (LRM 9.6). A parallel block turns each
of its parallel statements into a concurrently scheduled process; the join keyword decides when the
forking (parent) process resumes. This is the first construct in the corpus where one process
dynamically creates more processes, so it rides directly on the suspending-coroutine machinery
established for time-consuming tasks (`functions.md` T2) and the timing controls owned by
`processes.md`.

Done when the LRM 9.3.2 parallel block reproduces: all three join modes, timing inside branches,
nested and sequential-block composition, per-branch local storage and the loop-spawn idiom, named
fork blocks, and the `wait fork` / `disable fork` controls over the spawned threads. The concurrency
is logical, not physical: spawned processes are interleaved on the one shared time axis by the same
single scheduler that already runs `initial` / `always` processes.

The `FJ*` IDs are stable references and do not impose a total order, but FJ1 is the foundation every
other item builds on.

## Background semantics

The three control options (LRM Table 9-1) differ only in when the parent resumes:

- `join` -- parent blocks until **all** spawned processes terminate.
- `join_any` -- parent blocks until **any one** spawned process terminates.
- `join_none` -- parent **continues immediately**, concurrently with the spawned processes.

One ordering rule governs all three (LRM 9.3.2): processes spawned by a fork do not start executing
until the parent process blocks (at the join) or terminates. `join_none` is the zero-threshold case
of the same wait -- resume after zero completions -- `join_any` the one-completion case, and `join`
the all-completions case; they are one mechanism parameterized by a threshold, not three separate
constructs.

A branch reads and writes the variables of its enclosing scope. What a branch may reach, and for how
long, follows from where the storage lives and how long the enclosing activation keeps its locals:

- Per-iteration data uses an automatic variable declared in the fork's `block_item_declaration`,
  initialized on entry to the block before any process is spawned. Each spawned process takes its
  own copy by value, so it is self-contained and stays correct under every join mode -- this is the
  loop-spawn idiom (LRM 9.3.2). A branch may likewise declare its own locals.
- A variable of an enclosing process (`initial` / `always` / `final`) has static lifetime (LRM
  6.21): it exists for the whole simulation, in storage owned by the instance rather than by the
  process activation. A branch reaches it by reference for shared read and write under every join
  mode, including a detached `join_none` / `join_any` branch that runs after the process body has
  completed. The LRM 9.3.2 loop example reads the enclosing loop variable after the loop finishes
  and sees its final value.
- Referring to a subroutine's by-reference formal argument from a `join_any` / `join_none` branch is
  illegal unless the formal is declared `ref static` (LRM 9.3.2); the frontend rejects it, so it
  never reaches lowering.
- An explicitly `automatic` variable of an enclosing scope that a detached branch can outlive -- a
  `task` / method local, or an outer fork branch's block-item local reached from a nested branch --
  is borrowed correctly: the scope's borrowed automatics are lifted into a shared activation object
  the branch keeps alive while it runs (LRM 6.21). The parent and the branch share that storage, so
  a parent write after spawning is visible to the branch, and the branch reads it after the
  declaring frame has returned. A `join` branch borrowing such a local is likewise correct (it
  completes before the declaring frame returns).

## Sub-Steps

### Spawning and the join condition

- [x] FJ1 -- A fork spawns each parallel statement as its own concurrent process, and the join
      keyword sets when the forking process resumes: after all of them (`join`), after the first
      (`join_any`), or immediately (`join_none`). The LRM 9.3.2 ordering rule holds -- spawned
      processes do not run until the parent blocks at the join or terminates. Branches carry their
      own delay so the three join modes are observable (the parent resumes after the slowest, the
      earliest, or not at all). How a branch reaches its enclosing scope's variables is FJ4.

### Timing across branches

- [x] FJ2 -- A delay, event control, or `wait` inside a forked branch suspends that branch
      independently while its siblings proceed; the join condition observes each branch's actual
      completion time, so `join` resumes the parent only after the slowest branch elapses and
      `join_any` after the earliest. Rides on the timing-control machinery (`processes.md` T1,
      T2..T5, P11) and the suspending-task work (`functions.md` T2).

### Composition

- [x] FJ3 -- Branches compose: a branch may be a begin-end sequential block (one process running
      several statements in order), and a fork may nest inside another fork's branch, building a
      tree of concurrent processes. A whole fork wrapped in a begin-end block is a single sequential
      process (LRM 9.3.2).

### Per-branch storage

- [x] FJ4 -- Per-branch storage. A fork is a procedural scope: a `block_item_declaration` is
      initialized on entry to the block, before any process spawns, and each branch takes its value
      as a by-value snapshot, so the loop-spawn idiom gives every spawned process its own
      per-iteration copy (LRM 9.3.2). A branch may also declare its own locals. A branch reads and
      writes the variables of its enclosing process by reference, lifting the FJ1 restriction to
      module-scope signals (LRM 6.21); because a process variable has static lifetime, a detached
      `join_none` / `join_any` branch reaches it correctly even after the process body has
      completed. A reference to a subroutine's by-reference formal from a `join_any` / `join_none`
      branch stays rejected at the frontend unless declared `ref static`. A branch that borrows an
      explicitly `automatic` variable of an enclosing scope it can outlive -- a task / method local,
      or an outer fork branch's local from a nested branch -- is supported: that scope's borrowed
      automatics are lifted into a shared activation object the branch keeps alive (LRM 6.21), so a
      parent write after spawning is visible and the read stays valid after the declaring frame
      returns.

### Naming

- [x] FJ5 -- A fork block may be named (`fork : name ... join : name`) or labelled, creating a
      hierarchy scope (LRM 9.3.4, 9.3.5). This is the handle that process-control statements and
      hierarchical references address.

### Process control over spawned threads

- [x] FJ6 -- `wait fork` blocks the current process until all of its immediate child processes (not
      their descendants) have terminated (LRM 9.6.1). Because a task runs in its caller's thread
      (LRM 9.5), a `wait fork` inside a task waits on children the enclosing process spawned before
      the task was called, and a process with no live child resumes without suspending.
- [x] FJ7 -- `disable fork` terminates all descendant processes of the calling process, including
      descendants of subprocesses that have already terminated (LRM 9.6.3). The calling process does
      not block: the next statement runs at the same simulation time. Terminating a descendant that
      is still parked -- on a delay, an event, a value change, or a join -- revokes the registration
      holding it, so nothing can resume it afterward. As with `wait fork`, a task runs in its
      caller's thread (LRM 9.5), so a `disable fork` inside a task reaches the descendants the
      enclosing process owns. A process with no descendants disables nothing.

### Fork inside a function

- [x] FJ8 -- `fork ... join_none` inside a function (LRM 13.4.4). A function is not a coroutine and
      cannot suspend, so it cannot await a join; only `join_none` is legal there, and it must spawn
      its branches without suspending. (`join` / `join_any` inside a function stays a compile error
      -- see Rejected at the frontend.)

## Rejected at the frontend

- A `return` statement inside a fork-join block is a compile error: the branch lives in a separate
  process (LRM 9.3.2).
- Inside a function, `fork ... join` and `fork ... join_any` are a compile error: a function cannot
  suspend (LRM 13.4). Only `join_none` is legal there. See `functions.md`.

## Out of Scope

- `disable` of a named block or task (LRM 9.6.2) as a general control-flow construct. It interacts
  with fork (disabling a named fork block) but is a broader process-control feature, tracked
  separately.
- `wait_order` (LRM 15.6).
- `std::process` and dynamic process handles (LRM 9.7): process introspection and `status` / `kill`
  / `await` / `suspend` / `resume` on a process object.

## Cross-references

- LRM anchors: 9.3.2 (parallel blocks, Table 9-1 join control, ordering rule), 9.3.3 (start / finish
  times), 9.3.4 (block names), 9.3.5 (statement labels), 9.5 (process execution threads), 9.6.1
  (`wait fork`), 9.6.3 (`disable fork`); 13.4 (`fork ... join_none` inside functions).
- Rides on: `functions.md` T2 (suspending-coroutine machinery), `processes.md` timing controls (T1,
  T2..T5, P11).
- Archive: the archived corpus has no dedicated fork-join feature tests and no fork-join
  implementation; `fork ... join_any` appears only as scaffolding inside one edge-trigger test.
  Nothing to port.
