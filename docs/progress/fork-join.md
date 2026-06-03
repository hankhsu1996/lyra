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

## Sub-Steps

### Spawning and the join condition

- [ ] FJ1 -- A fork spawns each parallel statement as its own concurrent process, and the join
      keyword sets when the forking process resumes: after all of them (`join`), after the first
      (`join_any`), or immediately (`join_none`). The LRM 9.3.2 ordering rule holds -- spawned
      processes do not run until the parent blocks at the join or terminates. Minimal end-to-end
      shape: single-statement branches with no internal timing, where each join mode is observable
      through the order in which the parent and the branches write shared state.

### Timing across branches

- [ ] FJ2 -- A delay, event control, or `wait` inside a forked branch suspends that branch
      independently while its siblings proceed; the join condition observes each branch's actual
      completion time, so `join` resumes the parent only after the slowest branch elapses and
      `join_any` after the earliest. Rides on the timing-control machinery (`processes.md` T1,
      T2..T5, P11) and the suspending-task work (`functions.md` T2).

### Composition

- [ ] FJ3 -- Branches compose: a branch may be a begin-end sequential block (one process running
      several statements in order), and a fork may nest inside another fork's branch, building a
      tree of concurrent processes. A whole fork wrapped in a begin-end block is a single sequential
      process (LRM 9.3.2).

### Per-branch storage

- [ ] FJ4 -- Variables declared in the fork block scope are initialized on entry, before any process
      is spawned (LRM 9.3.2). The loop-spawn idiom -- a fork inside a loop whose branch declares an
      automatic local capturing the loop variable -- gives each spawned process its own
      per-iteration copy. In `join_any` / `join_none` blocks it is illegal to refer to a
      subroutine's formal arguments by reference except in those initializers, unless the formal is
      declared `ref static` (LRM 9.3.2).

### Naming

- [ ] FJ5 -- A fork block may be named (`fork : name ... join : name`) or labelled, creating a
      hierarchy scope (LRM 9.3.4, 9.3.5). This is the handle that process-control statements and
      hierarchical references address.

### Process control over spawned threads

- [ ] FJ6 -- `wait fork` blocks the current process until all of its immediate child processes (not
      their descendants) have terminated (LRM 9.6.1).
- [ ] FJ7 -- `disable fork` terminates all descendant processes of the calling process, including
      descendants of subprocesses that have already terminated (LRM 9.6.3).

## Blocked

- The runtime side of FJ1 -- a running process bringing new processes into being mid-simulation, and
  the ownership and scheduling of those children -- rides on the single object-tree refactor
  (`refactor.md` R3). Today processes are born only at construction and the scheduler walks a
  topology tree mirrored at bind time; fork needs processes created during simulation that attach to
  the live hierarchy. Designing the spawn / ownership substrate against the current dual-tree shape
  would be invalidated when R3 collapses it, so the runtime design waits on R3. The suspending-
  coroutine and join-barrier machinery this builds on is settled (`functions.md` T2). The LRM
  semantics above, the join threshold model, and the fork statement's IR representation are
  independent of R3.

## Rejected at the frontend

- A `return` statement inside a fork-join block is a compile error: the branch lives in a separate
  process (LRM 9.3.2).
- A function body may contain only `fork ... join_none`, never `join` or `join_any`, because a
  function cannot suspend (LRM 13.4). See `functions.md`.

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
