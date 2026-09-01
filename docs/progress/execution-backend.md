# Execution backend

Tracks the MIR / LIR -> LLVM execution backend's own realization -- the parts that are the
backend's, not a single SystemVerilog feature. Per-feature backend status lives in the feature files
(a DPI scalar in `dpi.md`, a timing control in `processes.md`); this file owns the backend
infrastructure: how a runtime value lives, what the backend can and cannot lower yet, and its
coverage.

JIT and AOT are link-time choices over this one backend, not separate surfaces. Design elaboration
runs here as it does on the C++ backend: the backend lowers cross-unit construction and realizes
members as runtime-owned storage, so it elaborates a hierarchy of modules through the design root.

Done when a design compiles and runs through this backend end to end, matching the C++ backend's
answers wherever both accept the source.

Contracts: `../architecture/backend_contract.md`, `../architecture/lir.md`,
`../architecture/runtime_distribution.md`, `../architecture/object_lifetime.md`.

## Agreement with the C++ backend

Wherever both backends accept a source they answer the same, and what this backend has not realized
refuses to lower and says which construct it was. The difference between the two is a diagnostic,
never a different answer -- a construct that lowers and then answers wrongly is a defect, not a gap.
A conformance case says nothing about either backend; it states what IEEE 1800 requires and both are
held to it. What this one refuses is recorded once for the path, so the cases absent from that
record are the cases it runs -- coverage is read off a file that only ever shrinks, never asserted.

## Runtime-value lifetime

A runtime value crosses the execution boundary as an opaque handle into runtime-owned storage
(`../decisions/jit-value-realization.md`); a transient lives in a per-stretch scope and is released
when that stretch returns. A value whose lifetime crosses a suspension needs storage that outlives
the stretch that made it.

- [x] **A value that crosses a suspension survives, for the value domains realized today.** A
      value-typed non-managed procedural local in a suspending body -- a loop counter, a read-only
      local read after it resumes, a local mutated across nested control flow -- lives in the
      activation frame, storage the running activation owns for its whole life and reaches through a
      handle the generated frame holds. A store copies into it; a load copies out; the store
      decision is made where storage placement belongs and the backend realizes it as an ordinary
      call, so it stays mechanical. Complete for every non-managed value domain the backend realizes
      today, aggregates included -- a struct or dynamic-array local crosses as one activation-frame
      value. Contracts and rationale: `../decisions/cross-suspension-value-storage.md`,
      `../decisions/activation-frame-and-transient-scope.md`.

- [x] **The storage lifetimes are named and separated.** The per-stretch transient scope, the
      activation frame, the activation's control identity, and the process lineage node are distinct
      names for distinct concepts; a per-stretch transient may not escape its stretch, and every
      store into longer-lived storage copies or promotes (the one non-copying path, a method return,
      stays in the caller's scope). Settled in
      `../decisions/activation-frame-and-transient-scope.md`.

The rest are further values that outlive the stretch that made them, none on the execution backend
yet. Each is another instance of the same lifetime question, so the first to land decides whether it
extends the activation frame or the backend adopts one lifetime discipline (a traced heap,
ownership, or native in-frame layout) for every value.

- [x] **The scalar real family** (real, shortreal, realtime) -- realized on the execution backend as
      a value domain alongside packed and string: arithmetic, comparison, the integer/real
      conversions, and real formatting run, a real signal member is an observable cell, and a real
      procedural local crosses a suspension as an activation-frame value. It extended the activation
      frame with another domain rather than forcing a new lifetime discipline, since a real is a
      non-managed value like a packed one.
- [x] **The chandle** (LRM 6.14) -- realized on the execution backend as a pointer-like value
      domain: the value is the pointer itself, carried inline rather than behind a handle to a
      runtime-owned object. A chandle defaults to null, assigns from null and from another chandle,
      takes the equality and case-equality families and the boolean test, and lives in a member slot
      as an owned inline value (not an observable cell, since no process subscribes to it). This is
      the first bare-pointer value domain; a class handle later reuses the shape.
- [x] **The unpacked struct** (LRM 7.2) -- realized on the execution backend as a product value
      domain: a runtime-owned product that owns its components by value and crosses as an opaque
      handle, so the generated side never inspects a component's representation. It default-
      constructs member-wise, builds from an assignment pattern, copies with value semantics, takes
      the equality and case-equality families, reads and writes a component (including a nested
      product and a string component), lives in a member slot as a whole-cell observable signal
      whose partial write fires subscribers, and crosses a suspension as an activation-frame value.
      A component write is a whole-value rebuild stored back through the value's owner, so an
      observable partial write never bypasses the cell's update semantics -- the aggregate
      partial-update protocol a container reuses later.
- [x] **The dynamic array** (LRM 7.5) -- realized on the execution backend as a run-time-sized
      container value domain, the first variable-size aggregate. It defaults to empty, builds from
      `new[N]` / `new[N](src)` and an assignment pattern, copies with value semantics, takes the
      equality and case-equality families, reports its size, reads and writes an element (an
      out-of-range read yields the element default and an out-of-range write is discarded, LRM 7.4.5
      / 7.4.6), empties under `delete`, lives in a member slot as a whole-cell observable signal
      whose element write fires subscribers, and crosses a suspension as an activation-frame value.
      An element write and `delete` are functional whole-value updates stored back through the owner
      -- never an in-place mutation of a value reached through a possibly-shared handle -- so value
      semantics hold across a copy; this is the mutating-container protocol the queue and
      associative array reuse.
- [x] **The fixed-size unpacked array** (LRM 7.4.2) -- realized on the execution backend as a
      fixed-arity container value domain. It default-constructs member-wise, builds from an
      enumerated element list and from a replicated pattern through one repeat-unit-and-count path,
      copies with value semantics, takes the equality and case-equality families, reports its size
      and its bit-stream width and count, reads and writes an element, reads a contiguous range
      select, lives in a member slot as a whole-cell observable signal, and crosses a suspension as
      an activation-frame value. Its payload is ordinal-only: the declared range is the receiver's
      static type's and arrives at a select as its own operand, so a whole-value store copies
      positions and relabels nothing -- and a store between two arrays whose declared ranges differ
      lowers, because the range is gone by this layer, both sides are one type, and a type pool
      keyed by content says so. Writing a range is refused: it rebuilds the whole value with that
      window replaced, which no container here offers yet.
- [x] **The queue** (LRM 7.10) -- realized on the execution backend as a run-time-sized ordered
      container value domain. It defaults to empty, builds from an assignment pattern, copies with
      value semantics, takes the equality and case-equality families, reports its size and its
      bit-stream width and count, reads and writes an element (a write at the element after the last
      appends one, LRM 7.10.1, and every other invalid index discards the write), takes a slice,
      pushes at either end, inserts, drops the entry an index names or empties entirely (LRM
      7.10.2.3), lives in a member slot as a whole-cell observable signal, and crosses a suspension
      as an activation-frame value. A declared bound (LRM 7.10.5) belongs to the variable rather
      than to the value written, so it reaches a construction as an operand and a semantic store
      passes its right-hand side through the bound the destination declares. A pop both updates the
      queue and yields the element it removed (LRM 7.10.2.4), so the entry completes with the two of
      them and the call site stores the queue back and takes the element as the call's value.
- [x] **The associative array** (LRM 7.8) -- realized on the execution backend as a keyed container
      value domain, the first container whose coordinates are values rather than ordinals. It
      defaults to empty, builds from a list of entries with or without the miss value a `default:`
      states (LRM 7.9.11), copies with value semantics, takes the equality and case-equality
      families, reports how many entries it holds and its bit-stream width and count, reads an index
      with no entry as the element default, allocates an entry on a write, reports whether an index
      has one, drops the entry an index names or empties entirely (LRM 7.9.3), answers the smallest
      and largest index it holds, lives in a member slot as a whole-cell observable signal, and
      crosses a suspension as an activation-frame value. It holds no prototype for an index -- the
      clause gives it no index bounds and no index default -- so an index crosses in the
      representation the array's declared index type names and the order two indices sit in is read
      from the indices themselves. A wildcard index (LRM 7.8.1) is refused: its entry is named by
      the value the index expression denotes rather than by the expression's own bits, and no
      conversion states that yet.
- [x] **The traversal family** (LRM 7.9.4 -- 7.9.7) -- realized on the execution backend. Each
      answers with the SV int the method reports and the index it visited, which is the probe
      unchanged where the array holds no such neighbour, and the call site stores that index into
      the variable the source named -- so the variable's own write path runs and its update event
      fires. `foreach` over an associative array and the index-ordered checks run through this.
- [ ] **An unpacked concatenation** (LRM 10.10) -- refused on the execution backend, so a queue
      built from one, and every queue whose declared bound is exercised through one, waits here. Two
      things it needs, both already shaped by what is settled. No C ABI names an entry per arity, so
      the parts fold into a chain: the empty container the destination declares, then one append per
      part, left to right, which is the order the parts were written in and so the order their
      elements land in. And a part that contributes its own elements may be a container of any kind,
      whose representation the appending entry has no way to know, so that part crosses erased like
      every other value that states a representation.
- [ ] **The union domains** (LRM 7.3 untagged, 7.3.2 tagged) -- not realized on the execution
      backend yet, so building one and reading a member both refuse. An untagged union holds one
      member at a time, so its value is that member plus which one it is, and a member write makes
      the written member the live one; a tagged union adds a checked tag, so a read whose tag does
      not match is a run-time error rather than the member's default. The packed spelling of either
      is a bit plane and needs nothing of its own -- it already runs.
- [ ] **A managed value (class handle) across a suspension.** A traceable frame and precise
      reclamation, none of which is implemented: the managed reference is realized as a
      reference-counted handle that does not reclaim cycles, and only in the C++ backend. Contract:
      `../architecture/object_lifetime.md`.
- [ ] **A reference argument aliasing storage that is not a cell.** A reference binds the cell its
      referent lives in, so a signal is lent by taking the address of the cell it already is, and a
      write through the reference raises the update event a write to that signal owes its
      subscribers. A plain local is lent the same way, since a local whose storage is lent is given
      a cell where it is declared. What is left refuses because the referent's value lives somewhere
      no address reaches: a suspending body's local lives in the activation frame, a class property
      is a member owning its value rather than a cell holding it, and a part of a value aggregate is
      no independent storage at all. An `output` / `inout` argument is not subject to this -- it
      copies out through the actual's own write path.

## Value realization: two tracks today, one native model deferred

The value layer is realized two ways, and the breadth work above runs against this split:

- The transitional C++ backend realizes each value type as a monomorphized target type -- the host
  C++ compiler expands one concrete type per element type, and an aggregate interior is written in
  place because that type owns real storage.
- The execution backend realizes each value as an opaque handle into a runtime-owned, type-erased
  object (`../decisions/jit-value-realization.md`, `../decisions/jit-aggregate-realization.md`): it
  emits generated code with no host compiler to expand a template, so an aggregate is one erased
  object and an interior write is a functional whole-value update.

Both are correct and agree per source (the backend-agreement tests check this), but they are two
implementations of the same value semantics. Every value domain added to the execution backend is a
second implementation beside the C++ one, so the two-track maintenance grows as the breadth fills.
This is deliberate, not overlooked: erasure is the uniform, correct baseline chosen so the
value-domain breadth can be filled first, and the C++ backend is transitional.

- [ ] **One native value model (physical value monomorphization).** The convergence that ends the
      two-track split: the execution backend generates specialized native code per concrete type --
      doing the type expansion itself, the way the host C++ compiler does it for the C++ backend --
      so a value's bytes live inline and its operations are native, reproducing the value layer's
      physical layout in generated code (`../decisions/jit-aggregate-realization.md` physical value
      monomorphization; `../decisions/jit-value-realization.md` native in-frame layout, member
      storage included). It is a deferred, value-model-wide endpoint gated behind the value-domain
      breadth being broad, never a per-domain step; once it lands the value model is native on both
      sides and the second implementation is no longer a separate track. A run-time-sized container
      keeps runtime-owned storage regardless -- its element count is a runtime quantity -- so this
      makes the fixed-arity aggregates and the element bytes native, not the container's own
      storage.

## Deferred effects and concurrency

Each defers a value, or hands control to another activation, past the end of the current stretch, so
each meets the same lifetime question above.

- [ ] A task enable. Control returns to the enabler only once the task completes (LRM 13.3), so the
      enabler suspends on another activation's completion rather than on a wakeup source it
      registered itself -- the one suspension whose resumption is a second body's to signal.
- [x] **Non-blocking assignment** (LRM 10.4.2). A read taken after the statement in the same time
      step still sees the value the destination had, and the assigned value appears only once the
      update region has run. Every destination form takes it: a whole variable, an element, a range,
      a structure member, and a concatenation left-hand side. A destination that is an
      automatic-lifetime local is still rejected. Rolled up in `processes.md` (P4).
- [x] **Fork / join, and the branches a `fork` spawns.** A branch is a callable value whose body
      completes as a coroutine: its captures are copied where the `fork` ran and the execution that
      runs the branch owns them, so nothing the branch reads points into a stretch that has already
      returned. The three join modes differ only in what the spawning process then waits for --
      every branch, the first of them, or nothing (LRM 9.3.2) -- and no branch starts until that
      process blocks or terminates. `wait fork` and `disable fork` read the executing process, so
      neither names a child.

      Two things this settled reach wider than `fork`. An entry that arranges an execution's
      resumption also answers whether it must park at all, because a join whose condition already
      holds and a `wait fork` whose children have all terminated leave nothing to wait for; a
      suspension that always parked would hang on either. And which disable targets an execution is
      inside is the execution's own state rather than its body's, so a spawned branch is enclosed by
      the targets its spawner was inside even though its own body states no region -- which is what
      lets `disable` of a named `fork` reach a branch parked on a delay, and what a task enabled
      inside a target will need for the same reason.

- [ ] Named events across a suspension. Rolled up in `processes.md` (P9).

## Other backend surfaces

- [x] **A runtime entry is named by its operation and typed by its call.** An entry's symbol comes
      from the operation, and its signature from the values the call passes, so neither is written
      down beside the other where the two could drift. Which entry a builtin resolves to is stated
      per builtin rather than inferred from what its operands happen to be, and what the library
      does not realize is stated the same way, naming which shape it has no entry for. An entry now
      exists as a prototype, a definition, and a binding held to each other by a check, so one
      written without the others fails the build instead of failing to resolve at run time. What is
      still refused no longer shares one message: the memory load and dump tasks walk their memory
      through the container kind and depth their entry is compiled against, which an erased handle
      does not carry, and a named event's members have no storage realization.
- [ ] **A memory load or dump (LRM 21.4, 21.5).** Every form the source may write reaches the
      backend now -- an unpacked memory of any depth, a dynamic array, a queue, an associative
      array, each either running upward from an address or bounded by a window -- and every one of
      them refuses, because the library reaches a memory's words by walking the container type its
      entry was compiled against, and an erased handle carries neither that kind nor its nesting
      depth. Realizing them means a walk driven by the value's runtime domain, and a load then
      answers through its completion like every other service that reports through an argument.
- [x] **A closure the runtime holds and runs later.** A deferred effect -- a non-blocking
      assignment, a postponed print, a deferred assertion's action -- is a closure the process hands
      to a region and keeps running past, so the body runs once the stretch that built it has
      returned. What it captured survives with it: a captured value is taken by copy where the
      closure was built, so nothing a deferred body reads points into storage that is already gone.
      The closure a `fork` branch builds shares that storage and differs in what becomes of it:
      entering the branch takes the captures rather than borrowing them, since the execution
      outlives the stretch that built them and nothing else owns them.
- [x] **A runtime service answers through its completion, never through storage the caller lends.**
      Every service that reports through an argument the call names -- `$fgets`, `$ferror`,
      `$fread`, `$value$plusargs`, `$readmem` -- completes with a product of the values it settled,
      its own result first and then one per argument it answers through, and the call site stores
      each where the source named it. Whether the destination's current value also crosses in is the
      formal's direction: a read that replaces its destination outright passes nothing in, while one
      whose answer is shaped by what the destination already holds passes it, which is what keeps
      the words a file does not address and the variable an unmatched plusarg names. It is the same
      rule and the same machinery a user subroutine's `output` and `inout` have always used, so
      nothing about these services is special to the call site any more -- including the position
      they may stand in, since a call that answers this way is an ordinary expression.

      What this replaced could not work here at all: a destination lent as an ordinary argument
      crosses as a handle the generated side may not mutate, so the callee's write reached nothing
      the caller could read. The two backends therefore disagreed on the same source, which is the
      one difference between them the agreement contract does not allow.

- [ ] Storage reached by name rather than through a receiver, for a class's static property and
      static constant. A package or `$unit` variable runs: such storage is named by its linkage
      symbol, a place opens at that symbol and dereferences it, and the execution session resolves
      the name to the address the design's own storage sits at. A class's statics are the same shape
      and need the symbol a class mints for them, which nothing publishes yet.
- [ ] Where a base class's storage sits inside a derived object. A member is reached by its position
      in the owning class's member list, and the runtime builds that list from the class's own
      members alone, so a field declared by a base indexes the derived object's list instead.
      Calling an inherited method has the same hole seen from the call side: the receiver crosses
      without being re-typed to the class that declares the body. Both need one decision -- a base
      sub-object as a place step, or one flattened member list with the base's members first -- and
      the runtime's member table has to match whichever it is. Constructing the base is the same
      question seen a third time, and it is the one place this backend does not refuse but drops: a
      constructor lowers as an ordinary function, so a class that initializes its base runs a
      constructor that does not. It reads as a gap only because a base is not reachable here yet;
      the two answers above each imply a different shape for it, so it is not separable from them.
      The C++ backend never had to answer any of the three, because the host language answers them.
      Unreachable end to end today, since constructing an object is itself refused here.
- [ ] `dump llvm`, and `run` / `compile` end to end against this backend, so a design goes from
      source to a running program without the C++ backend.
- [ ] The smoke, benchmark, and AOT CI jobs, which are disabled until a design runs end to end here.
- [x] **An array of owned children.** A child scope -- a module instance, a generate block, a
      procedural block scope -- is constructed, reached by name and per-axis index, and reports its
      hierarchical name, whether it stands alone or is one of an array. The array is not a sequence
      a member holds: each element is its own child, told apart by the index its hierarchy segment
      carries, which is what a lookup by index matches against and what `%m` renders in brackets. An
      index is an ordinary value of the design and reaches the runtime as the handle every value
      reaches it as.
- [ ] Driving a net. A net's value is the resolution of its drivers, so a driver attaches to a
      resolution node and updates a contribution rather than writing a cell; neither reaches the
      runtime from generated code yet, so a net-bearing design does not run here. Rolled up in
      `nets.md`.
- [ ] By-pointer DPI-C marshaling (the by-value scalar surface runs; see `dpi.md`).
- [x] **A region that consumes a control effect** -- what a named block, a named fork, and a task
      need so that `disable` of one resumes execution after it (LRM 9.6.2). A named procedural block
      runs here whether or not anything disables it, a self-`disable` leaves its own region, and an
      effect naming an enclosing target passes outward through the regions that decline it. The
      region's extent is bracketed by two ordinary calls under a cleanup that runs on every way out
      of the body, so no backend has to run code at scope exit to hold the membership; where an
      execution regains control inside a region it asks the runtime whether a target it is inside
      was disabled while it was away, and the generation comparison behind that answer stays in the
      runtime. An effect no region claims settles the activation cancelled through the completion
      outcome rather than by unwinding. Disabling a named `fork` exercises all of it: the branches
      it spawned are enclosed by the target even though their bodies state no region, so the
      `disable` wakes one parked on a delay, that branch settles cancelled where it regains control,
      and the process that entered the block resumes after it in the same time step. What still
      waits on starting another activation is the `disable` of a task, whose enable is an await on a
      coroutine callee.
- [ ] The transient-escape rule is held by construction and naming, not by a checker.
- [ ] Displaying an aggregate. A print item is named by the operand's value domain, and the erased
      container this backend realizes exposes no per-element walk for a formatter to use. It is the
      collection domains' item above seen from the formatting side.
- [ ] **Below LIR, an unrealized construct reports itself as a compiler bug rather than as a
      diagnostic.** The contract above says the difference between the two backends is a diagnostic,
      and the lowering into LIR honors it: a construct with no LIR shape answers `unsupported`. Two
      of the three places below it now do as well -- a builtin the library has no entry of the shape
      for, and a member type with no storage realization, both of which used to tell the reader to
      file a bug for a gap nobody had filled. What is left is a name the generated module calls that
      nothing defines, which still surfaces as the module failing to link. The memory dump task was
      one such name and now refuses instead, but it was found by asking rather than by anything
      failing: no case reached it, because a case that dumps a memory reads it back and was stopped
      by the load first. So the gap is narrower -- which entry a builtin resolves to is stated per
      builtin, and a prototype, a definition, and a binding are held to each other -- while a name
      minted for an entry nobody ever wrote is still checked only by the corpus reaching it. Closing
      it means admitting a module against what the runtime realizes before the session materializes
      it.
- [x] **End-to-end coverage is the corpus, not a handful of cases.** What this path refuses is
      recorded once for the path rather than on any case, and a case that starts running fails until
      its entry is dropped. So the record only ever shrinks, and dropping entries is what landing a
      construct looks like -- in the same change, the way a checkbox above is flipped with the code
      that closes it. Absence of an entry is a claim the run checks, which is what makes the record
      a measurement rather than an assertion.
