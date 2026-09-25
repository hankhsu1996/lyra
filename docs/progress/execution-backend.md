# Execution backend

Tracks the MIR / LIR -> LLVM execution backend's own realization -- the parts that are the
backend's, not a single SystemVerilog feature. Per-feature backend status lives in the feature files
(a DPI scalar in `dpi.md`, a timing control in `processes.md`); this file owns the backend
infrastructure: how a runtime value lives, what the backend can and cannot lower yet, and its
coverage.

What this backend produces is a program: each unit's module compiled to an object and linked with
the runtime library, which `build` hands back and `run` executes. Design elaboration runs here as it
does on the C++ backend: the backend lowers cross-unit construction and realizes members as
runtime-owned storage, so it elaborates a hierarchy of modules through the design root.

Done when a design compiles and runs through this backend end to end, matching the C++ backend's
answers wherever both accept the source.

Contracts: `../architecture/backend_contract.md`, `../architecture/lir.md`,
`../architecture/runtime_distribution.md`, `../architecture/storage.md`,
`../architecture/lifetime.md`.

## Agreement with the C++ backend

Wherever both backends accept a source they answer the same, and what this backend has not realized
refuses to lower and says which construct it was. That refusal is checked rather than trusted: an
operation named for a value domain it was never implemented for is refused before any of the design
stands, and says which operation, so it cannot instead reach the run and arrive as a design that
failed to come up. The difference between the two is a diagnostic, never a different answer -- a
construct that lowers and then answers wrongly is a defect, not a gap. A conformance case says
nothing about either backend; it states what IEEE 1800 requires and both are held to it. What this
one refuses is recorded once for the path, so the cases absent from that record are the cases it
runs -- coverage is read off a file that only ever shrinks, never asserted.

## Runtime-value lifetime

A value the generated side makes is an object in its own frame, built there by the entry that
answers with it and ended where the source says it stops being readable
(`../decisions/a-value-lives-in-its-makers-frame.md`). A value whose lifetime crosses a suspension
needs storage the execution owns.

- [x] **Memory is bounded by what the program can still read, not by how much it computed.** Every
      value an entry or a generated body answers with is built in the caller's frame and ended at
      the end of its full-expression, on every early exit and every departure too; a declared
      variable's frame slot holds its object and ends it with its block. A loop of a million
      function calls holds 4.5 MB, as it does at a hundred thousand; before, the values accumulated
      until the next wait, 1.85 GB at a million calls.

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

- [x] **The storage lifetimes are named and separated.** The activation frame, the activation's
      control identity, and the process lineage node are distinct names for distinct concepts; a
      value never outlives its full-expression except by being handed to storage that owns it, and
      every such hand-off takes the value's end with it or copies. Settled in
      `../decisions/activation-frame-and-transient-scope.md`.

- [x] **One ownership model for a procedural value, instead of three.** A declared variable is one
      storage, owned by the execution that declared it, opened with the body and ended on every way
      out of it -- including the one no statement spells, where the driver ends a parked execution
      rather than resuming it. Nothing about the body it sits in, and nothing about what that body
      does with it, takes part.

      Three homes preceded it -- a slot of the generated frame, a cell of the execution's own store,
      and a cell built where the declaration runs -- and which one a variable got followed from
      whether its body could suspend and whether anything lent it. Neither is a property of the
      variable, and the first two were mutually exclusive, so a variable needing both was refused:
      lending a variable was refused in every process body, which is every `initial` and every
      `always` there is.

      A fourth home went one cut earlier: a local the body never wrote used to have no storage at
      all, its initial value standing in at every read. That was not a home but an optimization --
      deciding a variable need not exist, from a scan of the whole body, in a step that translates
      one node at a time -- and it refused legal programs wherever the scan's idea of a write was
      narrower than a write (`../decisions/a-declared-local-is-storage.md`).

      **Both cuts ended the same way, and that is the part worth keeping.** Each time the proposed
      fix was to carry the fact the scan recovered, so the translation could read it instead of
      computing it; each time the fact turned out not to be needed at all
      (`../decisions/a-declared-variable-is-one-storage.md`).

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
- [x] **The chandle** (LRM 6.14) -- realized on the execution backend as a value domain holding one
      host pointer. A chandle defaults to null, assigns from null and from another chandle, takes
      the equality and case-equality families and the boolean test, lives in a member slot as a
      variable of its own, and comes into existence from the pointer a foreign call hands back --
      the only way one ever holds a value, since the language admits no other literal for it. It is
      an object in the frame like every other value, one that owns nothing, so ending it is nothing.
- [x] **A class handle as a value domain** (LRM 8.3) -- realized on the execution backend as the
      erased value a type-erased aggregate holds, beside the member slot and the local it already
      lived in. A handle is an element of a fixed-size unpacked array, a dynamic array, a queue and
      an associative array, a member of an unpacked structure, and an associative array's index
      whose entries order by which object each names (LRM 7.8.3, a null index included). It takes
      the equality and case-equality families as one operation answering with the 1-bit value, the
      boolean test in every position including a negation, and the assignment-pattern conversion,
      where a handle naming nothing prints the word null (LRM 21.2.1.6). Every operation such a
      collection supports follows from the domain rather than from the collection, so this was one
      answer and not one per container. Reading a class object out as a bit stream (LRM 6.24.3) is a
      separate operation over the object's own properties and is not carried out.
- [x] **The unpacked struct** (LRM 7.2) -- realized on the execution backend as a product value
      domain: a runtime-owned product that owns its components by value and crosses as an opaque
      handle, so the generated side never inspects a component's representation. It default-
      constructs member-wise, builds from an assignment pattern, copies with value semantics, takes
      the equality and case-equality families, reads and writes a component (including a nested
      product and a string component), reads its bit stream out and builds one back from a stream
      and a prototype, lives in a member slot as a whole-cell observable signal whose partial write
      fires subscribers, and crosses a suspension as an activation-frame value. A component write is
      a whole-value rebuild stored back through the value's owner, so an observable partial write
      never bypasses the cell's update semantics -- the aggregate partial-update protocol a
      container reuses later.
- [x] **The dynamic array** (LRM 7.5) -- realized on the execution backend as a run-time-sized
      container value domain, the first variable-size aggregate. It defaults to empty, builds from
      `new[N]` / `new[N](src)` and an assignment pattern, copies with value semantics, takes the
      equality and case-equality families, reports its size, reads and writes an element (an
      out-of-range read yields the element default and an out-of-range write is discarded, LRM 7.4.5
      / 7.4.6), empties under `delete`, reads a contiguous range select as a fixed-size unpacked
      array and writes one functionally (LRM 7.4.6), lives in a member slot as a whole-cell
      observable signal whose element write fires subscribers, and crosses a suspension as an
      activation-frame value. An element write, a range write, and `delete` are functional
      whole-value updates stored back through the owner -- never an in-place mutation of a value
      reached through a possibly-shared handle -- so value semantics hold across a copy; this is the
      mutating-container protocol the queue and associative array reuse.
- [x] **The fixed-size unpacked array** (LRM 7.4.2) -- realized on the execution backend as a
      fixed-arity container value domain. It default-constructs member-wise, builds from an
      enumerated element list and from a replicated pattern through one repeat-unit-and-count path,
      copies with value semantics, takes the equality and case-equality families, reports its size
      and its bit-stream width and count, reads its bit stream out and builds one back from a stream
      and a prototype, reads and writes an element, reads and writes a contiguous range select,
      lives in a member slot as a whole-cell observable signal, and crosses a suspension as an
      activation-frame value. Its payload is ordinal-only: the declared range is the receiver's
      static type's and arrives at a select as its own operand, so a whole-value store copies
      positions and relabels nothing -- and a store between two arrays whose declared ranges differ
      lowers, because the range is gone by this layer, both sides are one type, and a type pool
      keyed by content says so. A range write rebuilds the whole value with that window replaced
      (LRM 7.4.6) and stores it back through the owner.
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
      defaults to empty, builds from a list of entries and the miss value a read of an absent index
      answers with -- which a `default:` states (LRM 7.9.11) and which is otherwise the element
      type's own default -- copies with value semantics, takes the equality and case-equality
      families, reports how many entries it holds and its bit-stream width and count, reads an index
      with no entry as the element default, allocates an entry on a write, reports whether an index
      has one, drops the entry an index names or empties entirely (LRM 7.9.3), answers the smallest
      and largest index it holds, lives in a member slot as a whole-cell observable signal, and
      crosses a suspension as an activation-frame value. It holds no prototype for an index -- the
      clause gives it no index bounds and no index default -- so an index crosses in the
      representation its own type names. What it does carry is the order its index type imposes (LRM
      7.8), which for every declared index type is the one the index values already carry and for a
      wildcard index (LRM 7.8.1) is not: that clause admits an index of any width, makes it
      self-determined and unsigned, and orders the entries by numerical value, so how two of them
      compare is settled by the declaration and absent from both indices. A wildcard index is also
      not a data type, so a construction cannot list its keys under the declared index type the way
      every other index type is listed: an assignment pattern's keys are self-determined and reach
      lowering at whatever width each was written at. They travel at the widest among them instead,
      which carries the same set of entries because the container reads a key by its numerical value
      across widths, and which no program can observe -- the clause withholds every way of reading a
      key back out, `item.index` in a `with` clause included (LRM 7.12.1, 7.12.4).
- [x] **The traversal family** (LRM 7.9.4 -- 7.9.7) -- realized on the execution backend. Each
      answers with the SV int the method reports and the index it visited, which is the probe
      unchanged where the array holds no such neighbour, and the call site stores that index into
      the variable the source named -- so the variable's own write path runs and its update event
      fires. `foreach` over an associative array and the index-ordered checks run through this.
- [x] **An unpacked concatenation** (LRM 10.10) -- realized on the execution backend: a queue is
      built from its parts, each contributing itself as one element or, spread, its own elements in
      order, so a queue grown or spliced through `{q, ...}` and one whose declared bound trims an
      over-long result both run here. No entry composes a part list of arbitrary length, so the
      concatenation is the left-to-right chain of two-operand appends it stands for, folded where it
      is built; a spread part crosses erased, its own container domain being no concern of the entry
      that appends its elements.
- [x] **The union domains** (LRM 7.3 untagged, 7.3.2 tagged) -- realized on the execution backend.
      An untagged union holds one member at a time, so its value is that member plus which one it
      is, and a member write makes the written member the live one; a tagged union adds a checked
      tag, so an access whose tag does not match the live one is a run-time error. Pattern matching
      (LRM 12.6) rides on the tagged form. One corner stays deferred: reading an untagged union
      member other than the one last written -- undefined in SV (LRM 7.3) -- is reported rather than
      returning that member's default, because only the live member is stored.
- [x] **The managed reference** (LRM 8.3, and the LRM 9.7 `process` a handle names) -- realized on
      the execution backend as a value domain: the object's address together with a share of its
      ownership, with the object's type erased, so one representation serves every object a handle
      can name. It defaults to null, copies as a value and so keeps its referent alive, takes the
      equality and case-equality families and the boolean test, and lives in a member slot as a
      variable written and read through its own storage. LRM 9.7 process control is what exercises
      it, since a handle to a process reaches the domain without building anything on the managed
      heap.
- [x] **A managed value across a suspension.** A variable of class type is storage its execution
      owns, like a variable of every other type whose value the body does not hold the whole of, so
      an object goes on being referred to while the process that named it is waiting and its
      properties read back what was written before the wait. This covers a variable of automatic
      lifetime, a subroutine's formal, and a local of a class method -- which is every local of
      every method, since a class method's lifetime is automatic whatever encloses it (LRM 8.6).
      What decided it is the standard's own split between a handle that keeps an object alive and a
      chandle that does not (LRM 8.4, Table 8-1): the claim on an object's life is part of a
      handle's value and no address carries it, so the body cannot hold the whole of one.

- [ ] **Reclaiming a managed value.** Precise tracing, which neither backend implements: what a
      handle keeps alive it keeps by shared ownership, so an unreachable cycle is not reclaimed on
      either. The storage a reference lives in is described, which is what root enumeration would
      walk, so what is left is the collector rather than a second home for the reference. Contract:
      `../architecture/lifetime.md`.
- [x] **A reference argument aliasing storage that is not a cell.** A reference names the storage
      its referent lives in, and that storage is of one of two kinds: a subscribable variable, where
      a write through the reference raises the update event the variable owes its subscribers, and
      storage nothing subscribes to, where it does not. Which of the two a reference holds travels
      with it rather than with its type, because a body that takes one is lowered once for every
      caller and so cannot ask what it was lent (LRM 13.5.2). So a signal, a declared variable and a
      class property are all lent, and nothing about the body a variable sits in or what that body
      does with it takes part. A formal that lends what it was lent hands on what it holds, so a
      chain of `ref` ports denotes the one variable at its end. An `output` / `inout` argument is
      not subject to this -- it copies out through the actual's own write path.

      Two things still refuse. A component of an aggregate is realized here as part of one value
      rather than as storage of its own, which the entry below covers. And reaching the storage a
      reference binds, rather than reading or writing through it, is not an operation here: the two
      kinds are different storage with one type between them, so an address taken through a
      reference would name whichever kind the type does not admit. Waiting on a `ref` port's own
      name is what asks for it (LRM 9.4.2).

- [ ] **A component of an aggregate is storage of its own.** The language gives a member of an
      unpacked structure and an element of an unpacked array an identity a second name may denote,
      independent of the position it sits at and of the value its parent currently holds
      (`../architecture/storage.md`). Three behaviours follow, and this backend offers none of them
      because it realizes an aggregate as one value and reaches a part of it by extracting and
      rebuilding. A component can be lent, so a `ref` actual may name a member or an element and a
      write through it reaches that component and nothing else. A whole assignment to a fixed-size
      aggregate writes into the components that are already there (LRM 7.6), so a reference to one
      goes on denoting it and observes the new value, rather than the store replacing the value
      those components were part of. And a variable-size container preserves every element's
      identity across an insertion or a removal at any position (LRM 7.10.3), with an element
      removed while a reference is bound going on existing for whoever holds it and its writes
      invisible through the container (LRM 13.5.2).

      The three are one question rather than three: whether a component's identity is independent of
      the value its parent holds. `../decisions/storage-owns-its-value.md` settles that a storage
      entity owns its value's representation and a component with identity is itself storage, and
      settles the fixed unpacked array and the unpacked struct; what a variable-size container's
      element storage becomes is deliberately still open there, as is how the IR names a component's
      storage at all.

- [x] **Writing into a local that holds a value rather than storage.** A local needs storage exactly
      when the body needs an address for it, and every way a body asks for one counts: assigning it,
      designating a part of it to write, and calling a method that changes it -- the last two
      because a local's storage is read and written whole, so both are a whole value rebuilt and
      stored back through the local. A local a body only reads stays the value it was bound to.

## Value realization: two tracks today, one native model deferred

The value layer is realized two ways, and the breadth work above runs against this split:

- The transitional C++ backend realizes each value type as a monomorphized target type -- the host
  C++ compiler expands one concrete type per element type, and an aggregate interior is written in
  place because that type owns real storage.
- The execution backend realizes each value as an opaque handle to a type-erased runtime object in
  its own frame (`../decisions/a-value-lives-in-its-makers-frame.md`,
  `../decisions/jit-aggregate-realization.md`): it emits generated code with no host compiler to
  expand a template, so an aggregate is one erased object and an interior write is a functional
  whole-value update.

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

- [x] **Calling a task.** Control returns to the caller only once the task completes (LRM 13.3), so
      the caller waits on another execution's end rather than on a wakeup source it registered
      itself -- the one wait whose end is a second body's to signal. A call creates no thread of its
      own (LRM 9.5 lists what does, and a call is not among them), so the callee runs in the
      caller's, becomes what a wait registered there parks and what the scheduler resumes, and hands
      the thread back when it ends. `output` and `inout` values pass at the return (LRM 13.5) into
      storage the caller allocated and handed over, which is why they are still there to read after
      the callee has stopped.

      Two things this settled reach wider than a task. **A caller allocates what it will read**: an
      execution's own storage is gone the moment its body ends, so anything read afterwards has to
      belong to whoever reads it -- the same rule that decides where an execution's cross-suspension
      values live, one level up. And a body that emits no landing has nowhere to catch a failure, so
      a failure is not one of the outcomes an execution settles at all
      (`decisions/run-time-failure-is-not-an-outcome.md`).

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

- [x] **Named events across a suspension** (LRM 15.5) -- realized on the execution backend. A named
      event is member storage the owner holds, a waiter set and the instant of its last trigger,
      reached only through its address and never read out as a value: `-> e` records the instant and
      releases every process parked on it at once, `@e` parks the running process until the next
      trigger, and `e.triggered` answers whether the most recent trigger happened in the current
      time step (LRM 15.5.3). The await is an ordinary registration whose suspension follows it, so
      an event a process parks on crosses a time step like every other wait. Rolled up in
      `processes.md` (P9).

## Other backend surfaces

- [x] **A runtime entry is named by its operation and typed by its call.** An entry's symbol comes
      from the operation, and its signature from the values the call passes, so neither is written
      down beside the other where the two could drift. Which entry a builtin resolves to is stated
      per builtin rather than inferred from what its operands happen to be, and what the library
      does not realize is stated the same way, naming which shape it has no entry for. An entry now
      exists as a prototype, a definition, and a binding held to each other by a check, so one
      written without the others fails the build instead of failing to resolve at run time. What is
      still refused says which shape it has no entry for: a named event's members have no storage
      realization.
- [x] **A memory load or dump (LRM 21.4, 21.5).** Every form the source may write runs here: an
      unpacked memory of any depth, a dynamic array, a queue, an associative array, each either
      running upward from an address or bounded by a window. A memory's words are taken out in
      address order, filled or rendered by the cores every memory task runs, and put back where they
      came from -- which is how an address the file does not reach keeps what it held, against a
      value nothing may write in place. What a memory addresses through is a property of the memory,
      so it is what names the entry: an unpacked memory reads the declared bounds of every
      dimension, whatever its nesting depth, a dynamic array or queue the dense space its current
      size spans, and an associative memory its own keys. A load answers through its completion,
      like every other service that reports through an argument.
- [x] **A body the runtime runs on the program's behalf, one alternative per call protocol.** Three
      run here: the effect a region runs once the stretch that built it has returned (a non-blocking
      assignment, a postponed print, a deferred assertion's action), the branch a `fork` spawns, and
      the `with` expression an array method evaluates once per entry of its receiver. Each
      alternative carries an entry of its own signature, because what a body is called with and
      answers is what its type says; the alternative a body is, is read off what its own invoke
      answers rather than recorded beside it. What no type carries is what C says nothing about --
      how the captured state is laid out, and which representation a result comes back in -- and
      those ride with the body as the record it was registered under.

      What a body captures is independent of what it is called with, so the captured state is one
      block whatever the body is, described by the same schema an object's properties and a scope's
      members are. A captured value is a copy taken where the body was built, so nothing a body
      reads points into storage that may already be gone. A `fork` branch differs in what becomes
      of that storage: entering the branch takes the captures rather than borrowing them, since the
      execution outlives the stretch that built them and nothing else owns them.

- [x] **Storage a block keeps for the branches it spawned.** LRM 6.21 gives a scope enclosing a
      fork-join block the lifetime of every process that block spawned, so a branch detached by
      `join_none` or `join_any` goes on naming the enclosing scope's automatics after control has
      left the block -- reading what the parent wrote after the `fork`, and writing what the parent
      reads later. Such a scope's declarations are one block of member storage, described exactly as
      an object's properties are, and what a branch captures is a hold on it rather than an address.
      A hold ends with whatever owns it, so the storage ends once the last branch and the declaring
      frame have both let go, and nothing releases one by hand on any way out.

      Counting holds is exact here rather than an approximation of reachability, and that is a
      property of the language rather than of the mechanism: a program cannot store a reference to
      an automatic, and LRM 9.3.2 bars a detached branch from naming a `ref` formal at all, so the
      only names into the storage are the frame that declared it and the branches spawned under it.
      Both of those edges run one way in time, so no cycle can form for anything to have to collect.

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

- [x] **A method that runs a body the call supplies, once per entry** (LRM 7.12). The locator,
      reduction, projection and ordering families run here over every unpacked container. A `with`
      clause is a callable value the call hands the entry, which enters it once per entry with the
      element and that entry's index and takes back the value it settled on; where the source wrote
      no clause the identity one is supplied, so one shape reaches every consumer. What crosses in
      each direction is what its own type names, and the one thing no type names -- which
      representation a handle is in -- is recorded where the thing it describes is registered: the
      body's answer on the body's definition, and the result shape the producer supplies on the
      call, which crosses erased because it follows the clause rather than the receiver. Erasing an
      index and an element makes them one type, so the algorithms are the ones the other backend
      runs rather than a second family beside them.
- [x] **Storage reached by name rather than through a receiver.** A cell the whole program shares is
      named by its linkage symbol, a place opens at that symbol and dereferences it, and the
      program's link resolves the name to the address the design's own storage sits at. A cell a
      class owns rather than an object of it is that same storage under a name qualified one step
      further, so it reaches this backend the way a namespace variable does and nothing about the
      class survives the lowering. What brings it up is whatever brings up the thing that replicates
      its declaration -- the declaring instance, or the declaring namespace's own two design-wide
      bodies -- so a class needs no startup body of its own, and every such cell takes both its
      declared representation and a value, the type's default where the source wrote none. Settled
      in `../decisions/type-associated-storage-is-the-declarers.md`.
- [x] **Where a base class's storage sits inside a derived object.** A member is named by the
      declaration that declares it together with the slot that declaration gave it, never by a
      position read against whatever the access arrived at -- so a class that redeclares a name its
      base already used no longer takes the base's storage for its own, and which of the two an
      access means is decided where the access is written (LRM 8.14). A class's storage is its
      base's extended with its own, so an inherited member keeps its position in every class
      extending it and an addition a base does not publish moves nothing. Settled in
      `../decisions/inherited-member-reference.md`.
- [x] **Reaching a member costs what reaching a field costs.** A member sits at a fixed distance
      from its value, derived when the unit is compiled, and a value holds its members in one
      allocation -- so an access is one address computation rather than a question put to the
      runtime. Only a class extending another unit's class reads how much storage its lineage
      carries ahead of its own, because a unit does not publish what it keeps to itself. Settled in
      `../decisions/a-member-is-reached-at-a-derived-offset.md`.
- [x] Calling an inherited method, and constructing the base. The receiver crosses without being
      re-typed to the class that declares the body, so an inherited method reaches the storage the
      object holds for what that body names; and constructing enters the base's construction first,
      on that same object, so what the base establishes is in place before any property initializer
      or constructor statement of the extending class can read it (LRM 8.7). The C++ backend never
      had to answer either, because the host language answers them. What a base construction carries
      is stated where the class is read, so nothing here establishes it: one narrower refusal
      remains, a base that left a formal to its default value, and it is raised before either
      backend sees the class.
- [x] Building an object whose constructor takes arguments. The runtime owns the heap, so it is what
      brings an object into existence; which body then initializes it is settled where the object is
      asked for, and the generated code enters that constructor the way it enters a base's.
- [x] **Which body a call reaches, decided by the object rather than by the call site** (LRM 8.20,
      8.21, 8.22). A value carries the bodies its class answers each behavior with, and a call names
      a behavior instead of a body: the runtime answers what class the value is, and the generated
      code enters the body that answer names. The split is the same one construction takes -- what
      only the runtime knows is the only thing that crosses to it -- so nothing generated depends on
      how an object or its class record is laid out. A class states what it adds to its lineage and
      nothing about the lineage itself: the behaviors it introduces, and the ones it takes over,
      each named by the declaration that introduced it, so a behavior keeps one position in that
      declaration and in every class extending it. A behavior introduced without a body (LRM 8.21)
      is a position nothing answers, which no value reaches because such a class is never
      constructed. An abstract class and its pure-virtual contract, a method defined out of block, a
      `super` call reaching past an override, and a virtual task all run here. A behavior a class of
      another compilation unit introduced runs here too: the introducer is found by walking what
      each class promised about the class it extends, so a call names it however many classes it was
      reached through. What is refused is a behavior an interface class states, which sits on no
      lineage and so has no position counted through one -- a class commits to several interfaces
      whose declarations are unrelated to each other and to its base, and two classes committing to
      one need not order them alike (LRM 8.26). Settled in
      `../decisions/dispatch-position-is-a-lineage-coordinate.md`.
- [x] **A class another compilation unit declares.** Reaching a property or a behavior on one is an
      ordinary access at every layer below the one that read the promise: the class is on that
      unit's signature, and the slot or ordinal is counted out of what it published. What a class
      promises is what it declares plus the class it extends, never what it inherited, so an
      inherited property or behavior is found by walking that chain -- and reading each promise on
      the way is what makes its unit a dependency. What a class keeps to itself (LRM 8.18 `local`)
      is on no promise and sits behind everything it published, so adding one moves nothing a
      referrer counted. Settled in `../decisions/reaching-past-a-published-class.md`. Constructing
      one is the same construction as any other: the allocation reaches the class's definition by
      the name its unit links it under and the constructor by that name and one more segment, so
      which unit declares the class decides where each answer is read and nothing after it
      (`../decisions/constructing-another-units-class.md`). Until that landed the construction fell
      out of the lowering entirely and the object came back with no property initialized, which is
      the one shape here that answered rather than refused. Holding a handle to such a class asks
      for no promise at all -- a handle refers to an object without reading anything its class holds
      -- so a hierarchical name that lands on such a variable resolves in either direction of the
      hierarchy, including on a class a design element declares, which no signature carries and none
      could. Reaching a property or a behavior through such a handle works as well: the name
      resolves at elaboration against the class the instance fixes, and what that answers -- a
      storage position, a dispatch position, or a body outright -- is what each access then uses
      (LRM 23.6, 8.14, 8.20). What still refuses is the type-associated storage of a class a unit
      declares, which no symbol names yet.
- [ ] `this` as a value in its own right (LRM 8.11), so an object can be returned, passed, and
      compared from inside its own method. A body holds a borrowed pointer to the object it runs on,
      which serves every member access; answering with a handle instead is what a shared-owner
      realization needs and a traced one does not, since there the handle is that pointer. So this
      waits on the reclamation model rather than on an entry: what it costs to add now is the record
      the tracing would make unnecessary.
- [x] **A compiled unit carries what it declares.** The definition a scope is driven through, the
      one every value of a class carries, a closure's, the description one body's variables need,
      and the storage a unit shares program-wide are all stated by the unit's own module, in a body
      whoever composes the program runs before the program starts. Nothing builds them from a
      lowered unit after compiling, and a class another unit declares is named by the cell holding
      its definition rather than matched to it by comparing linkage names across the program.
      Settled in `../decisions/a-unit-states-what-it-declares.md`.

- [x] **An artifact this path produces, rather than only a session it runs in.** `build` on this
      backend links the design into an executable that runs after the compiler has exited and
      wherever the kind of machine it was linked for does, reading its plusargs off its own argv and
      answering with the design's exit status, and `run` builds the same program and executes it.
      The program starts at one entry the backend emits from the design root. The session that used
      to compose a design inside the compiler is gone: measured, it was not faster on a first run,
      and a second run of the kept program pays no compile at all
      (`../decisions/a-program-is-kept-by-what-built-it.md`).
- [ ] An AOT CI job. Neither the smoke job nor the benchmark runs this path: both run against the
      C++ path, per merge and nightly respectively. The artifact they would time now exists; what is
      still missing is the job that drives it.
- [x] **An optimization pipeline.** A module goes through the toolchain's standard pipeline at the
      level the build asked for -- unoptimized by default, optimized under `--release`, as the C++
      path's design code is -- and that pipeline is also what makes a suspending body executable, at
      either level. A program built at one level is kept apart from the same design built at the
      other.
- [x] **As wide as the build was told, from lowering on.** As many units go from their semantic
      model to a module at once as `-j` allows, and as many modules are compiled to objects; what
      the units make is still collected in the order the design lists them, so the program is the
      same however many ran at once. Measured on the RISC-V core at `-j 4`: 7.2 s to build by
      default and 27.0 s under `--release`, where the C++ path takes 51.8 s and 679 s.
- [ ] **What optimizing the design's module buys at run time.** Nothing measurable yet: a loop of
      two million iterations runs in the same time at either level, because every operation on a
      value is a call into the runtime library, which is compiled apart from the module and cannot
      be inlined into it. Optimizing across that boundary is what the next saving waits on.
- [x] **An array of owned children.** A child scope -- a module instance, a generate block, a
      procedural block scope -- is constructed, reached by name and per-axis index, and reports its
      hierarchical name, whether it stands alone or is one of an array. Each element is its own
      child, told apart by the index its hierarchy segment carries, which is what a lookup by index
      matches against and what `%m` renders in brackets; what the declaring scope keeps is one
      member holding the sequence of handles on them, built once and read by coordinate. An index is
      an ordinary value of the design and reaches the runtime as the handle every value reaches it
      as.
- [x] **Driving a net.** A net's value is the resolution of its drivers, so a driver attaches to a
      resolution node and updates a contribution rather than writing a cell, and a net-bearing
      design runs here. A net is storage of its own: it fixes its declared type and its fold once,
      answers with the fold of its drivers' contributions, and takes no store at all. Every form the
      source may write resolves -- one driver, several, a driver contributing high impedance, a
      driver covering only part of the net, a net reached across a port, and a net whose data type
      is an unpacked aggregate -- and a change in the resolved value wakes its subscribers while a
      contribution that moves without changing it wakes nobody. Which fold a net uses reaches it as
      the install its construction names rather than being assumed below, so a second net type adds
      its own without disturbing this one. Rolled up in `nets.md`.

      Two things this settled reach wider than a net. **How an access through a capability wrapper is
      realized is one question asked at one place**, answering for every wrapper what a load, a store,
      and the install that fixes a declared representation each reach -- and which of the three a
      given wrapper defines at all, since a net takes no store and a driver installs nothing. And the
      value layer's erased half states the net fold its monomorphized half already had, so an
      aggregate is valid as a net's data type on both paths rather than on one.

- [x] **An open array whose actual is an unpacked array.** Imaging one walks the actual down to its
      leaves, which a monomorphized array ends by instantiating the walk at the element type. The
      erased walk ends where the value says it holds no elements by position, which is the same
      question asked of the value instead of of its type, and the image's own element shape comes
      from the actual's declaration as an operand rather than from an element the actual may not
      hold. An actual whose extent is fixed while the program runs -- a dynamic array or a queue --
      is refused by name on both backends: Annex H.7.6 reports each unsized dimension with the
      actual's own range and the image is built before the call from ranges the lowering states.
- [x] **What a loaded design knows about a scope.** The record a scope is built from carries every
      name space it answers a call in and its whole timescale, so a hierarchical name, a foreign
      name and a time query each read what that scope states rather than what the last consumer
      happened to need. It is assembled from the executable body plus the unit's source-level
      metadata, which is why a fact absent from either was reported as the runtime's default -- a
      scope with no unit of its own answered the DPI-C time queries with the simulation's precision
      (LRM 3.14.2.3, Annex H.13).
- [x] **Foreign code calling in.** An exported subroutine is reachable under the C name the standard
      fixes (LRM 35.4, 35.7), and a DPI task crosses in either direction. What this needed is that a
      program have one linker: the design's foreign sources are linked into the program rather than
      loaded beside it, so the link resolves names across everything it holds instead of the outward
      direction resolving in one place and the inward direction in another that cannot see it.
- [x] **A region that consumes a control effect** -- what a named block, a named fork, and a task
      need so that `disable` of one resumes execution after it (LRM 9.6.2). A named procedural block
      runs here whether or not anything disables it, a self-`disable` leaves its own region, and an
      effect naming an enclosing target passes outward through the regions that decline it. The
      region's extent is bracketed by two ordinary calls under a cleanup that runs on every way out
      of the body, so no backend has to run code at scope exit to hold the membership; where an
      execution regains control it asks the runtime whether it has been told to stop -- a target it
      is inside was disabled while it was away, or its own termination is owed because a foreign
      call it made had still to return -- and the comparison behind that answer stays in the
      runtime. Leaving is what the target's own scope exit does: a call that could be left that way
      names where to continue as well as where to return, and a body that claims nothing passes the
      departure outward, so it reaches a caller's region as readily as an enclosing one of its own.
      An effect no region claims anywhere settles the activation cancelled, and a body that can
      suspend is left by one the way it is left by returning, whether or not any path in it could
      reach its end: a process that waited and then ends the run, or a branch looping forever that
      disables the block around it, ends what it holds once. Disabling a named `fork` exercises all
      of it: the branches it spawned are enclosed by the target even though their bodies state no
      region, so the `disable` wakes one parked on a delay, that branch settles cancelled where it
      regains control, and the process that entered the block resumes after it in the same time
      step. `disable` of a task runs here too, now that a task enable does, and so does one written
      inside a function and naming a block outside it.
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
      it means admitting a module against what the runtime realizes before it is linked.
- [x] **End-to-end coverage is the corpus, not a handful of cases.** What this path refuses is
      recorded once for the path rather than on any case, and a case that starts running fails until
      its entry is dropped. So the record only ever shrinks, and dropping entries is what landing a
      construct looks like -- in the same change, the way a checkbox above is flipped with the code
      that closes it. Absence of an entry is a claim the run checks, which is what makes the record
      a measurement rather than an assertion.

- [x] **A check that only a failing design reaches.** A guard the language requires to run as part
      of evaluating an access (LRM 11.3.5) -- reading a tagged union's member against a tag it does
      not hold is the one that has it (LRM 11.9) -- is realized here, and a design that trips it now
      reports the failure where it used to refuse to lower.

      **The gap was a naming claim rather than a missing function.** The guard was filed as realized
      once per representation of the value it acts on, which asserts a family of entries exists. It
      reads its condition and yields what it was handed, so it acts on no representation at all: one
      realization serves every value, and serves a cell an access reached through as well. Filing it
      as that one turned a family nobody had written into a single entry.

      **The second thing is a boundary convention, and it is the one that cost the time.** The text a
      guard raises crosses as a pointer to a constant rather than as a value of the string domain,
      which is what a literal argument to any entry does. Reading it as a string value yields a view
      over whatever the bytes happen to say, and the failure lands as the host running out of memory
      while building the message -- a symptom that names neither the entry nor the operand. An entry
      whose operand is a literal says so in its prototype, and that is the only place it is said.

      **What is worth more than either is that the corpus cannot see this class.** Every conformance
      case is a program that passes, and what a guard reaches is a program that must not, so no case
      can hold it and the refusal record has no entry to shrink. It surfaced from a command-line case
      asking how a failing design is reported. So that record measures what this backend refuses to
      compile and says nothing about what it refuses to check, and the second question still has no
      instrument.

- [ ] **What this backend refuses to _check_ is not measured by anything.** The refusal record is
      the measurement of what it refuses to compile, and it works because a conformance case is a
      program that passes: a case that starts running fails until its entry goes. A check a design
      trips is reached only by a program that must not finish, so no case can hold one and the
      record has no entry to shrink. The one gap found this way was found by a command-line case
      asking how a failing design is reported, which is to say by accident.

      What it needs is a way to enumerate the checks a design can fail and ask this backend for each
      -- the checks are stated in the semantic layers rather than discovered, so the list is
      derivable rather than guessable. Until then, the honest reading of a clean refusal record is
      that it covers one of the two questions.

- [ ] **Running a design selects this backend without being asked.** Today it selects the C++ one,
      so the ordinary way to see what a source does compiles emitted C++ with a host compiler and
      runs the program -- for an answer this backend gives from its own code generator in a fraction
      of the time. That is the edit loop the whole compiler is optimized for, and the cheaper of the
      two paths is not the one it takes.

      What it costs is the entries in this path's refusal record. A design that trips one gets a
      diagnostic naming what is missing where it used to get an answer, and the other path still
      answers it by name. That is the pressure the record is for, and it is the reason to make the
      change while the record is short rather than after it has been short for a while.
