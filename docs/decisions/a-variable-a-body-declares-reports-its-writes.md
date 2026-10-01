# A variable a body declares reports its writes

Date: 2026-09-29 Status: accepted

## Context

A wait names what it watches by where elaboration put it: a route out of the reader's scope to a
cell on the design hierarchy. A variable a call or a block declares has no such place -- LRM 6.21
confines a reference to one to procedural blocks and bars reaching it hierarchically -- so a wait on
one was routed anyway, failed at time zero looking for a signal that does not exist, and the
execution backend then aborted. That held for every variable of a body's own lifetime: a task's
local, an `automatic` local of an `initial`, a `for` loop variable, a class method's local, and a
formal of any direction, `ref` and `ref static` included. A sampled value function over one was a
compiler internal error, because the history it keeps is evaluated by a process the scope
synthesizes, which no variable of a body is reachable from.

Two more defects sat under the first. The C++ backend kept such a variable as plain storage unless a
detached fork branch captured it, so a reference to one held nothing to report to, and a write
through it woke no one. And a `ref` port's wait registered on the cell its reference binds, which
exists only where the reference names a whole variable.

## Decision

**D1. Every variable a body that can wait declares lives in a cell that reports its writes, as a
design variable does.** Any process that can reach a variable may wait on it (LRM 9.4.2), and a
body's variable is reached by the branches the body forks and by the subroutines it lends the
variable to (LRM 6.21, 13.5.2). A function cannot wait (LRM 13.4.4), a subroutine it lends a
variable to is a function too, and a branch it forks outlives it and so holds what it names in
storage of its own -- so a function's variables have nobody to report to and stay values. What
decides is the declaration and the kind of body it sits in; nothing about what the body does with
the variable is read. MIR states it as the variable's type, so the two backends stop deciding it
separately -- the execution backend already gave a cell to every local whose value the runtime
keeps, and the C++ backend gave one to none.

**D2. A wait may watch a variable the running body has declared where the wait stands, named by that
declaration.** A route is for storage elaboration sealed; a body's variable is found in the frame
that declared it. What a wait's own region declares is not there yet: an implicit event control
reads its whole statement (LRM 9.4.2.2) and a called function reports its whole body, but an
automatic either of them declares comes into being only once it is entered and cannot be named from
outside (LRM 6.21), so nothing writes it while the wait stands and it is left out; a static one
exists throughout and is watched. Nothing writes a foreach index, an array method's iterator or a
pattern's binding while a wait stands -- the first is read-only (LRM 12.7.3), the second exists only
inside its method's expression (LRM 7.12), the third is set by the match (LRM 12.6) and the front
end refuses any other write -- so no wait watches one wherever it stands.

**D3. A wait on storage reached through a reference registers on whatever a write through the
reference is told to** -- the variable, or the object a property belongs to (LRM 13.5.2, 9.4.2), and
nothing where the storage belongs to nothing. The reference's own bits are not its holder's, so such
a leaf watches the whole of the holder, and the change of the waited expression decides the event.

**D4. An automatic variable's sampled value is its current value, and so is its past value (LRM
16.5.1).** A `ref` formal that is not `ref static` is sampled the same way, being usable only where
an automatic variable is (LRM 13.5.2). No cell is armed for one and no history is kept for an
expression reading only such variables.

## Survey

- **Verilator** (5.045, run here on the case this entry adds): answers a wait on a `ref` formal and
  on a lent automatic local correctly, by inlining the task per call site, so the formal resolves to
  its actual at compile time, and by giving a task's automatic locals static storage (the generated
  `__Vtask_<scope>__<variable>`). Lyra compiles a subroutine once for every caller and keeps
  recursion, so the actual is known only at run time and the reference has to carry what reports its
  writes. That difference is structural, not unbuilt.
- **SystemC** (Accellera reference implementation, `sc_signal_ports.h`):
  `sc_in<T>::value_changed_event()` is `(*this)->value_changed_event()` -- the event belongs to the
  channel the port is bound to, and the port only forwards. Only a channel has an event. D3 is the
  same answer, and D1 the same rule: storage a wait can name is storage that reports.

## Rejected

- **A cell only for a variable another process can reach**, decided from whether a fork branch names
  it or a `ref` actual lends it. The facts are all in the declaring body, since nothing outside it
  can reach the variable, but the answer is then a summary of the body rather than of the
  declaration -- the shape
  [a-declared-variable-is-one-storage](a-declared-variable-is-one-storage.md) removed.
- **Plain storage plus one coarse source told of any write to plain storage**, which
  [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md) D3 permits.
  Every direct write to a variable a branch or a callee may wait on would still have to report,
  which is what a cell does already, only coarser.
- **Registering a wait on the cell a reference binds.** It exists only where the reference names a
  whole variable, so a reference to a part or a property had nothing to register on.

## Consequences

- Waiting on any variable of a body's own lifetime works on both backends, and so does waiting on a
  `ref` formal lent a variable, an element, a member or a class property, and on a `ref` port.
- A closure's snapshot of a variable -- what a fork branch captures by value -- is the value rather
  than a cell: it is the closure's own copy and no other process reaches it.
- The execution backend still gives a value a function declares a cell of its own where the runtime
  keeps the value, for the storage reasons
  [a-body-holds-a-value-or-its-execution-stores-it](a-body-holds-a-value-or-its-execution-stores-it.md)
  gives, and that cell is the reporting kind. It reports writes nothing can wait on; the C++ backend
  does not, and neither is wrong.
- This extends
  [a-body-holds-a-value-or-its-execution-stores-it](a-body-holds-a-value-or-its-execution-stores-it.md)
  rather than revising it: where a variable's storage lives is still that entry's question, and a
  `chandle` local of a body that can wait is now a cell too, so a wait on one is answered rather
  than refused.
- Still refused: a sampled value of a `ref static` formal, which the front end's output does not yet
  distinguish from a plain `ref`'s, and a history function (`$past`, `$rose` and the rest) over an
  expression reading both an automatic variable and one the design element holds, which needs the
  history kept per variable rather than per expression.
- A variable of a process or a task on the C++ backend is a `Var<T>` where it was a bare value: a
  subscriber record, one pointer of rare state, and a test on each write for whether anything waits.
  Measured with the benchmark runner (C++ backend, `--release`) against `664a06a6`:
  `representative/compute-block` 4,112 to 3,861 table passes/s, `control-flow/loop-tight` 1,642 to
  1,684 passes/s. Making a function's variables cells as well cost `control-flow/call-chain`
  2,700,101 to 1,455,307 iterations/s, which is what the function half of D1 is measured against;
  with it the case is 2,859,657.

## Cross-references

- [a-lent-part-carries-its-variable](a-lent-part-carries-its-variable.md) -- a reference carries
  what holds its storage, which is what D3 asks.
- [object-is-an-event-source](object-is-an-event-source.md) -- a property's writes report to its
  object.
- [sampled-value-and-its-clock](sampled-value-and-its-clock.md) -- the history a sampled value
  function keeps, which D4 leaves out.
