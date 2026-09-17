# A declared variable is one storage

Date: 2026-09-16 Status: accepted

## Context

The lowering into the execution IR gave a body's variable one of three homes: a slot of the
generated frame, a cell of the running execution's own store, or a cell built where the declaration
runs. Which one a variable got followed from two questions about the body rather than about the
variable -- can this body suspend, and does anything in it lend this variable by reference.

The first two homes are mutually exclusive and each has half of what a variable needs. The frame
slot has an address a reference can name and does not survive a suspension, because what it holds is
a handle into storage released when the body next hands control back. The execution's store survives
a suspension and is reached by the calls that read and write it rather than by an address. The
lowering asked the suspension question first and let its answer stand, so a variable that needed
both got the one without an address.

Every SystemVerilog process suspends. So lending a variable was refused in every `initial` and every
`always` there is:

```systemverilog
initial begin
  automatic int n = 7;
  bump(n);          // refused
end
```

The second question was answered by a pass that read the whole body before any of it was lowered,
looking for a reference built over each variable.

## Decision

**D1. A declared variable is one storage, and its declaration is the whole of what decides that.**
Neither the body it sits in nor what that body does with it takes part. There is no second home to
choose between and nothing to ask.

**D2. That storage is owned by the execution that declared the variable.** It is opened before the
body's first statement and ended on every way out of the body. A declaration reached many times is
one storage whose contents begin afresh, which is what its own initializer does.

**D3. Ending a variable is emitted by the compiler, on every way out, including the way no statement
spells.** A body left where it stands -- ended by whoever drives it rather than run again -- runs
none of its statements, so what it owes cannot be one. The suspension names that way out and what
runs there is what the open scopes owed.

## What decides D3, because the obvious argument expires

The storage this backend gives a value is owned by the runtime, so an owner that dies with the
execution would end every variable in it and the compiler would need to emit nothing. That argument
is true today and it is not a reason: values are runtime-owned because the compiler cannot yet lay a
value's bytes out (`jit-value-realization.md`), and that is a thing to build rather than a
condition.

The test that survives it: **does this answer change once values are laid out in the frame?**

| answer                                                   | after that work |
| -------------------------------------------------------- | --------------- |
| a declared variable is one storage, from its declaration | unchanged       |
| the owner's death ends everything, nothing is emitted    | **changes**     |
| the compiler emits the ending on every way out           | unchanged       |

The middle one is the answer that expires, so it is not the one to write down. Today the way out it
names runs nothing, and that is a fact about how values are realized rather than about what a body
owes.

## Survey

**Ending a suspended execution splits the field in two, by what kind of ending it is.**

- **A compiler-emitted ending -- a destructor, drop glue -- runs without resuming.** A C++20
  coroutine's state records its suspension point precisely "so that a resume knows where to
  continue, and a destroy knows what local variables were in scope"; destroying it runs those
  destructors, frees the state and returns to the caller, and no statement of the body runs
  (<https://en.cppreference.com/w/cpp/language/coroutines>). Rust generates a separate drop function
  over the locals its transform moved into the coroutine object
  (`rustc_mir_transform/src/coroutine.rs`).
- **A program-written ending -- a `finally` -- resumes the execution so the program's own statements
  run.** Closing a Python generator "raises a `GeneratorExit` exception at the point where the
  generator function was paused", which is what lets pending `finally` clauses execute
  (<https://docs.python.org/3/reference/expressions.html#generator.close>). Kotlin cancels the same
  way, by throwing at the suspension point.

SystemVerilog has neither a destructor nor a `finally`, so a variable's ending is only ever the
first kind, and `disable` -- which the language does define, and which decides where the process
goes next -- is only ever the second. The two mechanisms are not duplicates of each other, and this
decision covers only the first; `disable` is reconciled where an execution regains control and is
unchanged.

**Every variable is storage a process may wait on.** LRM 9.4.2 synchronizes a procedural statement
with a value change of an _expression_, and the grammar's event expression takes an expression with
no restriction on what declared it. So storage that can be waited on is what the language grants
every variable, and giving only some of them that property would be a saving taken from a reading of
the whole body -- the same shape as the pass this removes.

## Rejected

- **Repairing the two questions rather than removing them.** Teaching the lowering to give a lent
  variable a home that also survives a suspension answers this program and leaves the shape: a
  variable's storage still follows what its body does, so every later construct that lends a new way
  reopens the same hole, and its failure mode is a refused program rather than a slower one.
- **Stating which occurrences lend, in a semantic layer, so the translation reads the answer.** The
  fact is not a semantic one and, more to the point, nothing needs it: a reference binds the storage
  the variable already is.
- **Choosing the home from whether the body can suspend.** That is a property of the body, and two
  identical declarations in different bodies would get different storage -- which
  `../architecture/lifetime.md` names as recording a lowering constraint as a semantic class.

## Consequences

- Lending a variable works in a process body, which is where SystemVerilog puts procedural code.
  Measured: of the six cases the execution path recorded against lending, two run now. The other
  four stop on the causes that message also covers -- a member owning its value, and a part of a
  value aggregate -- which are storage questions of their own.
- The pass that read a whole body before lowering it is gone, and with it the last question the
  translation answered from anything but the declaration in front of it.
- **A silent wrong answer went with it, and how it was found is the part to keep.** The set of types
  a variable gets storage for had been the set that crosses a suspension, and it omitted the
  containers -- a queue, a dynamic array, an associative array, a fixed-size unpacked array -- and
  the unions. A process declaring one and using it after a delay was dropped without a word: no
  output, no diagnostic, exit zero, so every gate stayed green over it. It was not found by running
  anything. It was found by reading the sentence above the set against the set, noticing they named
  different things, and only then writing the program. Both a clean upstream tree and the cut before
  this one fail that program identically, so the omission is older than any of this work.
- The storage a body's variables live in is stated the way a declaration's members already are: a
  described list the host builds, and one block per execution realizing it. A target whose own
  language gives a declaration storage states no variables and builds nothing.
- Every variable now costs storage the runtime owns, including one nothing ever lends. That saving
  is available in full to whoever lays values out natively or removes what nothing reads; nothing
  measures this path today, so the cost is stated rather than measured -- which is the second cut in
  a row to have to write that sentence.

## Cross-references

- `a-declared-local-is-storage.md` -- the same pass answering the other question, removed one cut
  earlier and for the same reason.
- `reference-binds-a-cell.md` -- what a reference binds, and the reopening that named a lending
  requirement deciding a place's representation as the thing to undo.
- `storage-owns-its-value.md` -- a storage entity owns the representation of its current value.
- `cross-suspension-value-storage.md` -- the cell this replaces for a variable, and the gate on
  non-managed types that is no longer how a home is chosen.
- `jit-value-realization.md` -- the opaque-handle baseline, and why leaning on it would have dated
  this decision.
- `../architecture/storage.md` -- a variable has storage identity, and a reference aliases without
  owning.
- `../architecture/lifetime.md` -- the automatic regime, and why placement is not part of one.
