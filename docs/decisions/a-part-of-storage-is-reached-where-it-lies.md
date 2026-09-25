# A part that is storage of its own is reached where it lies

Date: 2026-09-24 Status: accepted. Realizes [storage-owns-its-value](storage-owns-its-value.md) on
the execution backend; supersedes, for parts that are storage, the functional update of
[value-projection-write](value-projection-write.md) D2 and the immutability of
[jit-value-realization](jit-value-realization.md) invariant 6.

## Context

On the execution backend a read of one element copied the whole variable out of its storage and then
took the element. A write copied the whole variable out, rebuilt it around the element, and stored
the whole back. The representative compute block ran 42 table passes a second there against 4,650 on
the C++ backend (2026-09-24, both optimized). A profile put about 97% of the time in those whole
copies. The variable in that block is a module variable, so where a variable lives was never the
cause. The cause was how an access to a part of it was lowered.

MIR already states the access the C++ backend renders. A read of a cell and of a part is a view,
`to_owned` marks where a part is kept, and a write opens the cell and assigns through the part. The
C++ backend spells that `a.Get().Element(i).ToOwned()` and `*a.Mutate()`. The execution backend
could not follow it, because the runtime owned every value behind a handle the generated side only
copied. Since a value is an object in its maker's frame
([a-value-lives-in-its-makers-frame](a-value-lives-in-its-makers-frame.md)), that reason is gone.

## Decision

**A part that is storage of its own is a step of the place holding the value it is part of. A read
through a place answers with the value where it lies, and a write lands in the object already
there.**

- **Which parts are storage is the language's fact, stated once per value domain.** An element of an
  unpacked array, a dynamic array, a queue or an associative array, and a member of an unpacked
  structure, are storage (LRM 7.2, 7.4, 7.8, 7.10). A bit or slice of a packed value, a character of
  a string and a member of a union are views of their whole. MIR-to-LIR reads that fact to choose
  between a place step and a value projection. Nothing else asks it.
- **LIR states an element and a component as place steps, beside member and dereference.** An
  element is named by the coordinate the program computed, a component by its position. The backend
  realizes each step through the library, as it realizes a member step, so no layout crosses into
  generated code. Which element a step reaches depends on the access. A read of a missing element
  reads the default. A write appends at a queue's one-past-the-end, allocates a missing associative
  entry, and otherwise lands where nothing reads it.
- **A load answers with the value where it lies.** Nothing is copied until a value is kept -- a
  store into a slot, a return, a `to_owned`, or a postfix step's old value read past its own write.
- **A write into what a wrapper holds is an open write.** It is an object in the writer's frame,
  opened on the cell, reference or driver, and the write's parts land in its storage. MIR types what
  opening a wrapper answers with as that object rather than as an address, so the layer that gives a
  value its end reads it off the type instead of recognizing the write. It is ended with the
  full-expression and on every departure, which is when the wrapper is told once what the write did
  (LRM 4.3). It holds a before-image only while something observes the cell. While a procedural
  continuous assignment is in effect, the write lands where nothing reads it (LRM 10.6), as a
  whole-value write is discarded. A local whose storage is a cell of the execution's store is opened
  the same way when a part of it is written, since the cell reports its writes whether or not the
  source's plain variable says so.
- **A store through an element or component step is an assignment into the object there.** It is not
  the end of one object and the start of another, so storage that names the part goes on naming it
  (LRM 7.6).
- **A method that changes its receiver changes it where it lies.** Push, pop, insert, delete, sort,
  `putc` and a shift-assign each act on the storage the receiver names. A receiver that is a view is
  read out, changed and written back, as any write to a view is.
- **A value no place holds is given one.** A part of a call's result, or of a value a view reached,
  is reached in a frame slot the value is bound to for the rest of the full-expression, which is how
  a C++ temporary gets an address. A write to a member of a structure held in a union member lands
  in that slot, and the union is rebuilt from it.

## Rejected

- **A call answering the part's address, dereferenced as a pointer.** It is what Rust does for a
  library container: `a[b]` is `*Index::index(&a, b)`. Here a dereferenced pointer to a value
  already means a value cell -- the storage of a class property, a namespace variable, or a property
  reached by position -- and the backend reads and writes a cell through the cell's own entry. So an
  element's address typed as a pointer could not be told from a cell's. A place step says which it
  is.
- **Making a variable an object of the frame.** It was the first answer written down, and it is not
  the cause: the measured variable is a module variable, which no frame holds, and an automatic's
  storage in the execution's store was already addressable.
- **Keeping the functional update and making copies cheaper by sharing.** A write to a shared value
  still copies the whole, so the cost stays proportional to the variable.

## Consequences

- compute-block, `--release`, 2026-09-24: **2,870 table passes a second on the execution backend,
  4,340 on the C++ backend** the same day.
- An observed variable still copies its whole value for the before-image of each write, on both
  backends, where what changed is answerable at the part written. That is not yet done.
- Lending a component to a `ref` formal is still refused. A write through such a reference into a
  component of a subscribable variable has to reach the variable as well, and a reference carries no
  way to say that yet. The place step and the assignment are what lending would use, so closing it
  does not change this decision.
- A queue's and an associative array's element identity across insertion and removal is a matter of
  their representation, and is not settled here.
