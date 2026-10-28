# Value representation

Moving every value to the representation
[a-value-is-its-machine-data](../decisions/a-value-is-its-machine-data.md) decides, and port
connections to
[a-port-shares-its-sources-storage](../decisions/a-port-shares-its-sources-storage.md). Done when no
value carries a description of its own type, every operation on a packed value is code generated for
its type on both backends, nothing the runtime does reads a type description, and a write to one
element of a value -- through ports or not -- costs the element.

Each phase replaces one concept completely and deletes the shape it replaces, leaves the whole
corpus green on both backends, and is measured once on Ibex by instruction count, beside Verilator,
against the run before it; the figures are in the pull requests that measured them.

## Phase 1: what is specific to a type is generated, not described

- [x] A runtime facility that acts on a value of any type -- formatting, DPI conversion, file and
      memory-image reading and writing, sampled history, a wait's comparison -- is handed the
      functions the compiler generated for that type. A value held beside its type's constant
      survives on the execution backend only where an associative array holds its key.
- [x] A write reports the range it reached, and a wait decided at the write tests that range against
      what it watches on the words, without materializing either side.
- [x] On the execution backend, a queue, a dynamic array, an associative array and a fixed-size
      array hold their elements as raw storage acted on through the element type's generated
      functions, and a value whose representation an entry cannot know crosses as itself and its
      type.

## Phase 2: a packed value is its words

- [x] A packed value is its value words, followed by its unknown words when four-state, in one
      contiguous sequence of bits; a type is identified by its exact width, signedness and state
      domain, on both backends.
- [x] Every operation on a packed value is generated for its type. On the execution backend it is
      instructions at the value's own width wherever every value it takes and answers is small
      enough for a frame; a product, a quotient and a remainder past 64 bits, and any operation over
      a larger value, are calls taking the words.
- [x] An element or a member at a run-time index is reached by bit addressing at the cost of the
      element, whatever size the array is, on both backends.
- [x] A write to part of a wide packed value costs the part wherever the value lies, a subroutine's
      own local included.
- [x] A write to part of a part of a packed value names only the bits written, so it changes no
      other bit. Writing one bit of a two-state member of a four-state packed structure whose bits
      are x leaves the member's other bits x on both backends, in a variable and in a subroutine's
      local (LRM 7.2.1, 11.5.1). The execution backend used to read the member out as two-state and
      write it back, which turned those bits into 0.
- [ ] Reading a part of a part of a packed value costs the part read. It costs the outer part on
      both backends.
- [x] Writing a character of a string changes that character where the string lies, on both
      backends, as the string's own `putc` does.
- [x] Writing a member of an unpacked union costs the part written, on both backends: the store lies
      where the member lies, and the write keeps and compares only what it lands on below the member
      while reporting the change to the whole union.
- [ ] No one-word operation costs more than it did while the value library's word functions were
      marked to be inlined always, and a shift of a value of several words stays inline in an
      optimized C++ build. Under clang at `-O2` the one-word operations together cost fewer
      instructions than before, but an XOR and an XNOR reduction of a four-state value cost more,
      and a right shift of a value of several words is a call where it was inline.
- [x] A value is its words at every width, and a value too large for a frame is held through a box,
      so what a call takes from the stack does not follow a declared width. The widest packed type
      the front end accepts (LRM 6.9.1, 7.4.1): a design declaring one, adding to it, shifting it,
      writing a part of it and passing it through a function builds and runs on both backends under
      a default stack. One bit wider is refused by the front end, which names the limit.
- [x] The size past which a value is kept apart from a frame is decided by what a frame may hold and
      not by speed.
- [x] A run of words a holder of a wide value released is found again by its length at the same cost
      however many wide lengths a design has; two lengths that share a slot evict each other.
- [ ] A design building more holders of one wide length at once than are kept keeps only that many
      of their runs, and the rest go back to the system each time. Not measured.
- [ ] An operator on a value wider than a word writes its answer once. Each clears the answer and
      then writes all of it, a second pass over the value. Not measured.
- [ ] An operation over a value wider than a word is carried out several words at a time by every
      host compiler. Under gcc its loops go a word at a time. Not measured as a rate.
- [ ] The size and the number of runs kept are checked against the wide cases of the benchmark
      corpus. Neither has been.
- [ ] A fill or an extension of a type too large for a frame is carried out each time it is
      evaluated, a pass over the value, where naming a constant was a copy of the same length. What
      that costs a design evaluating one often is not measured.
- [x] A local, a parameter taken by value, and an argument handed to one, of a type too large for a
      frame, is held through a box the compiler states in MIR before either backend reads it, so
      both keep the value off the stack by translating what they are given. A parameter is boxed
      wherever its signature is stated, so a call through the address of a body passes what the body
      takes. A variable's cell keeps such a value in storage of its own on both backends, as every
      holder does, by the one rule every holder reads.
- [x] A frame keeps a body's values only up to a fixed sum, and a local, a variable's cell or a
      value an expression makes past it is held through a box: a function, a process before its
      first wait and a branch it forks, each declaring locals as large as a frame keeps one and
      together many times the stack, run on both backends under a small stack. A frame still follows
      how many operations and arguments the source writes, as any compiled program's does, so an
      expression of hundreds of operands on values as large as a frame keeps one needs a stack of
      the size such a program would.
- [x] A list of elements an array is built from is made, elements and all, in its box where it is
      too large for a frame, on both backends.
- [x] A value of such a type that an expression makes is made in a box and read through it, a member
      of one is read through its box, and the arm a condition chooses, or the value a block of steps
      ends with, is made where the condition's or the block's value is. A value a box is built from
      is made in the box's storage on both backends, never in a frame first, so a call answering one
      makes it in the box. What a suspending call completes with is kept out of every frame on both
      backends where it is too large for one.
- [x] A closure capturing such a value holds it through a box, which the closure owns and ends, and
      a closure is as large as its captures as it holds them, so one whose captures together pass
      the bound is itself held through a box.
- [x] A structure's operations on its whole value -- equality, case equality, the unknown tests, the
      stream conversions, a net's resolution -- refer to the structure they are asked of and to
      every operand of the same type where it lies, as Rust's comparison operators borrow theirs; an
      operand the program computes is held for the reference to the end of the full-expression, in a
      box where it is too large for a frame, and the operations' own bodies keep what they hold by
      the same rule as any body.
- [x] The execution backend keeps no value apart from the frame on its own account, and the compiler
      refuses a body that leaves a value too large for a frame unboxed. A receiver an operation is
      asked of is boxed as any operand is, while what a write lands on is carried as the place it
      names.
- [x] An integral, a fixed-size array and a structure are laid out flat at every size on both
      backends, and no operation of the value library builds a whole value of its own on the stack:
      what it makes it builds where its caller keeps it, and what it must keep aside while it works
      it keeps in storage of its own. A callable answering a value makes it where its caller takes
      it. A design whose every value is twice the stack it runs on runs on both backends.
- [x] A union is laid out flat on both backends.
- [x] A wait on part of an unpacked aggregate is passed over by a write that reached another part of
      it, and a write visits only the waits on what it reached.
- [x] On the C++ backend a fixed-size unpacked array is its elements in place at every size, the
      count part of its type. An element keeps its address across every assignment on both backends,
      so a reference to one (LRM 13.5.2) goes on naming it.
- [x] On the execution backend a fixed-size unpacked array is laid out in place from its type: its
      elements one after another where the array lies, at every size. A read of an element is
      arithmetic on where the array lies, with a call only for an index naming no element. A write
      into an element of a variable is a call for each level of the select, which is where the
      variable is told the part the write reached. A variable, a net and a history holding one hold
      it with its type, and a reference to one and a part designated within a write are told the
      type at each access. Building one takes its elements and nothing its type states, on both
      backends.
- [x] What a position of an array holds before anything is written to it (LRM 7.4.5) is stated once
      for the element's type, for a fixed-size array, a dynamic array, a queue and an associative
      array alike, on both backends
      (`../decisions/an-arrays-element-default-is-its-element-types.md`). Building one, sizing one
      with `new[]`, converting one array kind to another, and a `map` or a locator answering one
      take no default, and a queue, a dynamic array and an associative array hold no copy of one. An
      associative array literal's own `default:` (LRM 7.9.11) is still handed over where the source
      wrote one. Not measured.
- [ ] An operation on a whole array on the execution backend is carried out inline where every
      element is its bytes. Comparing two, and each net fold, is a call that asks the element's type
      for every element. Not measured.
- [x] Constants are compile-time constants on both backends, and the execution backend lays out a
      value, a cell and a frame slot of an integral up to 64 bits from the type.
- [x] Whatever holds an integral wider than 64 bits -- a variable, a net, a sampled history, a
      procedural local -- holds the value's words and is handed no type. A variable, a net and a
      procedural local hold the words in their own storage, which the code generator lays out at the
      size the type gives, and are told the width where they are built, so the value has one address
      from then on, the one every reference to it names; past the size a frame may keep, that
      storage holds the address of words allocated once, where it is built. A history is told the
      width where it is installed. A reference, and a part designated within a write, are told the
      width at each access, and a store into a procedural local is told nothing.
- [x] A whole write of a value wider than 64 bits compares the arriving bytes with the words the
      holder has and takes them there: a store into a variable, a push into a sampled history and a
      driver's contribution. A force keeps each value its evaluation produces apart from what it
      covers, since it may hold only some of its positions.
- [x] A comparison answers 0, 1 or x as one scalar wherever it is carried out, and generated code
      stores it at the type the language gives the answer. A net's positions are words and a count.
      No record of an integral type is made while the program runs.
- [x] What the library is told of an integral operand -- its width, whether it is signed, whether it
      can hold x or z -- is declared with the routine that reads it, and a call is composed from
      that declaration.
- [ ] Every call on the library is composed from one declaration and nothing else. An entry is over
      values of one kind, states the representations it is realized for and what each realization is
      told, and a call on any other representation is refused where it is composed. The type a
      generated module declares an entry at follows from how the call is arranged, and a name
      declared at two types is refused. An operand an entry reads as a machine integer is still
      converted by hand where each call is built, and the count of a run of array elements, which
      the run's type fixes, is still an operand of reading one.
- [x] The instructions the execution backend writes for an operation are held to the library's
      statement of the same operation by a test that evaluates both on the same operands, at widths
      on both sides of 64 bits.
- [x] A net of an integral type has one install on both backends, and counts its positions by its
      declared type.
- [x] An enumeration's member table states the base type it is laid out at, and the library reads
      how many words a member takes from the table.
- [ ] Every operation over constants is folded where it is built, except where folding would make
      data the source did not write: an operation whose answer is too large for a frame and wider
      than its operands together stays an operation, and a wide value that is a few bits extended is
      stated as that extension. An operator, a cast and a system function over constants are folded,
      and one left unfolded where it would fold is refused. A conversion of constant bits to a
      string and the text of a constant under a format are still folded by functions of their own,
      an enumeration method called on a constant is not folded, and an enumeration's members are not
      constants the unit names.
- [x] A variable, a net, a class's property and what a suspending call answers into, of an unpacked
      structure or a fixed-size unpacked array, are told its type where they are built and keep the
      value in their own storage, holding a valid value of the type from then, so the value has one
      address from the moment its holder exists and is read and written where it lies. A property
      read before its initializer runs (LRM 8.7) reads that value.
- [x] A tuple's bytes no longer open with its type: every holder of one -- a cell, a reference, a
      designated part, a net, a sampled history -- states the type of what it holds.
- [x] The execution backend lays out a cell of an unpacked union from the union's type, so a
      property of a union type, or of a structure holding one, read before its initializer runs
      reads the union's first member's default (LRM 7.3) on both backends.
- [x] An unpacked union is one storage its members overlay, laid out from its type. A structure
      member's common initial sequence reads what was written through another member (LRM 7.3), a
      union streams its first-declared member whichever was written last (LRM 11.4.14.1), and a
      net's drivers on different members fold as its first member, on both backends.
- [x] Every operation on a whole value of a union, a tagged union or a fixed-size unpacked array --
      the equalities, `$bits`, `$countbits`, `$isunknown`, the bit stream, the net folds, whether a
      write changed it, and its copy and its end -- is code compiled for the type on both backends,
      as a structure's is. A union's are stated once, by its declaration, as a structure's are; an
      array's by each unit naming it. The prebuilt library is told an array's count and its
      element's type for reaching an element, building one from elements and merging two, and no
      description of a union.
- [x] Runtime scalars that were packed values -- descriptors, seeds, counts, a takeover's generation
      -- are machine integers, and a constant passed as one is a constant of that machine integer. A
      delay's amount is not one of them: it crosses as its bits with its width, because an unknown
      amount is a zero delay (LRM 9.4.1), which a machine integer cannot say.
- [ ] The C++ backend's compile time on Ibex and on the largest open designs available is measured
      against the run before.

## What the library is told of an element's type

The library exists before any design does, so what it may be told of a design's type is a number of
bytes, a number of bits, or the address of a function the design's own code contains. Phase 2 makes
that true of a packed value. These make it true of what holds one.

- [ ] An element of a queue or a dynamic array at an index is addressed where the index is written,
      without entering the library.
- [ ] The storage of a queue, a dynamic array and an associative array is managed once, told the
      element's size, and both backends use that one implementation.
- [ ] What accompanies a type wherever a value of it is held is only how to copy it and how to end
      it. A comparison, an ordering or any other operation a type answers is handed to the operation
      that uses it.

## Phase 3: a port shares its source's storage

- [x] A child's input port is a reference, naming a whole variable of an equivalent type wherever it
      is declared, or another such port, and a cell the instantiator assigns otherwise: an
      expression, a part of a variable, a default, and an input left unconnected.
- [ ] An output connected to a whole variable of an equivalent type makes that variable a reference
      to the child's port.
- [ ] A variable continuously assigned the whole of another of an equivalent type, with no delay,
      names that other's storage, and no assignment runs.
- [x] A net with one driver once the design is built shows that driver's value with nothing
      resolved, where its net type contributes nothing of its own and no connection joined it to
      another net. Where that driver is a connection or an assignment from the whole of a variable,
      another net, or an input port declared as a variable, of the net's type, the net shows that
      object and nothing runs; a wait on the net is a wait on the object, and a force on the net is
      retargeted as one on a port declared as a variable is.
- [ ] Every other connection moves only the range its source's write reached.
- [x] A force on a sink retargets its reference and a release restores it, together with every port
      the sink is handed on to and every wait on them.

## Not this workstream

- How the engine carries out a wait and a wake: a wait whose places are fixed registered once rather
  than at every activation, an edge decided where the write lands, a nonblocking assignment held as
  a value of its type, and a process the language makes a reaction (a continuous assignment,
  `always_comb`, `always_ff`) realized as a function rather than a coroutine that waits again.
  Measured on Ibex, that per-event work is a larger part of the gap to Verilator than the value work
  here, and it is designed on its own.
