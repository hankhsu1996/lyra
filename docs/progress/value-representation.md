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
against the run before it. The reference point is Ibex at `91b619f1`: 12.26 G instructions on the
C++ backend and 14.96 G on the execution backend against Verilator's 0.122 G, 100x and 122x per
cycle.

## Phase 1: what is specific to a type is generated, not described

- [x] A runtime facility that acts on a value of any type -- formatting, DPI conversion, file and
      memory-image reading and writing, sampled history, a wait's comparison -- is handed the
      functions the compiler generated for that type. A value held beside its type's constant
      survives on the execution backend where a union holds its member, an associative array its
      key, and a closure or a `with` clause its answer.
- [x] A write reports the range it reached, and a wait decided at the write tests that range against
      what it watches on the words, without materializing either side.
- [x] On the execution backend, a queue, a dynamic array, an associative array and a fixed-size
      array hold their elements as raw storage acted on through the element type's generated
      functions, a union holds its member with the member's type, and a value whose representation
      an entry cannot know crosses as itself and its type.

Measured 2026-10-05 on Ibex, whole run under callgrind, `--release`, both programs ending at
`$finish` at 26548 as the reference run did: the C++ backend 10.87 G instructions (819 K per cycle)
and the execution backend 13.13 G (990 K per cycle), against 12.26 G and 14.96 G at the reference
point -- 11% and 12% fewer, and 89x and 108x Verilator's 0.122 G.

## Phase 2: a packed value is its words

- [x] A packed value is its value words, followed by its unknown words when four-state, in one
      contiguous sequence of bits; a type is identified by its exact width, signedness and state
      domain, on both backends.
- [x] Every operation on a packed value is generated for its type: inline up to 64 bits, and a call
      taking the words above.
- [x] An element or a member at a run-time index is reached by bit addressing at the cost of the
      element.
- [ ] A wait on part of an unpacked aggregate is passed over by a write that reached another part of
      it.
- [x] Constants are compile-time constants on both backends, and the execution backend lays out a
      value, a cell and a frame slot of an integral up to 64 bits from the type.
- [x] Whatever holds an integral wider than 64 bits -- a variable, a net, a sampled history, a
      procedural local -- holds the value's words and is handed no type. A variable, a net and a
      history are told the width once, where they are installed. A reference to one, and a part
      designated within a write, are told the width at each access.
- [x] A procedural local wider than 64 bits is told its width once, where its storage is built, and
      a store into it is told nothing.
- [x] A whole write of a value wider than 64 bits compares the arriving bytes with the words the
      holder has and takes them there: a store into a variable, a force on a variable or a net, a
      push into a sampled history and a driver's contribution. The first value a forced variable
      takes after a force begins is the one exception, since the level holds nothing to take it into
      until then.
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
      converted by hand where each call is built, and a count a result type fixes is still an
      operand of a run of array elements and of the conversions to an array of bytes.
- [x] The instructions the execution backend writes for an operation on one word are held to the
      library's statement of the same operation by a test that evaluates both on the same operands.
- [x] A net of an integral type has one install, told how many positions its declared type has, on
      both backends.
- [x] An enumeration's member table states the base type it is laid out at, and the library reads
      how many words a member takes from the table.
- [ ] Every operation over constants is folded where it is built. An operator, a cast and a system
      function over constants are, and an operation over constants left unfolded is refused. A
      conversion of constant bits to a string and the text of a constant under a format are still
      folded by functions of their own, an enumeration method called on a constant is not folded,
      and an enumeration's members are not constants the unit names.
- [ ] The execution backend lays out a cell of every other type from the type.
- [ ] A tuple's bytes no longer open with its type: every holder of one -- a cell, a reference, a
      designated part, a net, a sampled history -- states the type of what it holds.
- [ ] An unpacked union is one storage its members overlay, laid out from its type. A structure
      member's common initial sequence then reads what was written through another member (LRM 7.3),
      and a union streams its first-declared member whichever is live (LRM 11.4.14.1). Both are
      refused today, since a union holds only its live member.
- [x] Runtime scalars that were packed values -- descriptors, seeds, counts, a takeover's generation
      -- are machine integers, and a constant passed as one is a constant of that machine integer. A
      delay's amount is not one of them: it crosses as its bits with its width, because an unknown
      amount is a zero delay (LRM 9.4.1), which a machine integer cannot say.
- [ ] The C++ backend's compile time on Ibex and on the largest open designs available is measured
      against the run before, and stated here.

Measured on Ibex, whole run under callgrind, `--release`, both programs ending at `$finish` at
26548: the execution backend 7.16 G instructions, against 13.13 G at the end of phase 1 -- 45%
fewer, and 59x Verilator's 0.122 G. The C++ backend 3.03 G instructions (228 K per cycle) against
10.87 G at the end of phase 1, measured on the merged tree, so what else merged in between is in
that figure.

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

- [x] A child's input port is a reference, naming the instantiator's whole variable where the actual
      is one of an equivalent type that the instantiating scope or a scope enclosing it declares,
      and a cell the instantiator assigns otherwise: an expression, a part of a variable, a variable
      reached through another instance, a default, and an input left unconnected. Measured on Ibex,
      whole run under callgrind, `--release`, execution backend: 3.61 G instructions to 3.17 G, both
      ending at `$finish` at 26548.
- [ ] An output connected to a whole variable of an equivalent type makes that variable a reference
      to the child's port.
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
