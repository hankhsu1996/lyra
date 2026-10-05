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
      functions the compiler generated for that type, and the erased any-value form goes.
- [x] A write reports the range it reached, and a wait decided at the write tests that range against
      what it watches on the words, without materializing either side.
- [x] On the execution backend, a queue, a dynamic array, an associative array and a fixed-size
      array hold their elements as raw storage acted on through the element type's generated
      functions, a union holds its member with the member's type, and a value whose representation
      an entry cannot know crosses as itself and its type.

## Phase 2: a packed value is its words

- [ ] A packed value is its value words, followed by its unknown words when four-state, in one
      contiguous sequence of bits; a type is identified by its exact width, signedness and state
      domain.
- [ ] Every operation on a packed value is generated for its type: inline up to 64 bits, a loop over
      a fixed word count above, a call taking the words only for the long algorithms.
- [ ] An element or a member at a run-time index is reached by bit addressing at the cost of the
      element.
- [ ] A wait on part of an unpacked aggregate is passed over by a write that reached another part of
      it.
- [ ] The execution backend lays out values, cells and frames from the type, and constants are
      compile-time constants on both backends. A tuple's bytes no longer open with its type: every
      holder of one -- a cell, a reference, a designated part, a net, a sampled history -- states
      the type of what it holds.
- [ ] On the execution backend, an element of a queue or a dynamic array at an index is addressed
      without a call. It waits on the packed value being its words, since reading the index is a
      call while a packed value is a library object.
- [ ] Runtime scalars that were packed values -- descriptors, delays, seeds -- are machine integers.
- [ ] The C++ backend's compile time on Ibex and on the largest open designs available is measured
      against the run before, and stated here.

## Phase 3: a port shares its source's storage

- [ ] A child's input port is a reference, naming the instantiator's whole variable where the actual
      is one of an equivalent type and a cell the instantiator assigns otherwise.
- [ ] An output connected to a whole variable of an equivalent type makes that variable a reference
      to the child's port.
- [ ] Every other connection moves only the range its source's write reached.
- [ ] A force on a sink retargets its reference and a release restores it.

## Not this workstream

- How the engine carries out a wait and a wake: a wait whose places are fixed registered once rather
  than at every activation, an edge decided where the write lands, a nonblocking assignment held as
  a value of its type, and a process the language makes a reaction (a continuous assignment,
  `always_comb`, `always_ff`) realized as a function rather than a coroutine that waits again.
  Measured on Ibex, that per-event work is a larger part of the gap to Verilator than the value work
  here, and it is designed on its own.
