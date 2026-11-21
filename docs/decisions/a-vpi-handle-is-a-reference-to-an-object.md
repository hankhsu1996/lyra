# A VPI handle is a reference to an object

Date: 2026-11-09 Status: accepted; D1 and D6 revised and D7 to D10 added 2026-11-16, when VPI came
to reach variables; D11 added 2026-11-19, when callbacks came to be registered

## Why this decision matters

VPI hands foreign code a handle to each object it reaches, and the code passes handles back,
compares them and releases them (LRM 38.3, 38.38). The design hierarchy holds more objects than the
run constructs: a task declared in a loop's block is a task of each block the loop built (LRM
36.10), and an instance array is an object beside its elements. The questions this settles come back
with every object kind VPI is taught: what an object is when the run made nothing for it, what a
handle holds, when two handles are one object, and what a routine answers of an object or a relation
Lyra does not reach yet.

## The decisions

```text
D1. An object is one of these: a scope the run constructed; a package or a compilation unit's
    scope; the set one declaration builds in a constructed scope -- an instance array, or the
    blocks a loop generate counts out (LRM 37.11, 37.85); a scope the run makes no object for, as
    it stands in the nearest constructed scope or namespace around it -- a block, a fork, a loop, a
    task, a function, one imported from C among them (LRM 37.41 detail 6), a class or a clocking
    block (LRM 37.12); a variable or a part of one (LRM 37.15 to 37.19, 37.26); or a constant of
    a unit a relation answers -- a typespec, a member of one, an enumeration's constant (LRM
    37.25), a range and a bound of one (LRM 37.22). A scope the run makes no object for is named
    by its holder and the declaration's record (`a-scope-is-named-by-its-declaration.md` D1), so
    one declaration held by two scopes is two objects, and a variable by its holder, the position
    of its row and its selection (D7).

D2. A handle refers to an object and holds nothing else of it. Two handles compare equal where they
    refer to one object, however each was reached (LRM 38.3), and releasing one leaves the other.
    A handle is the address of a word holding its own number in the run's table. One foreign code
    passes back is looked for among the addresses the table issued before anything is read through
    it, so a pointer that is no handle is reported as an error (LRM 38.2) rather than followed.

D3. An iteration holds the objects its relation answered when it was made, and the object it was
    made on (LRM 37.84 `vpiUse`). It is released when stepping reaches its end (LRM 38.40), and a
    relation that answers nothing gives no iterator (LRM 38.23).

D4. A routine asked for a relation or a property Lyra does not reach yet answers nothing and says
    so through `vpi_chk_error` (LRM 38.2). Whether a routine is defined at all is settled by
    `foreign-interface-routines-are-defined-against-the-standard-s-headers.md` D2; this is what a defined
    routine does with a question about an object it cannot yet answer.

D5. Where the standard leaves the answer to the tool: a scope the source did not name answers no
    name and no full name; `vpiInternalScope` from an instance or a generate scope answers every
    scope directly inside it, each element of an array or a loop on its own and no array itself;
    the delay mode and decay time directives are informative (LRM Annex E) and Lyra reads neither,
    so a definition answers no delay mode and an undefined decay time; Lyra reads no configuration
    (LRM 33.4), so a definition answers none, the cell its text lies in as its cell (a module
    declared inside another lies in that one's, as `%l` prints it, LRM 33.7), and the library
    its text was read into (LRM 33.3) as its library, `work` where the command line names the
    design; the built-in package `std` is no object, its classes being Lyra's own definitions; a
    package is reached by its name alone where no top-level instance has that name; a
    compilation unit's scope answers `wire` as its net type and no default lifetime, since no
    directive can be in force for it, and one that declares nothing is no object of the run;
    a scope named inside one the source gave no name answers, as its full name, the name `%m`
    prints inside it (`a-scope-is-named-by-its-declaration.md` D5), though no hierarchical name
    reaches it (LRM 9.3.4), since a full name is shown rather than looked up (LRM 37.10 detail 5);
    and asked of no object, the time unit and the time precision are the simulation time unit, as
    LRM 38.6 says and DPI's own queries answer (LRM H.13, which makes the two equal), where LRM
    37.10 detail 7 says the smallest precision of the modules, which differs from it wherever a
    package states a finer one.

D6. A compatibility mode changes what is a variable and an array of them and what a scope's
    relations to them hold, as LRM 36.12.1 Table 36-10's first seven entries say for the mode, and
    nothing else these routines answer: the routines answering a type or a relation (`vpi_get`,
    `vpi_get_str`, `vpi_iterate`) take the mode an application was compiled for (LRM 36.12.2.1),
    and every other one answers alike in every mode. Under 1364-2001, which allowed either shape
    for an array of `reg`, the later one is taken. An application compiled for no mode is read in
    the one the run's `+lyra+vpi+compatibility=<version>` names, by the suffix of the version's
    compiler symbol (`1364v2005`), and in the latest where it names none (LRM 36.12.2.2); a run
    option is how the dump's choices reach a built program too
    (`a-dump-is-chosen-when-the-run-starts.md` D2), so choosing one needs no rebuild.

D7. A variable's object is what holds it, the position of its row among the data objects the holder
    describes (`a-dump-is-chosen-when-the-run-starts.md` D1), and its selection: the steps from the
    variable to the part -- a member's index among its structure's or union's members, an element's
    index as the source would write it, an entry of an associative array by the key the array keeps
    it by, a value of its index type -- and then the steps into the packed value the selection ends
    at, a member of a packed structure or union or an index of a dimension or a bit. It names an
    index, never an address, so a handle to an element of a dynamic array is whatever the array
    holds there when it is used (LRM 37.17 detail 27), and two handles reaching one part by a name
    and by an index are one object. An index foreign code hands an associative array is the index of
    the select it would write (LRM 7.8.4, 38.19): the code settling the selection, compiled for the
    array's type, takes the step at the index type and has the number -- or, for one no 64-bit
    integer holds, its bits -- converted into that type by the library's conversion of an integral
    value, the one the design's own casts into it take, never by a reading of the type's record. The
    handle is made from the keys that code hands back, so it is settled whole when it is made, by
    the types alone and however the run stands, and the entry is named by that key from then on: 300
    into a `byte` index names the entry at 44, and two keys are one where they are of one type and
    the same bits. A wildcard index, which no one type holds, stays the number it was named by, read
    unsigned, which the design's own select indexes with (LRM 7.8.1). Within a packed value a part
    is the bits it is, so a bit named through a member and the same bit named by its index into the
    whole are one object too (LRM 38.3). A `ref` port's name is a reference object of the same shape
    (LRM 37.15), whose actual object is the variable whose watch is the reference's own, looked up
    in a table of every variable by its storage that the run makes once, at the first such question;
    a reference connected to a part of a variable is not yet reached.

D8. A value is read, written, forced and released by code compiled where its type is known, one body
    per type and operation that takes the one step a selection states at its depth into a value of
    that type -- a member, an element through the declared range, an entry by its key, a tagged
    member with its tag checked -- and hands the rest to the bodies of the part's type, as a type's
    derived serialization hands each field to the field type's own; the class declaring the variable
    states one body per operation that finds the variable by its row and hands it to the bodies of
    its type. A read is handed a reference to where the value lies and takes each step as the
    design's own read does, so an element it does not hold reads as its type's default and is not
    made (LRM 7.4.6, 7.8.6); a write is handed a reference belonging to the variable and takes each
    step as a reference to the part is taken, so it lands as the design's own write through a `ref`
    does (LRM 13.5.2, 9.4.2) and an entry it names is made (LRM 7.8.7). A member of a union is never
    lent by reference, since it is not among what may be passed by reference (LRM 13.5.2), so the
    design writes a member only as a write of the union. A read reads the member's value and hands
    on a reference to that, and a write writes the value the member holds as a value of its own and
    writes that to the member, as the design's own write of the member lands. A selection settles,
    by the same bodies and the types alone, into the key each entry it names is kept by and the
    positions a takeover counts before the part, which a force and a release read. The code compiled
    grows with the types and the data objects, never with the paths through them. Which bits of that
    value a part is, is the runtime's: a read is sliced from the value read whole, and a write of
    some bits is the value read, those bits laid in, and the value written whole, so a wait on its
    other bits sees no change of them (LRM 9.4.2). Where a selection ends at an array whose size the
    run sets, a read hands how many elements it holds, and, where the reader asks for them, an
    associative array's indices in the order it keeps them (LRM 7.9.4), which is what its elements
    and the range of its first dimension are read from (LRM 37.17 details 4, 9, 37.22); a size is
    read without walking an index. Each key crosses with its type and is kept as the array keeps it,
    so an entry of every index type the array holds is named, a class handle's among them, and a
    handle to an entry keyed by a class handle holds that object as the key it names, so the object
    lives while the handle does; an integral value read as a real, or a real written into one, is
    converted by the one conversion the design's own casts use (LRM 6.12.1). A variable declared
    `const` is not written (LRM 6.20.6), nor forced. Foreign code enters the design on the terms a
    call it makes into the design does (LRM 35.9): no code of the design is entered -- no read,
    write or takeover -- where what called into foreign code was disabled or ended the run, while
    what the records alone answer is answered; a read or a write the design cannot do -- a member of
    a tagged union other than the one its tag names, an operation not yet carried out -- is that
    request's error, answered through `vpi_chk_error` (LRM 38.2), and the run goes on as it would
    had the request not been made, since what failed is what foreign code asked, not the design; a
    departure raised under it was recorded where it was raised and is taken as the import returns;
    and nothing is raised into foreign code.

D9. A force from foreign code is a procedural force (LRM 38.34) and takes over what one takes
    under the reading the build was given, which is the front end's: the severity it gives a
    procedural force of a part of a variable, read once where the design is loaded, so the two
    cannot disagree. The force and release bodies are compiled under that reading and each answers
    what it came to. Under the standard's, only a whole singular variable is taken, asked for
    whole, and anything less answers that it is forbidden (LRM 10.6.2); under the tolerant reading
    of a forced part (`language_conformance.md`), any part is taken as a procedural force of it
    would be, and a part no takeover holds at a fixed position -- a tagged union's member, a part
    of a dynamic container -- answers that it is not yet supported. Only a unit built under the
    tolerant reading carries a case for each part. A release hands back the value the object holds
    after it.

D10. Where the standard leaves the answer to the tool: an array variable states no sign; the size of
    a real, a handle or an event is undefined, answered as `vpiUndefined`; a `longint`'s own format
    is a vector, since an integer format holds 32 bits; a digit of hexadecimal or octal text whose
    bits hold both x and z is an X, as `%h` prints one (LRM 21.2.1.3); an element of a dynamic
    container is allocated by the run, answered as `vpiDynamicScheme`; a typespec no typedef names
    answers no name, but a class's, which is the class whatever typedef names it and is named after
    it (LRM 37.25 detail 1, 37.32 details 1, 4); and a net or a named event is reached by name as
    not yet supported, as are listing the entries of an associative array with a wildcard index,
    whose keys no operation of the language lists (LRM 7.9), and the name of an entry keyed by a
    class handle or a `chandle`, which has no literal to be written with.

D11. A callback and a time queue are objects too, of the simulation rather than of the design (LRM
    36.10, 37.80, 37.81). A callback is named by its identity among those the run registered, so
    a handle to it is released like any other and two registrations of the same data are two
    callbacks; one removed or ended names none, and a handle to it then answers that. A time
    queue is named by its time, answers it through `vpi_get_time`, and is listed in increasing
    time, the current slot only while it holds events ahead of its Postponed region, and only
    slots the run will still run. An object relates to the callbacks placed on it, a time queue
    to those waiting in it, and no object to those related to none of the diagram's objects --
    one for a simulator action, on a signal, on a task or a function, or due when time next
    moves. From an object of a kind the diagram does not name, the relation is refused as one
    the object does not have.
```

## Rejected alternatives

- **A handle that is the constructed scope's address, as DPI-C's `svScope` is.** It reaches only
  what the run constructed, so a task, a block or an array would have no handle.
- **An object made at run time for each block, task and function.** It would cost every instance a
  node per procedural scope for a question few runs ask; the holder and the record already name it.
- **A handle holding the names it answers.** Names are read from the records the classes state, so a
  copy is a second statement of one fact.
- **A variable's handle holding the address of the part it reaches.** An element of a dynamic array
  moves as the array is resized, and LRM 37.17 detail 27 has the handle name whatever the array
  holds there when it is used; an address would name storage the array no longer holds.
- **The runtime reading a part's storage by an offset it computes.** It would state a second time
  how each type is laid out and how each select maps an index, which the design's own compiled steps
  already state; a body compiled where the type is known reads through them (D8).
- **The runtime refusing a forced part from a reading each scope records.** The reading is one fact
  of the build, which a record per scope would state once per scope, and the body compiled under it
  already knows it; the body's answer is the one place it is read (D9).
- **Finding a reference's actual object by searching the design on each question.** Its cost would
  follow the design's size on every `vpiActual`; a table made once costs that once per run (D7). The
  binding the connection makes is what would reach a part of a variable as well.
- **The runtime converting an index foreign code hands into an index type by its own reading of the
  type's record.** It would state a second time what the design's own select states, and read
  wrongly where the two differ; the code reaching the entry converts it at its own type (D7).
- **A selection holding an associative index as a 64-bit number or a string.** No class handle, no
  key wider than 64 bits and no unsigned 64-bit key above the largest signed one fits, so those
  entries would have no name; the key the array keeps is the step (D7).
- **A body per class unrolling each data object's type into every path through it.** Its code grows
  with the paths through each type -- `m` members nested `k` deep are `m^k` paths from `k*m`
  declarations -- and repeats for every variable of a type; a type's own body taking one step and
  handing the part on to the next type's, as a reference to it, grows with the types (D8). The
  objection that a part handed between bodies is no path the design's own write takes does not hold:
  a reference to a part, stepped into one part at a time, is how the design hands a part to a `ref`
  formal, and a write through it is the design's own.
- **A reference lent to a member of a union.** A member of a union is not among what may be passed
  by reference (LRM 13.5.2), so the design never writes through a reference to one, and a foreign
  write through one would be a write the design has no counterpart of (D8).
- **A table of each type's step functions, which a loop in the runtime calls.** A table gives every
  type's step one prototype, which needs a reference erased of its type and recovered in each body;
  a body calling its part type's body directly needs neither, and the loop is then the compiled
  calls (D8).
- **Settling a selection by a read that writes the keys it converts back into the selection.** A
  handle would be finished only after design code ran on it, and its identity would depend on
  whether that read had happened; settling by the types alone makes the handle whole when it is made
  (D7).
- **Ending the run where a foreign read or write fails as the design's own would.** The design did
  not ask for what failed, so ending its run would let foreign code's mistake end a simulation the
  standard gives an error channel for (LRM 38.2); a debugger interface keeps the program it reflects
  running and answers the request with an error, as Java's JDI answers a failed request with an
  exception and LLDB's SB API with an `SBError` (D8).

## Cross-references

- [a-scope-is-named-by-its-declaration](a-scope-is-named-by-its-declaration.md) -- the records an
  object is named by, and how a name from foreign code is read.
- [foreign-interface-routines-are-defined-against-the-standard-s-headers](foreign-interface-routines-are-defined-against-the-standard-s-headers.md)
  -- which routines are defined, and the compatibility modes.
- [hierarchy_and_generate](../architecture/hierarchy_and_generate.md) -- the constructed tree.
- [a-dump-is-chosen-when-the-run-starts](a-dump-is-chosen-when-the-run-starts.md) -- the rows a
  scope describes its data objects in, and the bodies reaching them.
- [a-part-of-a-variable-is-named-by-its-steps](a-part-of-a-variable-is-named-by-its-steps.md) -- how
  a write of a part is watched, which a write from foreign code goes through.
- IEEE 1800-2023: 10.6.2 (force), 36.10 (the object model), 36.12 (compatibility), 37.10 to 37.12,
  37.15 to 37.26 (references, variables, ranges, typespecs, structures), 37.84 and 37.85 (the scopes
  and iterators), 38.2, 38.3, 38.6, 38.7, 38.11, 38.13, 38.15, 38.18 to 38.21, 38.23, 38.34, 38.38
  and 38.40 (the routines).
