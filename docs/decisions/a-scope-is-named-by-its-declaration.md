# A scope is named by its declaration

Date: 2026-11-09 Status: accepted

## Why this decision matters

Every scope of a running design is named by someone outside the code that built it: `%m` prints its
name, DPI-C hands it to foreign code and takes it back (LRM 35.5.3), a value dump writes it, and VPI
answers what each scope is, what it holds and what it is called (LRM 37.10 to 37.12, 37.85). A unit
is compiled once and its instances made at run time (`north_star.md` invariant 2), so whatever a
scope says of itself has to be stated by the class it is an object of, once, and read off at run
time; and a unit's constants are part of what it compiles to, so nothing stated there may change
when text that changes nothing else moves (invariant 4). The questions this settles come back with
every interface that names a scope: what states a scope's name and kind, which scopes have an object
of their own, what a package is to the run, how a name handed in from outside is read, and why no
record says where a scope was written.

## The decisions

```text
D1. Every class of the design hierarchy, and every package and compilation unit's scope, states a
    record of each scope it declares: an instance, an array of instances of a module, an
    interface or a program, a generate block, the blocks a loop generate counts out, a class --
    one however many of its specializations the design uses (LRM 37.31) -- a clocking block, a
    subroutine it imports from C (LRM 37.41 detail 6), and each procedural scope LRM 37.12 makes
    a scope: a task, a function, a `for` declaring its variables or a `foreach`, and a block or a
    fork the front end made a scope, which it does where the source named it or it declares a
    block item (LRM A.2.8). A record holds the scope's name as a hierarchical name writes it (LRM
    23.6), none where the source gave none, its kind -- one the run builds scopes for or one it
    builds nothing for -- whether the name is the one LRM 27.6 gives, and the record of the
    procedural scope it lies inside, if it lies in one. The class also states one record of the
    definition its scopes come from (LRM 37.10): the definition's name, its library and the cell
    its text lies in (LRM 33.2.1), its timescale and whether the source stated it, its kind, and
    the directives in force where it was defined (LRM 22.8, 22.9, 22.10, 23.2.1).

D2. A constructed scope's segment is the record of the declaration it fills, with the indices that
    select it. Its name, its kind and what a dump records it as are read from that record, so an
    instance carries one pointer where it carried a copy of its name.

D3. Every generate block a construct builds is a constructed scope. A block, or the set of blocks a
    loop generate counts out, that the source gave no name is named as LRM 27.6 names it,
    `genblk<n>`, and its record says the name was given rather than written (LRM 37.85
    `vpiImplicitDecl`).

D4. A package and a compilation unit's scope are each one object of the run beside the instance
    tree (LRM 37.10). Installing its storage is where the design brings it up, which the design
    root's constructor does, as an instance's constructor installs its declared variables; so that
    is where it hands the runtime its definition and the records of what it declares, and the run
    knows every timescale the design states, packages' included, before it fixes the global time
    precision, which only a stated timescale takes part in (LRM 3.14.3); a scope left to the
    default time unit (LRM 3.14.2.3) takes none, and where no scope states one the default is the
    precision. Such a scope can then measure time more finely than the tick: what it reads of the
    time is in its own unit, and a delay it names rounds to the nearest tick, which the standard
    leaves to the tool. A class lowered on its own and a text the tool reads in beside the design
    are no such object: the classes of the built-in package `std` are Lyra's own definitions of
    what the front end declares (LRM 26.7), and state no timescale the design wrote. A
    compilation unit's scope is named `$unit` (LRM 3.12.1).

D5. A name that reaches the run from outside the design -- from foreign code or the run's command
    line -- is read in one place: an identifier, escaped or simple, with the selects after it, a
    package's name and `::` before the first. Two names agree where their identifiers do, so an
    escaped identifier and the simple one it spells are one name (LRM 5.6.1). Each part is looked
    for among the scopes the run constructed and then among the declarations the class states,
    from the tops of the design or from the scope the name is relative to; what a scope the source
    gave no name declares is reached only from that scope (LRM 9.3.4). DPI-C, the dump's scope
    option and VPI all read names this way; each keeps what it may reach. Nothing within a
    compilation unit's scope, and nothing an import makes visible, is reached by a name from
    foreign code (LRM 37.10 detail 6). A name the run shows -- what `%m` prints, the full name VPI
    answers, the scopes a dump writes around what it records -- is one name wherever it is shown:
    the named scopes around a thing and its own name, with no part for a scope the source gave no
    name, as the front end writes a symbol's path. It is shown rather than looked up, so it may
    name what no hierarchical name reaches, and two things may share it (LRM 37.10 detail 5).

D6. No record states where a scope was written. A place stated as a constant of a unit makes text
    moved above the unit change the unit, and recompile it, while nothing it does has changed.
    Where VPI asks a scope's file or line (LRM 37.10 `vpiFile`, `vpiLineNo`), the answer is not yet
    carried out.
```

## Rejected alternatives

- **A copy of each scope's name in each instance.** It is what a segment held. It costs a string per
  instance for a fact of the class, and it says nothing of what the scope is, so every reader asking
  that had to be told separately.
- **A run-time object for every procedural scope.** A block, a task or a function has no object of
  its own (`procedural-storage-scope.md`), and VPI needs none: what it reaches is the declaration as
  it stands in the constructed scope holding it.
- **Records holding where each scope was written.** Measured on the incremental cases: with places
  in the records, text moved above a design element remade the units below it.
- **Packages found by a table of names on the runtime.** A name is not an identity; the runtime
  holds what each namespace handed it, and a name is looked up only where one is read from outside.
- **A third bring-up entry each namespace publishes for its records.** Installing storage is already
  the one step the design takes once per namespace, so the records ride on it and the design root
  depends on nothing more.

## Cross-references

- [hierarchy_and_generate](../architecture/hierarchy_and_generate.md) -- the tree, the segment, and
  the search a name from outside the design is resolved by.
- [emission_model](../architecture/emission_model.md) -- what a scope class keeps that no route
  reaches.
- [procedural-storage-scope](procedural-storage-scope.md) -- why a procedural scope has no object.
- [unit-scope-naming](unit-scope-naming.md) -- the name a `$unit` scope's unit is published under.
- [a-dump-is-chosen-when-the-run-starts](a-dump-is-chosen-when-the-run-starts.md) -- how a dump
  reads the declaration each variable lies in.
- [a-vpi-handle-is-a-reference-to-an-object](a-vpi-handle-is-a-reference-to-an-object.md) -- the
  objects VPI makes of these records.
- IEEE 1800-2023: 5.6.1 (escaped identifiers), 23.6 (hierarchical names), 26.7 (`std`), 27.6
  (unnamed generate blocks), 37.10 to 37.12 and 37.85 (the scopes VPI reaches).
