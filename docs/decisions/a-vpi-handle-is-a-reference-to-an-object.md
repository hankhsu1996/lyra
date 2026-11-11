# A VPI handle is a reference to an object

Date: 2026-11-09 Status: accepted

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
D1. An object is one of four things: a scope the run constructed; a package or a compilation unit's
    scope; the set one declaration builds in a constructed scope -- an instance array, or the
    blocks a loop generate counts out (LRM 37.11, 37.85); or a scope the run makes no object for,
    as it stands in the nearest constructed scope or namespace around it -- a block, a fork, a
    loop, a task, a function, one imported from C among them (LRM 37.41 detail 6), a class or a
    clocking block (LRM 37.12). The last two kinds are named by that holder and the declaration's
    record (`a-scope-is-named-by-its-declaration.md` D1), so one declaration held by two scopes is
    two objects.

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

D6. No compatibility mode changes what these routines answer of these objects: every
    incompatibility LRM 36.12.1 lists is of variables and arrays of them.
```

## Rejected alternatives

- **A handle that is the constructed scope's address, as DPI-C's `svScope` is.** It reaches only
  what the run constructed, so a task, a block or an array would have no handle.
- **An object made at run time for each block, task and function.** It would cost every instance a
  node per procedural scope for a question few runs ask; the holder and the record already name it.
- **A handle holding the names it answers.** Names are read from the records the classes state, so a
  copy is a second statement of one fact.

## Cross-references

- [a-scope-is-named-by-its-declaration](a-scope-is-named-by-its-declaration.md) -- the records an
  object is named by, and how a name from foreign code is read.
- [foreign-interface-routines-are-defined-against-the-standard-s-headers](foreign-interface-routines-are-defined-against-the-standard-s-headers.md)
  -- which routines are defined, and the compatibility modes.
- [hierarchy_and_generate](../architecture/hierarchy_and_generate.md) -- the constructed tree.
- IEEE 1800-2023: 36.10 (the object model), 36.12.1 (incompatibilities), 37.10 to 37.12, 37.84 and
  37.85 (the scopes and iterators), 38.2, 38.3, 38.6, 38.7, 38.11, 38.13, 38.18 to 38.21, 38.23,
  38.38 and 38.40 (the routines).
