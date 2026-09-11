# Storage a type owns is brought up by whatever brings up its declarer

Date: 2026-09-10 Status: accepted

## Status

Accepted. Applies [class-declared-in-a-structural-scope](class-declared-in-a-structural-scope.md) D3
to the half it left unrealized, and states the rest of what
[variable-initialization](variable-initialization.md) means for a cell no instance holds; reverses
neither.

## Why this decision matters

A class's static property (LRM 8.9) is one cell the type owns, not a member replicated on every
object. Where that cell sits was already settled: whatever replicates the class declaration. What
was not settled is where the code that brings it up runs, and the answer was invented separately for
each of the two placements -- an instance's own construction for a class a structural scope
declares, and a startup body of the class's own for a class a namespace declares. The second had no
realization below MIR at all, so a package-declared class's static property could not be reached on
the execution backend, and a `static` with no source initializer was brought up nowhere on either.

Getting it wrong is not loud. The C++ backend hung the class's startup body on a C++ dynamic
initializer, which runs before the design root has installed anything, so an initializer reading a
package variable would have read uninstalled storage. Both halves answered for the cases that had
been written and neither answered a case nobody wrote.

## What the standard requires

**LRM 10.5**: "Setting the initial value of a static variable as part of the variable declaration
(**including static class members**) shall occur before any initial or always procedures are
started." The parenthetical is the decision: the standard puts a static class member under the rule
for static variables rather than beside it, and orders both against the same moment.

**LRM 8.9**: a static property "shall be created and initialized once", and its example initializes
one from `$fopen` -- so the initialization is observable, and once means once.

**LRM 8.9** also: "The static class properties can be used without creating an object of that type",
which with **LRM 10.3.2** makes such a property a legal continuous-assignment operand whose change
re-evaluates the assignment.

## What other systems do, and where our conditions differ

The question worth asking is which component allocates the storage and which runs the initializer,
at which stage -- not what the storage looks like.

| System                            | Allocates                            | When                                 | Runs the initializer                       | Order                           |
| --------------------------------- | ------------------------------------ | ------------------------------------ | ------------------------------------------ | ------------------------------- |
| JVM (JVMS SE21 5.4.2, 5.5)        | the runtime                          | class **preparation**, defaults only | the runtime, `<clinit>`, **lazily on use** | per class, use-triggered        |
| C++ (Itanium C++ ABI 2.8)         | the linker                           | load, zero-initialized               | the C++ runtime before `main`, guarded     | **unspecified across TUs**      |
| Go (spec, Package initialization) | -- no type-associated storage exists | program start                        | the runtime, in two phases                 | dependency order, before `main` |

The JVM's split is the one that transfers: preparation "does not require the execution of any Java
Virtual Machine code" and gives every static field its default, and `<clinit>` runs the written
initializers afterwards. That is two phases, and it is the same two Lyra already runs for a
namespace's own variables.

**Where our conditions differ, and this sentence decides the design: SystemVerilog has no lazy
trigger.** LRM 10.5 requires every static variable's initial value -- static class members included
-- to be in place before any `initial` or `always` starts, so the JVM's use-triggered initialization
is unavailable and the whole set must come up eagerly at time zero. And Lyra compiles per unit and
links at run time, with no static-initializer list a C++ runtime would walk, so the C++ answer's
unspecified cross-TU order is not merely undesirable -- nothing in the session would run it. What is
left is Go's answer, which Lyra already built for package variables: two eager phases over every
unit, defaults first and written initializers second, in a compiler-chosen dependency order.

Go's other half is the conclusion reached from the far side: it has no type-associated storage at
all, because a package-level variable already is the program-shared cell. That is what this decision
adopts.

## Decision

**Storage a type owns rather than an object of it is brought up by whatever brings up the thing that
replicates its declaration, and never by a mechanism of its own.**

### D1. The cells of a class a namespace declares are that namespace's storage

Nothing replicates such a class, so its cells are one per program -- which is what a unit's
namespace already owns, and the unit already publishes two receiver-less bodies the design root
calls at time zero to bring that storage up. The class's cells join those two bodies. There is no
per-class startup body, and no second moment to order against the first.

This is the same sentence the structural case already obeyed: a class a structural scope declares
has its cells as fields of that scope's instance and brings them up where the instance brings up its
own. One rule, read twice.

**A second mechanism cannot be ordered correctly, which is why this is a decision and not a
tidy-up.** The two phases exist because a value initializer may read another unit's cell and must
reach installed storage. A class static initializer may read a package variable, and a package
variable's initializer may read a class static; with the class's bring-up outside the two phases
there is no position for it that is right in both directions.

### D2. A cell a name can reach outside every body is an observable cell

A static property needs no object, so a name reaches it from anywhere the class is visible, and a
continuous assignment may take it as an operand (LRM 8.9, 10.3.2). That is the same test that makes
a namespace variable a cell; an instance property, reachable only through a receiver, stays a plain
value. Stating it as a plain value made a question about the storage answerable only from the shape
of the access that reached it -- a member projection meant a runtime-owned cell and a dereferenced
symbol meant a raw pointer -- and the two answers disagree for one storage.

### D3. Every type-associated cell is installed, whether or not the source wrote a value

The storage arrives with no established representation, so the install phase gives every cell its
declared representation and its type's Table 7-1 default; a property the source left alone is
finished there, and one with a written value stores it through the cell afterwards. What the
codebase's own rule forbids is skipping a cell -- the default case is a statement like any other, in
the phase that owns it, rather than something a backend re-derives from the declaration
([variable-initialization](variable-initialization.md) rejects that by name). Before this a property
with no written value was reached by neither phase.

### D4. Below LIR a type-associated cell is storage under a symbol, and nothing about the class survives

LIR carries no object model, so a cell the program shares is a symbol, a type, and nothing else --
which is what a namespace variable already is there. The class survives only in how far the name is
qualified: the unit, then the class, then the cell. Both the unit that declares the class and every
unit that reaches one of its cells compose that name from the parts each already holds, so they
agree with no table between them.

## Rejected alternatives

- **Keep a per-class startup body and have the MIR-to-LIR lowering call it.** The design root calls
  what a unit publishes; a lowering synthesizing a call MIR did not state is the shape
  `variable-initialization.md` forbids by name, and it would still leave the body unordered against
  the two phases.
- **Keep the C++ dynamic-initializer realization and give the execution backend an equivalent.** It
  is the Itanium answer, and the Itanium answer is unspecified order across translation units. The
  package protocol rejected exactly that for the same cells one scope up.
- **Leave a static property a plain value and teach the place lowering that a dereferenced symbol
  reaches runtime-owned storage.** It answers a question about the storage from the shape of the
  access, which is the defect rather than the fix, and D2's reading of the LRM says the storage is a
  cell whatever reaches it.
- **Give LIR a per-class list of type-associated cells.** Object and member semantics at LIR is a
  forbidden shape in `../architecture/lir.md`, and every consumer would then branch on which list a
  cell came from to do the same thing with it.
- **Put the cells on the published class promise so a referrer counts a slot.** A cell is reached by
  a symbol, and a symbol is a name both sides already compose --
  [constructing-another-units-class](constructing-another-units-class.md) D2's rule, on the storage
  axis. Inheritance needs no walk either, because the front end resolves `Derived::inherited` to the
  class that declares it.

## Consequences

- A class a package or `$unit` declares has reachable, initialized static properties on the
  execution backend, and the cross-unit form reads and writes the same cell.
- A static property with no source initializer holds its type's default from time zero on both
  backends; before this it was brought up nowhere.
- The two bodies a unit publishes cover everything its namespace owns rather than only its own
  variables, so what the design root asks of a unit is the same question whatever that unit
  declares.
- The design root's best-effort initialize order now counts a unit's classes' cells among the reads
  that make one unit a dependency of another.
- `mir::Class` carries no startup body, and the C++ backend emits no program-startup trigger.

## Cross-references

- LRM 8.9, 8.10, 10.3.2, 10.5, 26.2
- JVMS SE21 5.4.2 (preparation) and 5.5 (initialization)
- Itanium C++ ABI 2.8 (initialization guard variables)
- The Go Programming Language Specification, Package initialization
- `class-declared-in-a-structural-scope.md` -- D3 places the cells; this places the code that fills
  them.
- `variable-initialization.md` -- one statement list for construction-time state, and why the
  default case is not skipped.
- `../architecture/lir.md` -- the forbidden shape D4 avoids.
