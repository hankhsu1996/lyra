# A port shares its source's storage

Date: 2026-10-04 Status: accepted. Applies
[a-value-is-its-machine-data](a-value-is-its-machine-data.md) principle 2 to port connections of
variables.

## Context

A variable port connection is two cells and a continuous assignment between them: the instantiator
evaluates the actual and stores it whole into the child's cell (input), or the reverse (output).
Writing one element of a large packed value therefore costs the whole value. Measured on the
execution backend, `--release`: a parent writes one byte of `logic [N-1:0][7:0] mem` connected to a
child's input 2000 times, 81.3 M instructions at N=1024 against 1,155.7 M at N=16384 -- 14 times the
work for 16 times the size, 61% of it moving bits, the rest copying and clearing the whole value. A
memory written one entry at a time through a hierarchy is the ordinary case, and its cost grew with
the memory's size rather than with the entry's.

The standard defines the connection as an assignment, and does not fix when the sink sees it. LRM
23.3.3 (p.746): "Each port connection shall be a continuous assignment of source to sink". LRM 4.8
(p.69), on `assign p = q`: "The simulator may either continue and execute the `$display` task or
execute the update for `p`, followed by the `$display` task." So a sink that observes the source's
value the moment the source is written is one of the schedules the standard permits. Each side has
one writer: "Assignments to variables declared as input ports shall be illegal", and "Procedural or
continuous assignments to a variable connected to the output port of an instance shall be illegal"
(23.3.3.2, p.747).

## Decision

**D1. The sink names the source's storage where the connection is a whole variable of an equivalent
type.** The sink is realized as a reference to the source's storage, bound when the instance is
built, so a write to the source is the sink's value with nothing propagated, and the value is held
once. Each direction is decided by the unit that owns the sink, where that unit compiles:

- **An input port is a reference in every instance of the child.** The child cannot know its
  actuals, so it reads every input through a reference -- one indirection per read -- and its
  compiled body is the same for every instance. Where the actual is a whole variable of an
  equivalent type, wherever it is declared, or another such port, the reference names that storage.
- **An output's actual is a reference where it is a whole variable of an equivalent type.** The
  instantiator states its own variables and its own connections, so it realizes such a variable as a
  reference to the child's port storage; the child's output is ordinary storage of its own.

**D2. Every other connection keeps a cell on each side and assigns continuously, moving only the
range the source's write reached** ([a-value-is-its-machine-data](a-value-is-its-machine-data.md)
D7). That covers an input whose actual is an expression, a select with a run-time index or a value
of a type that is not equivalent -- the instantiator evaluates it into a cell of its own, which the
child's input reference names -- and an output whose actual is a part of a variable, a concatenation
or a conversion. An input left unconnected, and an input of a top-level unit, name such a cell too,
holding the data type's default initial value (23.3.3.2).

**D2a. A reference is bound while the design's references are resolved, and a variable's storage
exists from construction.** A reference names where a value lies, so the storage of every variable
-- one whose value is laid out at run time included -- is installed as its scope is built, holding
the data type's default, and only its declared initial value waits for initialization. Binding is
then free of order: scopes resolve parent first, a connection may name a port of an instance
resolved later, and a port bound from one not yet bound takes what that one names when it is bound.

**D3. A `force` on the sink's name retargets its reference to a cell holding the forced value, and
`release` points it back at the source.** A force on the source reaches the sink, as the continuous
assignment the standard defines would carry it; a force on the sink must not reach the source, and
retargeting keeps it local. Both are acts of the run, so no unit needs to know whether another unit
forces anything.

What a sink drives follows it. A port handed on to a child's port, at any depth, is retargeted with
it, and so is every wait enrolled through any of them, so that a change of the source while the
force holds is no event for a wait on the sink and the force itself is one. A port below that a
force of its own covers keeps that force, and takes the new binding as what it will show once
released. For this a reference says which member of a scope it was bound into, a binding records
which members were bound from which, and a wait records the member it was reached through.

The forced storage belongs to the sink, from the force to the release, whatever becomes of the
process that made the force. It first holds what the sink showed and is then written with the forced
value as any variable is, and a release writes what drives the sink into it before the bindings go
back; so whoever waits on the sink is told of the force and of the release exactly when the value it
shows changed. These are the three operations a procedural continuous assignment has on any target
([procedural-continuous-assignment](procedural-continuous-assignment.md) decision 6).

`inout` ports are already one resolved net, and `ref` ports already share storage; neither changes.

## Rejected

- **Two cells, propagating only the changed range in every case.** Meets principle 2 for the write,
  holds every connected value twice, and still runs a continuous assignment per connection where D1
  runs nothing.
- **Inlining the connection into one variable at the instantiator.** Verilator's answer
  (`inlinePort` in its `V3Inline.cpp`), which needs the child's body; a unit compiled from published
  declarations cannot rewrite another unit's variables. It also declines to inline a forced port,
  which needs the whole design to know.
- **A child input that is a reference only where the actual is a whole variable.** Two realizations
  of one port, chosen per instance, make the child's body depend on how it is instantiated; D1 keeps
  one, and D2 supplies a cell for the reference to name when there is no variable to share.

## Consequences

- Writing one element of a memory reaching a child through ports costs the element at every level.
- An input port read in the child is one load more than a local variable.
- The time-zero event 23.3.3.3 mentions (p.747) arises only where the two sides' data types differ;
  those connections are D2's, which still assign.
