# A place answers where a wait on it watches

Date: 2026-11-03. Status: accepted.

## Context

A wait enrols on the storage its expression reads (LRM 9.4.2), and a read report names the same
storage for a wait the evaluation makes. That storage is reached many ways: by its route, as a
variable the running body declares, through a `ref` formal (LRM 13.5.2), through a port that stands
for what its connection names (LRM 23.3.3), and through a virtual interface, which makes every
component of the instance it holds available, its ports among them (LRM 25.9). Where a write to the
storage is told is the same whichever way it was reached.

Each way of reaching it composed its own answer. A route took a cell's address, or asked a reference
what it reports to; a body variable asked the reference or took the cell's address; a net asked what
it shows its value through; a member reached through a virtual interface handed over its own
address. So the place crossed to the runtime as three types -- a borrowed pointer to a cell, an
erased pointer, and the library's watched place -- and the trigger and the report each took two of
them. The member reached through a virtual interface is where it broke: for a port that member is a
reference, and the reference itself was handed over as though it were a cell. The execution backend
read it as one and ended by a signal at the first write; the C++ backend found no overload to call.

## Decision

**Where a wait on a place watches is a question asked of the type that stands for the place's
storage, answered once for that type by the library that defines it. Every place a wait or a report
names is handed to that one question, however it was reached.**

- **One entry, `reports_to`, over a variable's cell, a net or a reference, answering the library's
  watched place.** A net's driver is no storage, only one contribution to a net, so nothing waits on
  one. A variable's cell answers itself. A net answers what it shows its value through, its own
  value or the object its one driver carries. A reference answers what a write through it is told,
  named by the member it was bound into where it was, so a wait built on a port follows a force on
  it (LRM 10.6.2).
- **The trigger and the read report take a watched place and nothing else.** One constructor, one
  `Add`, one `AddWrite`.
- **The lowering asks and never answers.** One function makes a watched place from a place; it reads
  the place's type only to know there is a wrapper to ask.

## The field

C++'s `std::atomic_ref<T>` waits on the object it refers to: libc++'s `__atomic_ref_base::wait`
calls `__atomic_wait(*this, ...)`, which waits at the address the reference holds, so a waiter never
asks whether it holds a reference or the object -- the type's member answers. Rust's
`tokio::sync::watch::Receiver::changed` always enrols on `shared.notify_rx`, a fact of the
receiver's own state, whoever holds the receiver. In both the program states a call and the type's
definition decides where the wait lands.

What differs here is the force: SystemVerilog can rebind what a port shows after a wait was built on
it, which neither has. That is why a reference answers with the member rather than the referent's
address. It changes what the reference answers, not who answers.

## Rejected

- **Each maker composing the operand.** What there was. A maker that has to know which wrappers
  report elsewhere is a maker that can forget one, and one did.
- **One entry per wrapper kind, chosen by a switch in the lowering.** It answers correctly, and it
  restates in the lowering what each wrapper type already states, so every wrapper added is one more
  arm to remember. Which wrapper realizes the entry is the backend's to read off the type, as it
  reads every other access through a wrapper.

## Consequences

- A wait on an interface's port through a virtual interface wakes on what the port is connected to,
  on both backends, whether the wait is an edge, a change or a level and whether it is written in a
  class's task, a module's procedure or a process a fork starts.
- The execution backend realizes `reports_to` per wrapper and value representation, as every access
  through a wrapper is realized; for a cell and a reference the answer does not depend on the
  representation, and each realization is one line.
- A place that is no variable, net or reference is storage nothing is told a write to, and asking
  where a wait on it watches is a compiler defect.
