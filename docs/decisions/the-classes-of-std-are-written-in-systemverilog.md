# The classes of `std` are written in SystemVerilog

Date: 2026-10-20 Status: accepted.

## Context

`semaphore` and `mailbox` are classes of the built-in package `std` (LRM 15.2, 26.7): a program
makes them with `new`, calls their methods, and may extend them. The front end creates `std` itself,
declaring each class and each method with its arguments and defaults and no body, and type checks
every call and every derivation against that declaration. The package can not be declared again in
source (the front end reports a duplicate definition of `std`), and `mailbox`'s type parameter
defaults to a type no source can spell, the one LRM 15.4.9 calls `dynamic_type`.

So something has to define these classes, and nothing in the program does.

## Decision

**D1. What `std`'s classes do is SystemVerilog Lyra carries, compiled as any class is.** Every
compilation reads one text of Lyra's own beside the design's sources, whose compilation-unit scope
defines each class the front end declares in `std` without a body, under the name, parameters and
methods the declaration gives it. That scope is compiled as the unit `std`, which is the unit a name
of one of those classes already reaches, and a specialization of one is defined by the definition
specialized on the same parameters. To everything after the front end, such a class is a class of
another unit: HIR, MIR, both backends and the runtime carry nothing of their own for it beyond the
value of any type D4 needs. A definition is specialized after the front end has reported on the
design, so each is bound before specializations are collected and one the front end cannot elaborate
for the parameters a design gave it is Lyra's own defect, stopped where it is found.

This is how the field supplies a language's library. Rust's channel is Rust in
`library/std/src/sync/mpmc/`, over a waiter list and the thread park primitive; C++'s
`counting_semaphore` is C++ in its library headers; SystemC's `sc_fifo` is C++, its modeling
language; Verilator's `verilated_std.sv` is `package std` written in SystemVerilog. Each is written
in the language its users write, over one waiting primitive. Ours differs in one condition: the
declarations belong to the front end, so the definitions are compiled in `std`'s name rather than
declared as `std`. They are no package either: a package is a design element, and the standard makes
it an error for some design elements to state a time scale and others not (LRM 3.14.2.3), so a
package of Lyra's would break every design that states one. A compilation-unit scope is no design
element.

**D2. A definition is what its declaration says.** Each method is a task or a function as declared,
with the same return type and the same arguments -- name, direction, type and default -- and every
method a design may call is declared. A test holds the two to each other, as a compiler holds a
definition to its declaration, since a call is checked against one and reaches the other.

**D3. A waiter is served in the order it arrived** (LRM 15.3.3, 15.4.5), and one that stops waiting
holds back nobody. Each kind of waiter -- a `get`, a bounded `put` -- is a list of the waiting
processes' handles in arrival order, and a process waits until it is first and what it asks for is
there, so a `semaphore` request for more keys at the head holds back a later one for fewer. Each
waiter is a process forked for the wait alone, so a caller that stops waiting by any route ends it:
killed, as the timeout `fork get(...); #N; join_any disable fork;` kills it (LRM 9.6.3, 9.7), or
left as a block around the call is disabled (LRM 9.6.2), which also ends what the call started.
Nothing in a process runs as it ends, so its place is not given back by it: the list drops a waiter
whose process has finished or been killed wherever it is next asked who is first, and the waiter
behind the first waits for the first to change or to end. A `try_` form succeeds only where its
waiting form would not wait. A waiter is woken by any change, so a change re-tests every waiter.

**D4. A message of a `mailbox` of no stated type is a value of any type, received only into a
variable of an equivalent type** (LRM 15.4.5, 15.4.9). What the front end gives the parameter is a
type no source spells, and the definition is the same text specialized on it. Each message is held
in an object of its own, since the front end admits no array of that type (a property of it is
admitted); a mailbox of a stated type holds its messages the same way, so the two share one text.
Taking a message is a `$cast` of it into the variable asking, which for a stated type asks nothing.

Two concepts are what the language library cannot state, and they are two, as Rust's `Box<dyn Any>`
and `&mut dyn Any` are and as C++'s `std::any` is apart from `function_ref`. A value of any type
owns its value, keeping it with that value's type and with the text of the type's equivalence class
(LRM 6.22.2), which every unit computes alone from the type, as rustc computes a `TypeId` in any
crate after erasing what does not count. A `ref` argument of no stated type is a reference to a
value of any type: it stands for a variable it does not own, with that variable's type and text and
a way to write it. A `put` makes a value of any type of the value handed; a `get` or a `peek` is
handed a reference to the asking variable, and a message is written through it only where the two
texts agree, the write landing in the variable when it is made, as any write through a `ref` does
(LRM 13.5.2).

A queue's bound is no part of which type it is (LRM 6.22.2 e), so neither text names one, and a
value is kept at its type with every bound dropped. A message received is written into storage of
that type, which the caller holds for the call, and the caller's own assignment then carries it into
the variable, applying the variable's bound as any assignment to it does (LRM 7.10.5).

## Rejected

- **A runtime library class per `std` class, as `process` is.** `process` reflects state only the
  runtime has; a `semaphore` or a `mailbox` holds keys, messages and the processes waiting, all of
  which SystemVerilog states, and the end of a waiting process it observes through that process's
  handle. A runtime waiter list could drop a killed waiter the moment it is killed; the handle
  reaches the same answer when the list is next asked. As runtime classes, a class extending one
  would need a library class as a base, which nothing provides, and `mailbox #(T)` would need the
  runtime to hold values of a type it is not compiled for; both backends would also need an entry
  per method.
- **Method bodies built in HIR by the compiler**, as clang builds an implicitly defined member's
  body. It writes SystemVerilog in C++, where no front end checks it and nobody reads it as source.
- **A source package named `std`.** The front end owns the name.
- **A package of another name.** It is a design element, so LRM 3.14.2.3 makes it an error beside
  any design element stating a time scale.
- **Waiters holding a ticket counted out on arrival.** A killed waiter keeps its ticket, so every
  waiter behind it waits for ever.
- **The type's address as its identity.** A record's type is stated by the unit declaring it and
  every queue shares one, so equal addresses neither follow from equal types nor answer for an
  element type.
- **MIR's own type identity as the equivalence class.** MIR keeps an enumeration by its members
  where the standard identifies one by its declaration (LRM 6.22.1), so two enumerations would test
  alike that the standard keeps apart.
- **A message received by copying it in after the call returns.** A `ref` writes the variable when
  the write is made (LRM 13.5.2); a copy made after `get` returns lets another process see the
  message gone and the variable not yet written.
- **One runtime object both owning a value of any type and standing for a variable.** An argument
  that owns its value is copied where a reference is not, and each operation then asks which of the
  two it holds.
- **A queue's bound in the text.** Two queues of one element and different bounds are equivalent, so
  a message put from one would be refused by the other.
- **A second definition for the `mailbox` of no stated type.** It differs from the other only in how
  a message is held, and two texts would each carry the waiting order, the bound and the `try_`
  rules.
- **Compiling the definitions only into designs that name one of their classes.** Every
  compilation-unit scope with content is compiled, and nothing records that a non-generic class was
  named before lowering; a `mailbox` specialization exists only where one is named. A cold C++ build
  measured 7.62 s against 7.41 s with the two classes beside a design that names nothing of them.
