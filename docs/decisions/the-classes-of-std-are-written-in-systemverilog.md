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
another unit: HIR, MIR, both backends and the runtime carry nothing of their own for it.

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

**D4. A `mailbox` of no stated type is refused until it is carried.** Its messages are of any type
and each is checked at run time against the variable it is read into (LRM 15.4.9), which needs a
value that carries its type and a test of type equivalence at run time; no definition written in
SystemVerilog can state either.

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
- **Compiling the definitions only into designs that name one of their classes.** Every
  compilation-unit scope with content is compiled, and nothing records that a non-generic class was
  named before lowering; a `mailbox` specialization exists only where one is named. A cold C++ build
  measured 7.62 s against 7.41 s with the two classes beside a design that names nothing of them.
