# The classes of `std` are written in SystemVerilog

Status: Accepted, 2026-10-20.

## Requirement

`semaphore`, `mailbox` and `weak_reference` are classes of the built-in package `std` (LRM 8.30,
15.2, 26.7), and a program makes them with `new`, calls their methods and may extend them. The front
end creates `std` itself, declaring each class and method with no body, and type checks every call
and derivation against that declaration, so something has to define these classes and nothing in the
program does. Whatever defines them must let a design's class extend one, hold a `mailbox` of any
element type, and carry nothing every layer below the front end has to know about.

## Choice

**What `std`'s classes do is SystemVerilog Lyra carries, compiled as any class is.** Every
compilation reads one text of Lyra's own beside the design's sources, whose compilation-unit scope
defines each class the front end declares in `std`, under the name, parameters and methods the
declaration gives it, and that scope is compiled as the unit `std`. To everything after the front
end such a class is a class of another unit, and HIR, MIR, both backends and the runtime carry
nothing of their own for it. A definition is what its declaration says, and a test holds the two to
each other.

What only the runtime has stays the runtime's, decided per method rather than per class, so
`weak_reference`'s `get_id`, which answers an object's identity, is carried out by the runtime as
`process` is.

This is how the field supplies a language's library, in the language its users write, over one
waiting primitive -- Rust's channel in Rust, C++'s `counting_semaphore` in C++, SystemC's `sc_fifo`
in C++, Verilator's `verilated_std.sv` in SystemVerilog.

## Alternatives turned down

- **A runtime library class per `std` class, as `process` is.** It fails a design extending one,
  since a source class would need a library class as its base, which nothing provides, and it fails
  a `mailbox #(T)` holding values of a type the runtime was not compiled for. The failure rests on a
  `semaphore` or a `mailbox` holding only what SystemVerilog states -- keys, messages, waiting
  processes -- unlike `process`, which reflects state only the runtime has.
- **Method bodies built in HIR by the compiler**, as clang builds an implicitly defined member. It
  fails a definition being checked, since SystemVerilog written as C++ is checked by no front end
  and read by nobody as source.
- **A source package named `std`.** It fails a front end that owns the name and reports it
  duplicated.
- **A package of another name.** It fails designs that state a time scale, since a package is a
  design element, and LRM 3.14.2.3 makes it an error for some design elements to state one and
  others not. A compilation-unit scope is no design element.

## Consequences across layers

- How each class waits, holds messages of any type and holds its referent weakly is stated by its
  SystemVerilog text and its section of [`object_model.md`](../architecture/object_model.md).
- The definitions are compiled into every compilation, since nothing records before lowering that a
  design names one of these classes.
