# A value too large for a frame is held through a box MIR states

Date: 2026-10-15 Status: accepted. How the box comes into existence is
[a-new-expression-allocates-then-initializes](a-new-expression-allocates-then-initializes.md); how a
holder keeps such a value is
[a-holder-lays-out-the-value-it-keeps](a-holder-lays-out-the-value-it-keeps.md).

## Context

A program the compiler accepts has to run at every size it accepts. SystemVerilog declares integral
widths up to 2^24 - 1 bits and arrays and structures of any count, and a value lies in the frame of
the function evaluating it, so a frame laid out from declared types takes from the stack whatever
the design declares; the stack's size is not the design's to choose.

Each backend used to decide this alone, from one shared constant. The C++ value types owned their
storage past it -- an integral its words, an unpacked array its elements -- and the execution
backend kept a large local apart from its frame. One fact was stated twice, every temporary of such
a type allocated, and neither covered a structure or a union of many small members, whose C++ type
owned nothing and so lay whole on the stack.

## Decision

**D1. Every type is laid out flat at every size, on both backends.** An integral is its words, an
array its elements, a structure its members, a union its members overlaid. No value type owns
storage of its own on account of its size.

**D2. A position where a value would lie in a frame holds a value of a type too large for a frame
through a uniquely owning pointer MIR states.** The positions are a local, a parameter taken by
value, a value an expression makes and a closure's capture by value. A parameter is boxed wherever
its signature is stated -- the body taking it and the type of an address of that body alike -- so a
call through a code address passes what the body takes. A conditional and a block expression are not
positions of their own: the arm a conditional chooses and the value a block ends with are made where
the conditional's or the block's value is, as clang evaluates both into their destination and rustc
a block's tail into its destination. A closure is as large as the captures it holds, each as it
holds it. What a suspending call completes with lies in storage the runtime allocates for its
awaiter, never in a frame, so it is not boxed. A value inside another value, an object or a
container lies flat there; whatever holds that whole is boxed if it is large. The bound is the value
library's `kMaxInlineBytes`, 1,024 bytes.

**D3. The box is written by a rewrite of the MIR-to-MIR stage**, after HIR-to-MIR has stated every
value plainly, and both backends read the rewritten unit. The rewrite asks a type how large a value
of it is by one query over MIR types, as rustc's passes ask `layout_of`; no MIR node carries a size.
The unit's verification refuses a body that leaves a value too large for a frame outside a box.

**D4. A large value is made in its box, and no body answers a box.** An expression of a large type
is evaluated into the storage its new-expression allocates, never into a temporary first. Every body
that is not a coroutine answers into its return place, and a caller reading a large call as a value
makes it in a box: the C++ backend writes `new T(f())`, which elides, and the execution backend
passes the box's storage as the return place.

**D5. What the value library keeps aside while it works is never a large value on its stack.** A
function compiled once for every type that needs a whole value of its own -- a copy for a
permutation, the first member of a union read for a comparison -- keeps it beside itself only while
it is small, and in storage of its own past that.

**D6. A closure kept past the statement building it is made in a box MIR states, and whatever keeps
it takes the box.** A region it is submitted to, a timer, an observation, an assertion's action and
a foreign fiber each outlive the frame that built the closure, so it never lies there, as Rust's
thread spawn keeps the closure it is handed in a `Box`. A closure borrowed for a call -- the order a
sort is handed -- lies where its value is. A closure started as a coroutine is made in a box MIR
states and called through it, and the started body takes the box over. No backend places a closure
on its own account: a box of a closure of a known type is an ordinary box, which both backends spell
and end as they do any box, and each builds the closure, its library's part included, where its
value lies.

## Where Lyra differs, and why

| Point                         | Field                                                                                           | Lyra                      | Reason                                                                          |
| ----------------------------- | ----------------------------------------------------------------------------------------------- | ------------------------- | ------------------------------------------------------------------------------- |
| who writes the box            | the programmer, in Rust (`Box<T>`) and C++ (an owning type)                                     | the compiler              | SystemVerilog has no word for where a value lives                               |
| where the compiler decides it | Go, in the middle end that owns layout (`escape/utils.go` `HeapAllocReason`, `MaxStackVarSize`) | MIR, before both backends | two backends read one unit, and each deciding again is what this record removes |
| how a box is filled           | rustc builds `Box::new`'s argument into the box (`builder/expr/into.rs`, `write_box_via_move`)  | the same                  | no condition of ours differs                                                    |
| a large parameter             | rustc keeps `fn(T)` and passes a large `T` by address through one ABI query (`abi.rs`)          | a box in every signature  | the C++ backend has no ABI layer of its own, so the box is stated in MIR        |
| a kept closure                | Rust's spawn takes `Box<dyn FnOnce>` (`thread/lifecycle.rs`); Go heap-allocates an escaping one | a box MIR states          | no condition of ours differs                                                    |

## Rejected

- **Each backend deciding from a shared constant.** It states one fact twice, allocates every
  temporary of a large type, and covers only the types that own their storage, so a structure of
  many small members still lay whole on the stack.
- **The C++ render choosing a holder per position by the type's size.** A render chooses spellings
  and never by a type's payload, and the host compiler lays out the C++ frames.
- **A body answering a box.** Once a body has a return place, as rustc's `_0` and clang's `sret`,
  answering a box is a second convention beside it.

## Consequences

- No frame's size follows a declared size on either backend; a design whose every value is twice the
  stack it runs on runs on both.
- The execution backend keeps nothing apart from a frame on its own account.
- A holder keeps a large value in storage of its own, as the holder record states, so a cell is
  never boxed.
