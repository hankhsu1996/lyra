# A value too large for a frame is held through a box MIR states

Date: 2026-10-15 Status: accepted. How the box comes into existence is
[a-new-expression-allocates-then-initializes](a-new-expression-allocates-then-initializes.md); how a
holder keeps such a value is
[a-holder-lays-out-the-value-it-keeps](a-holder-lays-out-the-value-it-keeps.md). Revised 2026-10-21:
a value a borrow refers to is made in a temporary of the evaluation too, where it used to be left
inside whichever step formed the borrow and ended with it. Revised 2026-10-24: an operand read out
of storage a later operand may change or end is taken into a temporary too, where D2 used to let a
later operand's step run before what is bound where it lies; a later operand dropping the last
handle to the object a call is entered on reclaimed it before the call ran. Revised 2026-10-27: a
bound on each value did not bound a frame -- 600 locals of 1,024 bytes, each within it, overflowed a
512 KB stack on both backends -- so a frame now also has a bound on what it keeps together (D7);
making the evaluation order explicit no longer asks any value's size, and the storage rewrite gives
a value it keeps apart a temporary in the order that rewrite left (D3).

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

A value an expression makes and then reads is made in a temporary: a local of the body, declared by
a step of the evaluation the value belongs to, held through a box as every large local is, and ended
with that evaluation, as a C++ temporary ends with its full-expression. The reader names the
temporary, which encloses nothing, so a chain the source writes flat -- three hundred additions of
wide values -- is three hundred steps beside each other rather than each link enclosing the one
before, which a target compiler bounds. An evaluation is a statement's expression, an arm of a
choice, or the value a block expression ends with, and its steps run in the order the evaluation
reads its operands: an operand read or handed to a body before one that takes a step, and that
computes something, is taken into a temporary of its own first, so holding a value off the stack
moves no value read ahead of another. So is an operand read out of storage before one with side
effects -- a call running code, a write, a suspension -- unless nothing after it can change what it
read. A suspension stands at the top of its statement, so where a task enable's operand is taken
into a temporary the statement runs after the steps declaring it: in a block of its own, or, for a
declaration, just before it in its own block. rustc's `as_operand` takes every place into a
temporary "so that we capture the value of `x` at this time" (`builder/expr/as_operand.rs`), leaving
one out only for the operand converted last (`NeedsTemporary::No`); here an operand is left where it
is unless what runs after it could change it. A step is a statement, and ends what it makes, so a
value a borrow refers to that the evaluation makes is made in a temporary of the evaluation as well,
whatever its size -- a place inside such a value held as the value read from it -- since the
reference may be carried into a temporary a step declares and read after: rustc makes a borrowed
value a local living to the end of its statement (`builder/expr/as_temp.rs`), and clang keeps one to
the end of the full-expression. Go's `order` pass needs no such rule only because what a Go
temporary refers to is collected rather than ended. What is bound where it lies is formed where it
is used -- a place keeps its own operands there, and the object a call is entered on may be a
reference to part of a value, which exists only where it is taken -- so a later operand's step can
run before it, an order the standard does not fix: it states none for an operator's operands, which
it requires only to be evaluated (LRM 11.3.5), and leaves a call's arguments' undefined (LRM 13.5).
That holds of where the value lies and not of whether it still exists: the object a call is entered
on is in use until the call takes it, and the standard reclaims only an object "no longer being
used" (LRM 8.29), which is why it runs last, after the arguments that could end it. The steps run in
a block expression whose value is made where the evaluation's value is; an execution's answer has no
such place, being read and stored into its completion, so its steps run in a scope around the
return, which reads the temporary where it lies. The call a suspension waits on stands under it as
it is, so where none of its arguments is taken ahead, each is an evaluation of its own. A box handed
to a body is built where it is handed, since the body owns it from there and nothing reads it here.

**D3. The box is written by two rewrites of the MIR-to-MIR stage, one job each**, after HIR-to-MIR
has stated every value plainly, and both backends read the rewritten unit. The first makes the
evaluation order explicit through temporaries -- every operand a later one could change, and what a
borrow refers to -- and asks no value's size, so a change to one type's size moves only where values
of it lie. The second decides where every value lies: it plans the frame (D7), gives each value it
keeps apart that an expression makes and then reads a temporary, in the order the first left, and
holds every value kept apart -- the temporaries among them, and a value a statement discards, made
in a box the statement discards with it -- and every large parameter, capture and argument handed to
a body through a box. Placing a temporary keeps the order through the same rule the first rewrite
follows -- an operand that computes something and is evaluated before one taking a step is taken
first -- so the second knows no evaluation order of its own. That is an obligation of the second
rewrite and not something a check after it can establish: it keeps how many times each operand runs,
under which conditions, in which order, when each value is taken, who owns it, where a closure was
made and when each value ends, and the check of the legalized form proves only that no order is left
to a target. Both are required. Go's compiler divides the work the same way: one pass states
evaluation order through temporaries (`cmd/compile/internal/walk/order.go`), and a later one decides
which variables leave the frame. The second asks a type how large a value of it is by one query over
MIR types, as rustc's passes ask `layout_of`; no MIR node carries a size. As rustc's query and
clang's record layout are, the answer is worked out once per type and kept with the unit, because a
structure's size is worked out from every member and is asked at every value of it. A closure is
sized by its captures as it holds them, as D2 says, so the rewrite that boxes a capture changes no
closure's size, and the kept answers hold for the unit's whole life. The unit's verification refuses
a body that leaves a value too large for a frame outside a box.

**D4. A large value is made in its box, and no body answers a box.** An expression of a large type
is evaluated into the storage its new-expression allocates, never into storage of its own first.
Every body that is not a coroutine answers into its return place, and a caller reading a large call
as a value makes it in a box: the C++ backend writes `new T(f())`, which elides, and the execution
backend passes the box's storage as the return place.

**D5. What the value library keeps aside while it works is never a large value on its stack.** A
function compiled once for every type that needs a whole value of its own -- a copy for a
permutation, the first member of a union read for a comparison -- keeps it beside itself only while
it is small, and in storage of its own past that.

**D6. A closure kept past the statement building it is made where MIR states its keeper keeps it.**
A region it is submitted to, a timer, an observation, an assertion's action and a foreign fiber each
outlive the frame that built the closure, so it never lies there. A region holds an effect exactly
until it runs it, so the effect is made in the storage the region's effects hand out, and they take
it over; every other keeper takes a box MIR states, as Rust's thread spawn keeps the closure it is
handed in a `Box`. A closure borrowed for a call -- the order a sort is handed -- lies where its
value is. A closure started as a coroutine is made in a box MIR states and called through it, and
the started body takes the box over. No backend places a closure on its own account: a box of a
closure of a known type is an ordinary box, which both backends spell and end as they do any box,
and each builds the closure, its library's part included, where its value lies.

**D7. A frame keeps at most a fixed sum of a body's values, 16 KiB, and every value past it is held
apart through a box.** A bound on each value bounds a frame only by how many values the source
writes, so a body declaring 600 locals of 1,024 bytes, or an expression of 600 such operands, took
600 KB of a stack the design did not size. The values counted are a value of the design -- an
integral, an array, a structure, a string, what the library holds as a value -- each larger than the
address that would stand in its place: the locals a body declares, in the order it declares them,
then each value one node makes and another reads or its statement discards, block by block. A node
that computes nothing names a value lying elsewhere, an arm of a choice is made where the choice's
value is, what a new-expression initializes is made in its storage and what a suspension waits on is
the runtime's, so none of them is counted; a parameter lies where the signature the source wrote
puts it, and only one too large for a frame is held apart. What the library builds for a call and
machine data are as many as the calls the source wrote, and a frame keeps them as the target's own
code keeps them. A variable's cell keeps its value inline, so a cell counts that value, and past the
budget the cell itself is held through a box -- whether the frame keeps the holder, apart from
whether the holder keeps the value inline. The unit's verification refuses a body whose frame keeps
more. The budget is a constant of the representation, beside the bound on one value.

So no width a design declares reaches a frame, and the values a body keeps past the budget cost it
an address each. That bounds what MIR states and not the target's whole frame, which still follows
how many things the source writes: a parameter, what the library builds for a call, and what a code
generator keeps of an operation's operands while it works, each of a size no declaration grows past
the bound on one value. Every compiled program's frame does the same. Clang reports a frame past a
size it is given (`-Wframe-larger-than`, read after register allocation by LLVM's
`PrologEpilogInserter`) and clippy's `large_stack_frames` sums a function's locals, temporaries and
arguments; both only report, and the remedy clippy names is to hold the value through a `Box`. The
budget applies that remedy for the author, since the source language gives them no way to write it.
The stack a simulation runs on is the one its user started it with, as a C++ or Rust program runs on
the stack its process was given.

## Where Lyra differs, and why

| Point                         | Field                                                                                                                                                                                                                                                                                                                                        | Lyra                                                                          | Reason                                                                                                            |
| ----------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| who writes the box            | the programmer, in Rust (`Box<T>`) and C++ (an owning type)                                                                                                                                                                                                                                                                                  | the compiler                                                                  | SystemVerilog has no word for where a value lives                                                                 |
| where the compiler decides it | Go, in the middle end that owns layout (`escape/utils.go` `HeapAllocReason`, `MaxStackVarSize`)                                                                                                                                                                                                                                              | MIR, before both backends                                                     | two backends read one unit, and each deciding again is what this record removes                                   |
| how a box is filled           | rustc builds `Box::new`'s argument into the box (`builder/expr/into.rs`, `write_box_via_move`)                                                                                                                                                                                                                                               | the same                                                                      | no condition of ours differs                                                                                      |
| a large parameter             | rustc keeps `fn(T)` and passes a large `T` by address through one ABI query (`abi.rs`)                                                                                                                                                                                                                                                       | a box in every signature                                                      | the C++ backend has no ABI layer of its own, so the box is stated in MIR                                          |
| a kept closure                | Rust's spawn takes `Box<dyn FnOnce>` (`thread/lifecycle.rs`); Go heap-allocates an escaping one                                                                                                                                                                                                                                              | a box MIR states                                                              | no condition of ours differs                                                                                      |
| a made value that is read     | rustc builds it into a fresh temporary local, a statement of its own (`builder/expr/as_temp.rs`); clang gives it storage for the full-expression (`CodeGen/CGExpr.cpp`, `createReferenceTemporary`); Go's `order` pass makes it a temporary, a statement before the one it serves or part of an expression's own init list (`walk/order.go`) | a temporary local, a step of a block expression, made by a rewrite of its own | MIR is a tree whose C++ text a compiler parses recursively, so a value built where it is read nests in its reader |

## Rejected

- **Each backend deciding from a shared constant.** It states one fact twice, allocates every
  temporary of a large type, and covers only the types that own their storage, so a structure of
  many small members still lay whole on the stack.
- **The C++ render choosing a holder per position by the type's size.** A render chooses spellings
  and never by a type's payload, and the host compiler lays out the C++ frames.
- **A made value boxed where it is read.** The box encloses the expression making the value and the
  reader encloses the box, so a flat chain of N links nests three brackets a link, and a sum of a
  few hundred wide values is text no C++ compiler has to accept.
- **Making the evaluation order and the boxes in one rewrite.** Every case of the first -- a value
  handed to a body, a suspension's call, an execution's answer, a value discarded -- then grew a
  case of the second, and the rewrite came to know the evaluation order of every node in order to
  place a box. The storage rewrite of D3 does place temporaries, but through the order-keeping rule
  the first rewrite follows, never a rule of its own, and its boxing holds locals, of which a
  temporary is one.
- **Making the order explicit with a temporary for each large value made**, as this record first
  did. The first rewrite then asked every value's size, so how a type was sized changed the shape of
  every evaluation over it, and a bound on the frame could not be added without a second, different
  set of temporaries.
- **One heap frame per scope holding the values past the budget, made at scope entry**, as Roslyn's
  closure conversion makes one per scope that has captured variables (`ClosureConversion.cs`,
  `IntroduceFrame`). A frame made first needs each moved value's value-before-anything-is-written,
  which only HIR-to-MIR can build -- a structure's needs its declaration's member initializers --
  and making it from the initializers fails where a later one reads an earlier moved local. A box
  per value costs the frame an address each, which is what a frame costs per declaration anyway.
- **A body answering a box.** Once a body has a return place, as rustc's `_0` and clang's `sret`,
  answering a box is a second convention beside it.

## Consequences

- No frame's size follows a declared size on either backend; a design whose every value is twice the
  stack it runs on runs on both, and so does one whose values are each within the bound and together
  many times the stack, in a function, in a process before its first wait and in a branch it forks.
- The execution backend keeps nothing apart from a frame on its own account.
- A holder keeps a large value in storage of its own, as the holder record states, and decides it by
  the one rule every holder reads, a value's size and alignment; a cell the frame has no room for is
  held through a box, which the execution backend builds in the storage it allocates, as a body
  builds its variables.
- A body declaring more than the budget pays an allocation for each value past it, and nothing else
  changes for it.
