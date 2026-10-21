# A value too large for a frame is held through a box MIR states

Date: 2026-10-15 Status: accepted. How the box comes into existence is
[a-new-expression-allocates-then-initializes](a-new-expression-allocates-then-initializes.md); how a
holder keeps such a value is
[a-holder-lays-out-the-value-it-keeps](a-holder-lays-out-the-value-it-keeps.md). Revised 2026-10-21:
a value a borrow refers to is made in a temporary of the evaluation too, where it used to be left
inside whichever step formed the borrow and ended with it.

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
moves no value read ahead of another. A step is a statement, and ends what it makes, so a value a
borrow refers to that the evaluation makes is made in a temporary of the evaluation as well,
whatever its size -- a place inside such a value held as the value read from it -- since the
reference may be carried into a temporary a step declares and read after: rustc makes a borrowed
value a local living to the end of its statement (`builder/expr/as_temp.rs`), and clang keeps one to
the end of the full-expression. Go's `order` pass needs no such rule only because what a Go
temporary refers to is collected rather than ended. What is bound where it lies is formed where it
is used -- a place keeps its own operands there, and the object a call is entered on may be a
reference to part of a value, which exists only where it is taken -- so a later operand's step can
run before it, an order the standard does not fix: it states none for an operator's operands, which
it requires only to be evaluated (LRM 11.3.5), and leaves a call's arguments' undefined (LRM 13.5).
The steps run in a block expression whose value is made where the evaluation's value is; an
execution's answer has no such place, being read and stored into its completion, so its steps run in
a scope around the return, which reads the temporary where it lies. The call a suspension waits on
stands under it as it is, so each of its arguments is an evaluation of its own. A box handed to a
body is built where it is handed, since the body owns it from there and nothing reads it here.

**D3. The box is written by two rewrites of the MIR-to-MIR stage, one job each**, after HIR-to-MIR
has stated every value plainly, and both backends read the rewritten unit. The first makes every
large value an expression makes and then reads in a temporary, as above; the second holds every
large local -- the temporaries among them -- parameter, capture, argument handed to a body and value
made and discarded through a box. Both are required. Go's compiler divides the work the same way:
one pass states evaluation order through temporaries (`cmd/compile/internal/walk/order.go`), and a
later one decides which variables leave the frame. Each rewrite asks a type how large a value of it
is by one query over MIR types, as rustc's passes ask `layout_of`; no MIR node carries a size. As
rustc's query and clang's record layout are, the answer is worked out once per type and kept with
the unit, because a structure's size is worked out from every member and is asked at every value of
it. A closure is sized by its captures as it holds them, as D2 says, so the rewrite that boxes a
capture changes no closure's size, and the kept answers hold for the unit's whole life. The unit's
verification refuses a body that leaves a value too large for a frame outside a box.

**D4. A large value is made in its box, and no body answers a box.** An expression of a large type
is evaluated into the storage its new-expression allocates, never into storage of its own first.
Every body that is not a coroutine answers into its return place, and a caller reading a large call
as a value makes it in a box: the C++ backend writes `new T(f())`, which elides, and the execution
backend passes the box's storage as the return place.

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
- **Making temporaries and boxes in one rewrite.** Every case of the first -- a value handed to a
  body, a suspension's call, an execution's answer, a value discarded -- then grew a case of the
  second, and the rewrite came to know the evaluation order of every node in order to place a box.
  Apart, the boxing rewrite holds locals, and a temporary is a local.
- **A body answering a box.** Once a body has a return place, as rustc's `_0` and clang's `sret`,
  answering a box is a second convention beside it.

## Consequences

- No frame's size follows a declared size on either backend; a design whose every value is twice the
  stack it runs on runs on both.
- The execution backend keeps nothing apart from a frame on its own account.
- A holder keeps a large value in storage of its own, as the holder record states, so a cell is
  never boxed.
