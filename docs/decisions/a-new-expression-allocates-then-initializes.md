# A new-expression allocates, then initializes, and the pointer owning the storage moves

Date: 2026-10-12 Status: accepted; for a class object, superseded 2026-10-22 by
[an-object-is-created-whole-then-constructed](an-object-is-created-whole-then-constructed.md), which
creates the object whole and then constructs it. A box's `new T(init)` stands.

## Context

Three things brought storage of their own into existence and were each stated as a construction of
the type that owns the storage: a class `new` (a handle), an instance of the design hierarchy (a
uniquely owning pointer to an object), and a box holding a value too large for a frame (a uniquely
owning pointer to a value). A construction is a call: its arguments are values handed to the type's
own way of coming into existence. So nothing stated that the box's value is formed in the storage it
is kept in, and each consumer supplied that itself. The C++ render opened a `new` in the callee's
name and closed it after the argument list, a piece of syntax no node stood for; the lowering to the
execution IR told the three apart by testing the result type, and lowered a box as its operand,
looking through it. Two consumers each answering a question the program had not stated is the
failure the backend contract predicts.

The same gap was one layer down. The execution IR lends every call argument, and has no value that
cannot be copied, so a pointer owning what it points at could not be an owned value there: an
instance handed to the runtime's tree would have been ended by the caller as well. The box was
therefore realized as a slot kept apart from the frame, and the pointer MIR states never reached the
execution IR at all.

## Decision

**A new-expression is its own node: it allocates storage for one value of the type its owner points
at and evaluates its initializer into that storage, the way clang's `CXXNewExpr` and C++'s
`new T(init)` do.** An object's initializer is the construction of that object from its
constructor's arguments; any other initializer is the value itself. Its type is what owns the
storage from there: a uniquely owning pointer or a class handle. A construction never yields either,
and the unit's verification refuses one that does.

**In the execution IR a uniquely owning pointer is an owned value with no copy.** Its end ends what
it points at, where that lies, and gives the storage back, as Rust drops the value in a `Box` and
then frees it. Wherever it is handed on -- into a slot, to a caller, to a callee, as another pointer
type -- its end goes with it, as Rust moves a value that is not `Copy`, and a callee owns a
parameter of such a type. A new-expression lowers in clang's order: allocate storage nobody owns
yet, evaluate the initializer into it with a way out giving the storage back and ending nothing, and
only then make the owner from the storage. A class object is referred to while it is initialized,
since its constructor may hand `this` on (LRM 8.11): the part every object starts with holds its
creator's reference from the moment it is built, and the handle is made from that reference.

## Rejected

- **The initializer handed to the owner's construction, as `Box::new(x)` or `make_unique<T>(x)`
  is.** A call's argument is formed before the call, in the caller's frame, which is the thing a box
  exists to avoid. rustc meets the same problem in `Box::new`, whose body writes its argument
  through `write_via_move`, and lowers that one intrinsic as building the value into the pointer's
  destination "to avoid introducing unnecessary copies" (`rustc_mir_build`, `builder/expr/into.rs`).
- **The box realized below MIR as storage kept apart from the frame, with no pointer.** It needs a
  question asked of every local and every parameter -- is this a box -- answered from a type below
  the layer that decided it, and it leaves a box that is handed on, rather than declared, with no
  shape at all.
- **An argument lent, and the callee that takes ownership leaving the caller's pointer empty**, as
  the C++ ABI passes a `unique_ptr` by an object the caller destroys after the callee has moved out
  of it. That needs an empty state on every owning pointer and a test of it at every end; a move
  stated where the argument is passed needs neither.
