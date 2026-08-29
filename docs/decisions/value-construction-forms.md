# A construction says which thing it is

Date: 2026-08-28 Status: accepted

## Context

`mir::Construct` is a callee that carries nothing. A call whose callee is `Construct` means "build a
value of this call's result type from these arguments", and every consumer recovers what to do by
reading the result type.

That works while a type has exactly one way to be built. It stops working the moment one does not,
and both backends carry the evidence:

- The C++ backend's construct render branches four ways on the result type -- an owning pointer to
  `std::make_unique`, a shared pointer to `std::make_shared`, a managed reference to a heap
  allocation, everything else to the type's own name.
- The LLVM backend could not read the result type alone, because a dynamic array has four ways to be
  built. It told them apart by counting operands and inspecting the second one's type: a machine
  array meant an element list, anything else meant a size. `new[N]` and an assignment pattern both
  carry two operands, so nothing else separated them.

The second is the load-bearing observation. The C++ backend never had to make that choice because
C++ overload resolution made it -- the host compiler picked the constructor from the argument types.
Overload resolution is a facility of one target language, not a semantic fact, so a backend without
it has to re-derive what the source wrote. `backend_contract.md` invariant 6 names this exact
situation as the falsifier for a MIR shape, and `mir.md` forbids "a primitive that stands for more
than one operation, leaving a consumer to pick which by reading its result type".

The same node also stands for something that is not a construction at all: crossing the enumeration
boundary, where a value keeps its bits and changes only the type ascribed to it.

## The forms, and where each already belongs

Nothing here is new vocabulary. Each form already has a home in the architecture documents; what was
missing is that a construction says which one it is.

1. **A value that is its own parts.** A product from its components, an element list from its
   elements. `mir.md` Owns lists "value-build primitives for aggregate construction" among MIR's
   primitives, and its Notes say they have no smaller decomposition. This is a primitive, not a
   call.

2. **An operation a library performs.** Building a `String` from text, building a container over an
   element list, sizing a container at run time (LRM 7.5.1 `new[N]`), reshaping a value into another
   representation. `mir.md` requires such an operation to appear as "an ordinary `CallExpr` against
   the library type's API" -- against the API, so the call names the entry it invokes.

3. **Re-typing a value without moving its bits.** Crossing between an enumeration and its base (LRM
   6.19.3). `mir.md` fixes a cast node as naming exactly one operation, and lists re-typing a
   reference among them; the value counterpart is the same kind of node.

4. **Bringing an object into existence under an owner.** An owning pointer and a managed reference
   are the composing wrappers `mir.md` Owns lists, and a member whose type is one of them holds a
   value like any other -- so this is the second form again, seen from the owner. What differs
   between owners is only what the target spells, which is type mapping's answer.

## Decision

**A construction states which form it is, in its own structure. No consumer recovers the form from
the result type, from the operand count, or from an operand's type.**

### D1. A value that is its own parts is a primitive; a container built from one is not

A product is its components: a `TupleExpr` lowers to LIR's `ProductInstr`, and codegen assembles the
value itself, naming no entry. An element list is its elements the same way: an `ArrayLiteralExpr`
lowers to an `ArrayInstr`, contiguous storage named by a span. Both have a smaller decomposition
into nothing, which is what makes them primitives.

A container holding that list is not its elements. It is a library type, with a representation the
library owns, and it comes into existence the way every library type does -- through its own
constructor, invoked with the list. So the list is the primitive and the container is D2: the
element list, how many times it repeats, the element default the container is seeded with, and a
bounded queue's declared bound are the arguments of that call, and every one of them is an operand.

The tell is what a backend has to write. A build primitive over a container renders as
`Container(seed, list, count)` -- a callee, parentheses, comma-separated arguments -- text no node
in the IR stands for, composed by the backend out of a shape it chose. A backend translates what the
IR states and composes nothing, so a render that has to compose a call is looking at a call.

This is also what keeps the realization question a backend's own. `jit-aggregate-realization.md`
settles that an aggregate on the execution backend is realized by erasure, and that the decision "is
entirely below LIR" -- LIR's aggregate operations are "realization-agnostic logical value
operations". The list is such an operation: the same `ArrayInstr` becomes a span today and a
physical layout later, with no consumer changing. What the container does with the list is the
container's own, and naming its type is what names it.

### D2. A library operation names its entry

Sizing a container at run time has no parts to be built from -- it has a size and an element default
-- so it is an operation, and it names which one. `new[N]` and `new[N](src)` are different entries,
not one entry told apart by how many operands arrived.

The identity is `support::BuiltinFn`, qualified by the type whose namespace declares the entry,
which is the shape `builtin-call-identity.md` already fixes for every other runtime entry: the
receiver or qualifier carries the type-side context, the callee carries the function-side identity.

### D3. Re-typing a value is a cast, not a construction

A value whose representation is unchanged and whose type is not has nothing built. Under the erased
value model a handle is immutable, so the operation emits nothing at all; under a native one it is
equally nothing. A node that calls this a construction obliges every backend to discover that the
construction constructs nothing.

### D4. What names a construction is the type's own answer, read through type mapping

`std::make_unique<T>`, `std::make_shared<T>`, and a managed-heap allocation are three spellings of
one operation over three types, so choosing among them is not a decision a construction makes -- it
is what those types are called when one is built, which is the same question type mapping already
answers for what they are called when one is named. A value-emission entry that picks among them is
holding a target-language spelling it has no business knowing, and picks it with an `if` whose arms
produce different syntax -- the shape `backend_contract.md` invariant 2 forbids and invariant 3
already has a home for.

There is no separate allocation form. A composing wrapper that owns what it points at is a value of
the program, so bringing one into existence is D2, and the pointee comes into existence with it.

## Consequences

- `mir::Construct` narrows to the second form alone, and says so: a library type's one way to come
  into existence, which naming the type names. A type that gains a second way does not gain a branch
  -- it names both, the way the run-time-sized array constructions do.
- A backend's construct path has no decision in it. Which entry a construct resolves to is read from
  the result type the same way extract and update read theirs -- a representation choice, not a
  semantic one -- and the arguments are forwarded as the call states them.
- A container's declared bound (LRM 7.10.5) is an operand of the construction, because it is a value
  the constructor takes. A fact may live on the result type; a value the target spells has to be
  materialized by the IR, or the backend is composing it.
- Adding a container is adding a construct entry and the element representation it takes, not a new
  construction form.
- How a value is represented stays wholly below the call. A homogeneous construction is named by the
  representation its elements take, so the entry converts its own operands into that representation
  and a caller states values rather than the form they are held in. A heterogeneous one -- a
  product, whose components each have a representation of their own -- is the one place a caller can
  state it, because no entry can be named by one component's answer.

## Rejected alternatives

- **Keep the nameless construct and let MIR-to-LIR resolve it.** Moves the same operand inspection
  one layer up. The information it recovers is known where the source construct was read, so
  recovering it anywhere later is re-derivation wherever it happens.

- **Make the container built from an element list a primitive too, beside the product.** Symmetric
  on the page and wrong in the backend: a product is assembled from its components, while a
  container is a library type whose constructor takes the list, so the primitive's render had to
  spell that constructor call itself. A primitive whose realization is a call the IR does not state
  is a call the IR is hiding.

- **Let the result type keep selecting, and give the ambiguous types more operands.** A
  discriminator operand beside the type is the parallel-classifier shape `mir.md` forbids for
  members, for the same reason: the structure already has room to say it.

## Cross-references

- `../architecture/mir.md` -- value-build primitives are MIR primitives; a primitive means one
  operation; a cast names exactly one.
- `../architecture/backend_contract.md` -- no decision logic in a value-emission entry, and the
  mechanical-LLVM-backend falsifier.
- `jit-aggregate-realization.md` -- erasure is below LIR; LIR's aggregate operations are
  realization-agnostic.
- `builtin-call-identity.md` -- the flat entry identity a named library operation uses.
- `concatenation-realization.md` -- the same defect in the join primitive, settled the same way.
