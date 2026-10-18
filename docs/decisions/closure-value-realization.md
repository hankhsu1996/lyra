# A closure value is an instance of its declaration, and its captures are member storage

Date: 2026-08-28. Status: accepted; D2 and D4 superseded by
[an-object-model-fixed-at-compile-time](an-object-model-fixed-at-compile-time.md) D1 and D4. The
execution backend lays a closure's captures out itself, so a closure value is built by an
instruction of its own -- the building code stores the address of the closure's definition at the
start of the storage the value lies in, as a constructor stores a class's table address, and fills
each capture the way its storage holds one -- rather than through the generic constructor protocol
with promotion driven by a schema. The closure's definition is a constant its unit emits, stating
its body and the body ending it. D1, D3 and D6 stand, and D5 is revised below.

## Why this decision matters

A deferred effect -- a non-blocking assignment, a postponed `$strobe`, a deferred assertion's action
-- is lowered as a closure the process hands to the engine and keeps running past
(`architecture/scheduling.md`). The closure is invoked after the stretch that built it has returned,
from a queue holding closures of many capture shapes and one signature.

The C++ backend gets this from the host language: the closure renders as a struct whose members are
its captures, and the host compiler lays it out. The execution backend has no host compiler to lay a
closure out. It needed an answer to three questions the C++ backend never had to ask: what the
environment is, who owns the captured values once the building stretch has returned, and how the
invoke reaches them.

`architecture/compiler_generated_storage.md` fixes the semantic shape -- capture fields plus one
invoke body, read through its receiver -- and leaves target realization open, naming
"payload-plus-code-pointer" as one possibility. This entry settles which realization the execution
backend uses, and it is not that one.

## The model

A closure declaration publishes a **definition**: the entry its body is, and the storage schema its
captures need. A closure value is an **instance** of that definition, owning one storage object per
capture. The pair the runtime holds is the definition and the instance.

```text
declaration  ->  definition { invoke entry, capture storage schema }
value        ->  instance   { definition, one storage object per capture }
```

This is the shape `member-slot-storage.md` already settled for an object's members, reached for here
rather than invented: the definition describes the schema, the instance owns the storage, and a
capture read is a member place. The two subjects share the field substrate at the semantic layer
already; sharing the realization is what makes the invoke's capture read need no vocabulary of its
own.

The capture kinds fall out of the schema vocabulary that exists:

```text
a captured pointer or reference  ->  a box holding a borrowed handle
a captured value                 ->  a value the instance owns, subscribed to by nothing
```

## The decisions

```text
D1. A closure declaration is its own LIR entity -- a name, its captures as members, and its invoke
    function. It is not a class: it has no base, no dispatch, and no constructor body, its captures
    being initialized where the value is built. It shares the member vocabulary and nothing else,
    which is the same relation the two categories have one layer up.

D2. A closure value is built through the generic constructor protocol, from the definition and the
    capture initializers in capture order. Nothing about a capture's representation is inspected
    where the value is built: each initializer crosses as the handle its own storage kind takes.

D3. The invoke takes its receiver, uniformly, and reads each capture as a member place. A
    declaration has exactly one body, so it has exactly one invoke signature and nothing has to
    decide which shape a declaration gets -- the receiver rule the semantic layer states is
    literally what the machine layer does.

D4. Constructing the value is where a captured value is promoted. A borrowed-handle slot stores the
    pointer it was handed; a value slot copies the value in. The copy is driven by the schema, in
    the one place that knows each slot's kind, so no generated code carries a promotion step and no
    transient handle is installed into storage that outlives the stretch that made it.

D5. A capture is written only where the closure's kind says so. The invoke's receiver is a
    read-only borrow for a closure that only reads its captures, a mutable one for a closure that
    keeps state between calls -- an event control's watch, which remembers what it last saw -- and
    the box for a body that takes the closure over. Revised 2026-10-17: this entry made every
    receiver read-only, which held while no closure kept state; the watch is the first that does.

D6. A closure's linkage name is qualified by its declaring unit. Its ordinal is counted per unit and
    the whole program links into one name space, which is the same reason a class and a namespace
    callable are qualified.
```

## Consequences

- The environment needs no representation of its own. It is member storage, so the schema
  vocabulary, the storage objects, the member place, and the address protocol are the ones an
  instance already uses, and a closure gains only its declaration, its definition, and the entries
  it is handed to.
- A capture whose type the runtime has no storage realization for is refused where any unrealized
  member is, and the refusal names the type. The set is the same set an object member draws from, so
  a capture cannot reach a storage kind an instance could not.
- The deferred-effect submits take a built closure into the region. This entry had them move the
  value there; [construct-in-final-home](construct-in-final-home.md) replaced that: the closure is
  made once where it will live, captures included -- for one a region keeps, a box MIR states -- and
  the region takes the box. The engine's own surface is unchanged; it still holds one callable per
  submitted effect.
- A closure whose body completes as a coroutine takes all of this and differs only in what becomes
  of the value it builds: the started body takes the closure's box over, because the execution
  outlives the stretch that built the captures and nothing else holds them. The schema, the
  instance, and the read through a receiver are the same either way.

## Rejected alternatives

- **A code address plus a product of the captures.** The shape `architecture/lir.md` names, and the
  first thing tried. A product's components are erased simulation values, and two of the three
  capture kinds measured -- a borrowed object pointer and a reference to a cell -- are neither: they
  carry no domain, answer no value operation, and widening the erased value to admit a machine word
  would give every struct component and array element an alternative that cannot occur in one. The
  environment is storage, not a value, and saying so is what makes the mixed kinds ordinary.

- **A generated drop function per closure, over a raw slot buffer.** It keeps the environment opaque
  to the runtime at the cost of a second emitted function per closure whose whole content is a
  release per owning slot -- which is the schema, spelled as code, in a place where the schema
  already crosses as data. Revised 2026-10-17: once the compiler lays every value out, no schema
  crosses for the runtime to walk, so the definition carries the body ending the closure, emitted by
  the unit as a class's definition carries its destructor.

- **Captures as the invoke's leading parameters.** This is what the lowering did while no closure
  escaped, and it was sound only for a body invoked where it was built -- which is a body that
  should never have been a callable value. Such a body is a block expression, so the shape it needed
  no longer exists, and what a closure is invoked with is its own signature rather than a prefix of
  captures every caller has to reconstruct.

- **Handing a closure to the runtime with no erased type at the semantic layer.** This entry first
  kept the erased callable
  [closure-environment-and-activation-frame](closure-environment-and-activation-frame.md) holds in
  reserve out of the IR, on the ground that no IR operation holds two closures of different bodies.
  That was true and was not the question. The runtime is compiled before the program, so the
  parameter of every entry taking a closure names a type the runtime can name, and a concrete
  closure is not one; each backend then bridged the gap in its own way -- one by a conversion its
  host language performed implicitly, allocating and taking ownership where MIR said neither.
  Revised 2026-10-17: the submit site casts the closure's box, or its borrow, to that of an erased
  callable of its signature (`architecture/compiler_generated_storage.md` invariant 10), which is
  the explicit erasure the reserve named, and the runtime reaches every closure through the
  definition this entry describes, on both backends. The definition carries the one call the
  closure's body is, at the body's signature, as a Rust `dyn FnMut(A) -> R` vtable carries its one
  call; whoever holds the closure holds it at that signature, which the runtime entry taking it
  states beside its bounds and the unit's verification checks, so nothing chooses which of several
  uses a closure is. The erased type also carries the closure's kind, as the principal trait of a
  rustc `dyn` type is part of that type, so whether what an entry is handed serves the kind it asks
  is read off the type handed rather than traced back to the closure that made it.

- **An erased box as two addresses, where the closure lies and its definition.** This is Rust's
  `Box<dyn Trait>`, a wide pointer the unsizing coercion makes, and it spares a closure that is
  never erased the definition's address. It was rejected for the shape C++ gives an object of a
  class with virtual functions, the address of its table at its start, which is already how every
  object of this program is laid out, so a box is one address wherever it is held, a closure's
  included, and the runtime reaches a closure it was handed as it reaches an object. The borrowed
  form is two addresses either way (`function_ref`, `&dyn`), since a borrow outlives no call.

- **Realizing a closure as a scope class.** The member storage would be reached by the entry an
  instance already has, at the price of giving a closure a base, a constructor, a dispatch table,
  and a place in the object tree -- the machinery `architecture/object_model.md` keeps the two
  categories apart to deny it. Sharing the storage vocabulary is the part that pays; sharing the
  category is the part that does not.

## Cross-references

- `../architecture/compiler_generated_storage.md` -- the semantic shape this realizes: capture
  fields plus one invoke, the receiver rule, and the capture form carried by the field's type.
- `../architecture/callable.md` -- callable code versus callable value, and that what the runtime is
  handed is an erased callable, no backend's choice.
- `../architecture/scheduling.md` -- a deferred effect is a closure submit rather than a suspension,
  which is what makes the region queue heterogeneous.
- [member-slot-storage](member-slot-storage.md) -- the schema-and-instance model this reaches for,
  and the member place a capture read becomes.
- [jit-value-realization](jit-value-realization.md) -- the opaque-handle baseline, the per-stretch
  transient store, and the consumption of a handle that must outlive its call.
- [activation-frame-and-transient-scope](activation-frame-and-transient-scope.md) -- the escape
  invariant D4 satisfies, which names a closure capture and a deferred effect outright.
- [jit-process-suspension](jit-process-suspension.md) -- the coroutine protocol a closure's invoke
  may state, and the runtime-owned coroutine entering one yields.
- [block-expression](block-expression.md) -- the node a body invoked where it is built became, which
  is what leaves a closure value only where one is genuinely handed over.
