# A closure value is an instance of its declaration, and its captures are member storage

Date: 2026-08-28. Status: accepted.

## Why this decision matters

A deferred effect -- a non-blocking assignment, a postponed `$strobe`, a deferred assertion's action
-- is lowered as a closure the process hands to the engine and keeps running past
(`architecture/scheduling.md`). The closure is invoked after the stretch that built it has returned,
from a queue holding closures of many capture shapes and one signature.

The C++ backend gets this from the host language: the closure renders as a lambda, its captures are
the lambda's own fields, and the conversion into the region's `std::function` is the language's. The
execution backend has no host compiler to lay a lambda out. It needed an answer to three questions
the C++ backend never had to ask: what the environment is, who owns the captured values once the
building stretch has returned, and how the invoke reaches them.

`architecture/compiler_generated_storage.md` fixes the semantic shape -- capture fields plus one
invoke body, read through a read-only receiver -- and leaves target realization open, naming
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

D3. The invoke takes its receiver, uniformly, and reads each capture as a member place. Every
    closure that survives is one a holder invokes later, so there is one invoke signature per
    declaration and nothing has to decide which shape a declaration gets -- the receiver rule the
    semantic layer states is literally what the machine layer does.

D4. Constructing the value is where a captured value is promoted. A borrowed-handle slot stores the
    pointer it was handed; a value slot copies the value in. The copy is driven by the schema, in
    the one place that knows each slot's kind, so no generated code carries a promotion step and no
    transient handle is installed into storage that outlives the stretch that made it.

D5. A capture is read-only. The invoke's receiver is a read-only borrow, so the only write a slot
    ever takes is the copy its construction performs, and the machine layer needs no store path
    through a capture place at all.

D6. A closure's linkage name is qualified by its declaring unit. Its ordinal is counted per unit and
    the whole program links into one name space, which is the same reason a class and a namespace
    callable are qualified.
```

## Consequences

- The environment needs no representation of its own. It is member storage, so the schema
  vocabulary, the storage objects, the member place, and the address protocol are the ones an
  instance already uses, and a closure gains only its declaration, its definition, and the entries
  that build and submit one.
- A capture whose type the runtime has no storage realization for is refused where any unrealized
  member is, and the refusal names the type. The set is the same set an object member draws from, so
  a capture cannot reach a storage kind an instance could not.
- The deferred-effect submits take a built closure and move it into the region, which is the
  established consumption of a handle that must outlive the call it was built in
  ([jit-value-realization](jit-value-realization.md) invariant 2). The engine's own surface is
  unchanged; it still holds one callable per submitted effect.
- A closure whose body suspends is untouched by this. Building one starts a frame the scheduler
  owns, which is an activation rather than a value, and it is refused on this backend by name.

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
  already crosses as data.

- **Captures as the invoke's leading parameters.** This is what the lowering did while no closure
  escaped, and it was sound only for a body invoked where it was built -- which is a body that
  should never have been a callable value. Such a body is a block expression, so the shape it needed
  no longer exists, and one invoke signature covers every closure that is left.

- **An erased callable type introduced at the semantic layer.**
  [closure-environment-and-activation-frame](closure-environment-and-activation-frame.md) holds one
  in reserve for a heterogeneous collection of closures of one signature, to be introduced with an
  explicit erasure operation. It stays in reserve, and the reason is where the heterogeneity lives:
  no operation of the IR holds two closures of different bodies, because SystemVerilog has no
  first-class subprogram value for one to come from -- every callable value a lowering synthesizes
  is built at a site that knows its body and consumed by one that was handed it. The heterogeneous
  collection is the region queue, which is inside the runtime, below the IR entirely. An erasure
  operation would therefore have no consumer to serve. What that entry forbids -- _implicit_ erasure
  of a concrete callable value at a submit site -- does not arise, because nothing at that site is
  erased.

- **Realizing a closure as a scope class.** The member storage would be reached by the entry an
  instance already has, at the price of giving a closure a base, a constructor, a dispatch table,
  and a place in the object tree -- the machinery `architecture/object_model.md` keeps the two
  categories apart to deny it. Sharing the storage vocabulary is the part that pays; sharing the
  category is the part that does not.

## Cross-references

- `../architecture/compiler_generated_storage.md` -- the semantic shape this realizes: capture
  fields plus one invoke, the receiver rule, and the capture form carried by the field's type.
- `../architecture/callable.md` -- callable code versus callable value, and the runtime shapes a
  backend hands closures to as that backend's realization.
- `../architecture/scheduling.md` -- a deferred effect is a closure submit rather than a suspension,
  which is what makes the region queue heterogeneous.
- [member-slot-storage](member-slot-storage.md) -- the schema-and-instance model this reaches for,
  and the member place a capture read becomes.
- [jit-value-realization](jit-value-realization.md) -- the opaque-handle baseline, the per-stretch
  transient store, and the consumption of a handle that must outlive its call.
- [activation-frame-and-transient-scope](activation-frame-and-transient-scope.md) -- the escape
  invariant D4 satisfies, which names a closure capture and a deferred effect outright.
- [jit-process-suspension](jit-process-suspension.md) -- the coroutine closure this entry does not
  cover, and why it is a separate foundation.
- [block-expression](block-expression.md) -- the node a body invoked where it is built became, which
  is what leaves every remaining closure an escaping one and its invoke signature uniform.
