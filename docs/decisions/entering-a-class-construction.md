# Allocation is the runtime's, construction is the asking code's

Date: 2026-09-09 Status: accepted

## Context

A class object comes into existence on the managed heap, which the runtime owns, so a runtime entry
is what allocates it. That entry also ran the constructor: the class's runtime definition carried
the constructor as a registered native entry, and the allocation called it on the object it had just
built.

That shape is the one a scope's definition has, and there it is right. A scope participates in the
elaboration lifecycle, so the runtime must be able to say "build this instance" without any
generated code asking; the entry exists because the call crosses from the runtime into generated
code. A class participates in no lifecycle -- which is one of the three axes
[object-model](object-model.md) separates -- and the only thing that ever asks for a class object is
a `new` expression, which is generated code already running.

Borrowing the shape imported a runtime-to-generated crossing where the actual crossing runs the
other way, and the cost was visible: a registered entry has one signature, so the definition could
only hold `void (*)(void*)`. A constructor declaring formals had nowhere to receive them, which the
execution backend refused at the construct site, and the linkage bound whatever symbol it found to
that signature with nothing checking that its shape matched.

Established runtimes do not put the constructor on the type's runtime descriptor. The JVM allocates
with `new` and initializes with a separate `invokespecial`; the CLR's `newobj` names the constructor
at the call site as a method token; Objective-C sends `alloc` then `init`; a C++ frontend emits an
allocation and then a constructor call. The reason is the same everywhere: a constructor is selected
by the static type written at the `new` site, is not inherited and is not overridden, so it is not
dispatched -- and a descriptor slot is exactly a dispatched, fixed-arity thing.

## Decision

**A class's runtime definition says what storage its properties need, and nothing about behavior.
Bringing an object into existence and initializing it are two operations, and only the first is the
runtime's.**

### D1. The definition carries storage, not a body

What the runtime holds for a class is the schema its properties need. The allocation entry answers
an object whose properties hold their storage's default; no body has run when it returns.

### D2. The construction is entered by the code that asked for it

A `new` expression becomes an allocation, an opening of the handle it answered, and a call to the
class's constructor with the object leading its arguments. The constructor is an ordinary function
of the program, entered the way every other body is -- which is already how a derived constructor
enters its base's, so one convention serves both and a constructor has one calling convention rather
than two.

The arguments cross as themselves. Nothing about them is erased, because nothing between the caller
and the callee has a signature of its own to erase them through.

### D3. The split is a realization, below MIR

MIR keeps one construction naming the type it builds, with the operands the source wrote
([value-construction-forms](value-construction-forms.md)). Which target language needs two steps and
which needs one is that target's answer: the C++ backend spells a construction with the host
language's own constructor and never sees the split, while MIR-to-LIR states both operations so the
execution backend translates each mechanically and decides nothing.

## Rejected alternatives

- **Widen the registered entry to take a span of arguments**, the way a closure's captures cross. It
  gives one constructor body two calling conventions -- a span-unpacking one for a `new`, a direct
  one for a base forward -- and the body would have to carry both. It also erases every argument
  across a boundary that has no reason to be there.

- **Keep one runtime call and name the constructor per class in the definition, with a signature per
  arity.** The same erasure, plus an arity axis on the definition, to describe a body the calling
  code can already name.

- **Split in the backend rather than in MIR-to-LIR.** A backend would compose an allocation, a
  dereference, and a call out of one IR node, which is a call sequence the IR does not state.

- **Let the object answer with the handle referring to it, so the runtime keeps holding one across
  the construction.** That record is what a shared-owner realization needs and a traced one does
  not, so adding it now buys a property the reclamation model is meant to make unnecessary.

## Consequences

- A class's runtime definition holds one thing. The registered-entry type, the definition's body
  field, the runtime call that invoked it, and the linkage step that bound a symbol of any signature
  to it are all gone, and with them the construct-site refusal that stood in for the signature the
  boundary could not express.
- A constructor runs in the generated-call scope of whatever called it, like every other generated
  call. The scope the runtime opened around it is gone, so a temporary the constructor builds now
  lives as long as any other temporary of the calling body.
- Reaching a class another compilation unit declares is one question again rather than two: its
  constructor is named by the cross-unit method symbol every other method of it is named by, so what
  a cross-unit `new` still waits on is the definition crossing, not a body.
- Nothing changes for a scope or a unit. The lifecycle entries stay what they are, because there the
  runtime really is the caller.

## Cross-references

- [generated-behavior-boundary](generated-behavior-boundary.md) -- the runtime-to-generated entry
  contract, and the separation of allocation, construction, and dispatch this applies to a class.
- [object-model](object-model.md) -- the three independent axes, one of which is lifecycle
  participation.
- [value-construction-forms](value-construction-forms.md) -- the construction MIR states, which this
  leaves unchanged.
- [callable-receiver](callable-receiver.md) -- the explicit-`self` body shape the constructor is
  entered through.
- [managed-value-realization](managed-value-realization.md) -- where a managed value may live, and
  the staged reclamation the rejected handle record belongs to.
