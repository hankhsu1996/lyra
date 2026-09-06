# The Object a Call Dispatches On

## Date

2026-09-05

## Status

Accepted. Supersedes the receiver convention `builtin-call-identity.md` fixed -- "instance calls,
where the receiver is `args[0]`" -- and generalizes
`calling-a-subroutine-on-another-units-object.md` D6 from one identity space to every call. It
reverses neither decision's subject: the callee identity stays one flat `support::BuiltinFn`, the
callee stays one `Direct` shape with no instance / static / free arms, and the C++ backend keeps
method-call syntax.

## Why this decision matters

Whether a call binds a receiver is one bit, and until now MIR did not carry it. The receiver was a
receiver because it was first in the argument list, so every consumer had to work out for itself
whether a first argument was one -- and each worked it out from whatever was nearest to hand.

Inside the C++ backend alone: a user method asked the callee's declared signature whether the code
takes a `self` formal; a built-in asked whether the call carried a scope qualification, and then
whether a namespace string came back empty; the cross-unit forms answered by construction, one
target each way. Half of render then had to tell the other half how many operands the callee text
had already swallowed, which is what `leading_arg_count` was. Below MIR the same question was
answered a fifth time, independently, by hardcoding `call.arguments[0]` wherever a receiver was
needed.

That is the signature `backend_contract.md` invariant 6 predicts for a fact MIR fails to state:
whatever a render works out, the other consumer works out again, and nothing holds the answers in
step. It is not hypothetical here. One of the derivations was a guess at whether the leading
argument happened to be pointer-typed, and it answered wrongly for a type-associated method whose
first parameter is a handle -- the argument was spliced into the callee text and vanished from the
call. `calling-a-subroutine-on-another-units-object.md` D6 fixed that one by splitting the target so
the node says which it is. This decision is that fix stated once for every call instead of one
identity space at a time.

MIR already had the right shape one alternative over. `Virtual` carries its receiver as a field of
the node, with the reason written beside it: the receiver rides the callee so the argument list
holds exactly what the source wrote. `Direct` stated the opposite convention in its own comment.

## Decisions

### D1. A receiver is a field of the callee, not a position in the argument list

`Direct` carries the object the call dispatches on, absent for a call that dispatches on nothing --
a type-associated method (LRM 8.10), a package subroutine (LRM 26.3), a runtime entry reached by
name. `CallExpr::arguments` then holds exactly the operands the source wrote, and the question "does
this call bind a receiver" is answered by reading one field.

The absence is the legitimate kind: empty means there is genuinely no object to dispatch on, not a
second meaning smuggled into a missing value. The producer knows which it is, because the producer
is what built the call.

### D2. Binding a receiver is a property of the call, not another alternative of the target

The target says where the code is found -- by name in this unit's arena, by name across a unit
boundary, in the DPI-C name space, in the runtime library. What the code is applied to is a separate
question, and the two vary independently: a callable target may or may not take a receiver, a
built-in may or may not, a cross-unit method does and a cross-unit static does not.

So the receiver does not split the target variant along a second axis. Where two targets already
differ for identity reasons and also differ in receiver-ness -- the cross-unit instance and static
methods -- that split stays, because it was made for how the name resolves, not for this.

### D3. One reading, consumed exhaustively

`CalleeReceiver` answers the question for every callee alternative through one visit, so gaining an
alternative breaks the build until it says what it means. Every consumer -- the C++ render, the
MIR-to-LIR lowering, the assignment-target walk, the write-path root walk -- reads it and derives
nothing.

### D4. Each backend spells the receiver its own way, and that is spelling

A receiver is stated once; how it reaches the callee in a target language is that target's business.
The C++ backend binds it into the callee text for a method (`(recv).Name(args)`) and passes it as
the leading argument where the runtime library publishes a free function
(`lyra::value::Require(subject, ...)`, `lyra::runtime::symbol(handle, ...)`), because a C++ free
function has no receiver to bind. The execution backend puts it first in the operand list, which is
where the runtime ABI takes it.

Both arms of that choice emit a call to the same entry with the same operands in the same order, so
no reader could tell them apart by running the program: it decides punctuation, not an operation.
What it replaces is a count passed between two halves of one render.

## Rejected alternatives

- **Keep the positional convention and verify it.** A pass could check that a call to an entry
  declared to take a receiver has a non-empty argument list. It cannot check that the first argument
  _is_ the receiver, which is the part that was wrong, and it would need the receiver-ness of every
  entry written down anyway -- at which point stating it on the node is strictly less machinery.

- **Put the receiver on `CallExpr` beside the arguments.** Symmetrical-looking, and it loses the one
  guarantee the current shape has: a virtual call always dispatches on something, and a field on the
  call node can only express that as an optional every consumer must then handle. The receiver
  belongs to the thing that needs it.

- **Split every target into a receiver-binding form and a receiver-less one.** This is D2's
  alternative taken as a rule rather than as an identity-driven exception. It doubles the target
  variant along an axis that has nothing to do with how a name is resolved, and it puts the same
  fact in two places for every target that can go either way.

- **Give the receiver a value category as well as a position.** The receiver of a mutating method
  names storage and the receiver of a query names a value, and that could ride the callee too.
  Rejected as a separate subject: value category is a property of an expression's position in its
  enclosing expression, not of the call, and where it is currently derived the root is the
  unpacked-struct lowering rather than the call shape.

## Consequences

- `CallExpr::arguments` means one thing everywhere: the operands the source wrote, plus the engine
  handle a runtime entry takes as a parameter. Nothing in it is a receiver.
- No consumer counts. The C++ render's argument loop walks `arguments` from the start, and the
  place-ness of an operand is asked at one site instead of at two that a third site's fallback arm
  kept apart.
- A builtin's declared namespace stops doubling as a call-form discriminator. An empty namespace now
  means global scope and nothing else, so a builtin nobody listed can no longer be re-classified
  into the instance form and lose an argument.
- One boundary this deliberately leaves where it was: the engine handle a namespaced runtime entry
  takes stays an ordinary operand rather than becoming a receiver. Whether the runtime library's
  split between a method on the engine and a free function taking it is a language fact or a library
  shape is a question about that library, and answering it here would have changed what a call means
  in order to tidy how one target spells it.

## Cross-references

- `decisions/builtin-call-identity.md` -- the callee identity this keeps, and the positional
  receiver convention this replaces.
- `decisions/calling-a-subroutine-on-another-units-object.md` -- D6, the same argument made for one
  identity space, with the wrong-code path that motivated it.
- `decisions/callable-receiver.md` -- what a receiver is for a callable body; unchanged.
- `architecture/backend_contract.md` -- the mechanical-translation contract whose invariant 6 names
  the failure this removes.
