# Runtime effects as generic calls

Date: 2026-06-17 Status: accepted

## Context

Runtime effects -- `$display` / `$write` / `$strobe`, `$finish`, file IO (`$fopen` / `$fread` /
...), `$fscanf`, `$sformat`, `$time` / `$timeformat`, NBA and postponed submission -- need a MIR
representation. Today MIR carries them as a dedicated `RuntimeCallExpr` node wrapping one of ~22
specialized payload structs (`RuntimePrintCall{kind, descriptor, items}`,
`RuntimeFinishCall{level}`, ...), and the C++ backend injects the engine handle as a spliced
`self->Services()` string at each call site.

Two problems, both against `mir.md`:

1. **The backend re-decides facts MIR did not state** (`mir.md` invariant 10). It splices
   `self->Services()` -- a parameter that exists in the runtime SDK signature but nowhere in the MIR
   call -- and it reads the payload struct's shape to lay out the call. A second backend (LIR ->
   LLVM) would have to re-derive both. A backend reads a stated fact; it never re-decides one.
2. **A specialized payload is an opaque call to the LLVM path** (`mir.md` Forbidden Shapes / Notes).
   `kind` / `items` carried as an ad-hoc struct become, under LLVM, an opaque call through which
   constant folding, dead-arm elimination, and jump-table synthesis cannot reach.

The tangle this caused (recorded so it is not repeated): trying to "fix" services by giving it a
special field on the runtime-call node, or a `ServicesType` used specially, or making runtime ops
methods on `Scope` (a god object), all chase the wrong axis. The axis is: MIR's call must be
generic.

## Decision

A runtime effect is an ordinary `CallExpr` -- a callee symbol plus a `vector<Expr>` of arguments,
nothing else. Every operand is a generic expression; MIR does not know the role of any argument.

- The engine handle is just one argument: a `self.Services()` expression (itself a `CallExpr`,
  result type `ServicesType`). MIR does not know it is "the services parameter". That the runtime
  free function takes the engine first is the SDK signature's business, not MIR's.
- `kind` is a literal expression; `descriptor` already is an expression; each print `item` is a
  value-build expression; `finish` `level` is a literal expression. The per-effect payload structs
  flatten into the argument list.
- The backend renders one generic shape: callee name, open paren, each argument rendered as an
  expression, comma-separated, close paren. No services injection, no payload parsing, no per-effect
  node. The C++ backend and the LLVM-via-LIR backend consume identical input.

`ServicesType` and the `self.Services()` call are NOT layer violations: a generic value type and a
generic call, exactly like `PackedArray` and `q.Size()`. The violation was only ever treating
services _specially_ -- a node field, a special-cased injection -- instead of as a plain element of
the argument vector.

## Rejected

- **Specialized payload node (`RuntimeCallExpr` + per-effect struct).** The current shape. Opaque to
  the LLVM optimizer; forces every backend to re-implement payload layout. This is what retires.
- **Backend injects `self->Services()`.** The backend re-decides a fact (the engine argument) absent
  from the MIR call; the LLVM backend would then receive a call with one fewer argument than the SDK
  function takes. Violates `mir.md` invariant 10.
- **`services` as a special field on the runtime-call node.** Non-generic: it special-cases one
  argument instead of leaving it a plain element of the argument vector. This was the original layer
  violation.
- **Runtime ops as methods on `Scope` (or the engine).** `self->Print(...)`, `self->Finish(...)`.
  Pushes dozens of effects onto one class (god object) for the cosmetic win of method-call syntax,
  and binds the runtime SDK to `Scope`. A free function keeps the minimal dependency -- it needs
  only the engine, not the whole scope. (Calling `LyraPrint(self, ...)` instead of
  `LyraPrint(services, ...)` is the same idea in disguise: the op stays a free function either way;
  only the first argument differs, and `self.Services()` as a generic arg is the cleaner pick.)

## Consequences

- One generic `CallExpr` is MIR's only call shape; `RuntimeCallExpr` and its payload structs do not
  exist. Runtime free functions (`LyraPrint(RuntimeServices&, ...)`, etc.) stay unchanged.
- Both backends eat the same generic input; the LLVM optimizer reaches through.
- Each runtime effect lowers to a generic `CallExpr` whose first argument is `self.Services()` and
  renders through one generic path with no injection. An entry that answers through a destination
  the call names completes with a product of the values it settled, and the call site stores each
  where the source named it, so the call carries no live-reference or mutate-routing concept. An
  effect whose SV form omits an optional argument materializes the default at lowering, so the
  argument vector is always complete; where the forms differ by more than a default, each is its own
  entry.
- An item-bearing effect (the `$display`, diagnostic, and `$sformat` families) flattens its items
  into a constructed `PrintItem` array -- each item a value-build `ConstructExpr` over a
  runtime-library value type (`PrintItem` / `FormatSpec`) -- and encodes any discrete mode (a
  print's newline discipline, a diagnostic's severity) in the selected runtime entry rather than as
  an argument. An effect that yields a value (`$sformat`) produces it as the call's rvalue for the
  surrounding statement to place.

## What the copy-out temp got wrong

This entry first said that an entry taking a write-through destination "receives a copy-out temp as
an ordinary argument". The reason it gave is right and still holds: the call must carry no
live-reference or mutate-routing concept. The mechanism was not. The callable model settled the
other one -- an `output` is a component of the result's output pack, an `inout` is a value parameter
plus one, and the call site writes each component to the actual it named -- and the subroutine
family moved to it while the runtime effects were left behind. Two staters of "how does a call
answer" then drifted, for as long as nobody wrote a service that needed both.

What made the drift visible is that a temp lent to a callee cannot be written at all where every
value crosses as a handle the generated side may not mutate: the same source ran on one backend and
handed back an untouched destination on the other. Stating the answer as the completion removes the
lending entirely, which is a stronger form of the property this entry wanted rather than a departure
from it.

"Selects a shorter runtime overload" is superseded the same way. An overload set is told apart by
arity, and a boundary whose signature comes from the call cannot tell two arities of one symbol
apart, so a form that differs by more than a materializable default names its own entry.

## Cross-references

- `architecture/mir.md` invariant 10 (multi-backend; a backend never re-derives a stated fact) and
  Forbidden Shapes / Notes (a specialized / opaque payload becomes an opaque call the LLVM optimizer
  cannot reach).
- `decisions/format-dispatch.md` (the runtime-side `Formatter<T>` mechanism each flattened print
  item still uses at execution time).
- `decisions/callable-receiver.md` (`self` binding; `self.Services()` reaches the engine through
  it).
- `decisions/unified-callable-model.md` (the output pack and the completion payload -- how a call
  answers through a destination it names, which supersedes the copy-out temp above).
- `decisions/runtime-entry-naming.md` (a signature taken from the call, which is why an overload set
  told apart by arity cannot survive at this boundary).
