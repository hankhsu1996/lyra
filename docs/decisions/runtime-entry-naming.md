# A runtime entry is named by its operation and typed by its call

Date: 2026-08-31 Status: accepted

## Context

The execution backend reaches the runtime library through a C ABI: the generated module declares a
symbol and calls it, and the runtime defines that symbol. Two facts have to agree at that boundary
-- which entry is meant, and what crosses to it -- and both were being written down twice.

The backend held a class of one method per entry, each hand-writing that entry's symbol and its LLVM
signature: 95 methods and 96 signatures. A builtin reached one of them through a switch of 61 arms,
and a builtin not listed there fell to a `default:` that re-read the call as an operation on a value
and refused it naming the receiver's type.

Three things follow from that shape, and all three were observed.

**The refusal named the wrong thing.** `parent` -- a scope's enclosing scope -- was refused for
having a pointer receiver, when the fact was that nobody had added the arm. Twenty-eight builtins
sat in the value path that are not operations on values; ten of them were reachable, and every one
of those reported a receiver type as though the type were the obstacle.

**The signature was a restatement.** Every runtime value crosses this ABI as an opaque handle, so
all 96 hand-written signatures were runs of pointer, void, span, and machine scalars -- exactly what
mapping the call's own operand types yields. Writing them again bought no checking: a declaration is
held to the call's arguments by LLVM, never to the runtime's definition, so a hand-written signature
that disagreed with the definition was undefined behaviour at run time.

**The symbol had no author.** It was composed at about twenty sites with the value representation in
three different positions -- `lyra_rt_packed_add`, `lyra_rt_cell_packed_get`,
`lyra_rt_activation_frame_alloc_packed`. Five entries were defined in the runtime and reachable from
nothing, because the header did not declare them and the execution session did not bind them; a call
to one resolved to no address.

## Decision

**A runtime entry is named by the operation it performs, and typed by the call that reaches it.
Neither fact is written down a second time.**

- **The signature comes from the values crossing.** Callee resolution runs after the arguments are
  marshalled, so the function type is built from what is actually being passed -- including this
  target's own encoding of the call, such as a run of values crossing as a `{pointer, length}` span.
  An entry and its call cannot disagree about what crosses, because the call is where the types come
  from.

- **The symbol has one form.** `lyra_rt_<domain>_<operation>` where the library realizes the
  operation once per value representation, and `lyra_rt_<operation>` where it realizes it once.

  The operation's spelling comes from the name table its own closed set already publishes. Two such
  sets are the source language's and reach the backend already named -- a builtin and an operator;
  the third is the execution model's own, the operations the program never writes (reaching the
  storage a wrapper stands for, erasing a value, building one), which the backend enumerates itself.
  Minting takes the operation as itself rather than as text, so a symbol cannot be composed from a
  string: an operation is nameable only if some closed set already publishes its spelling, and one
  that gains a member fails to compile until it is spelled.

- **What the library does not realize is stated.** Naming is total over the builtin set: each
  builtin says whether its entry stands alone, is named by the representation of the value it acts
  on or of the cell it reaches through, crosses two representations, or that the library has no
  entry of its shape and which shape that is. There is no arm that re-reads a call as something
  else.

- **Two requests are two entries.** Where the runtime publishes an overload set, the caller names
  which member it means rather than leaving the arity to say so. Opening a file with and without a
  mode, flushing one channel and flushing all, running a command and asking after the command
  processor were the three that had not been split; the runtime already spelled each pair
  separately.

- **The three lists are held together by a check.** An entry exists as a prototype in the ABI
  header, a definition beside the runtime it wraps, and a binding that gives the session its
  address. The host compiler holds the definition to the prototype; a policy check holds the binding
  to both, and every prototype to a binding.

## Rejected

- **Adding the missing arms.** The ten reachable builtins could each have got a method and a switch
  case, leaving the shape intact. Rejected because the shape is what produced them: the next builtin
  added anywhere in the pipeline lands in the same `default:` and is refused for the same wrong
  reason. Whatever a fallback arm cannot name, some producer knew and did not write down.

- **Keeping the position of the value representation as each family had it, and checking the
  result.** A check finds a name nothing defines after it is written; one position means there is
  one site that writes names at all. The first is misuse detected, the second is misuse that cannot
  be spelled.

- **Deriving whether an entry is named by a representation from the call.** The test "does the first
  operand name a value representation" is what the old `default:` effectively applied, and it is
  wrong in both directions: a delay's second operand is a value and its entry is realized once,
  while a container construction has no operand of the representation its entry is named by.

- **Making the runtime's binding list a macro that stringifies the identifier.** This removes a typo
  between a bound name and the function it binds, but not the entry nobody remembered to bind, which
  is the failure that was actually observed. The check covers both and changes no code shape.

## Consequences

- The backend's ABI class is gone; what replaces it is the naming rule and a total table of what the
  library realizes per builtin.
- A builtin gaining an entry, or arriving without one, breaks the build until the table says which
  -- the same way the C++ backend's own name table already behaves, so the two backends are
  symmetric under a new builtin.
- An entry that is declared, defined, or bound without the other two fails a policy check rather
  than an unresolved symbol at run time.
- A refusal names the operation and the shape the library has no entry for, so the reason a case is
  refused is the reason it is refused.

**Where a construction still recovers its own form.** A queue is built empty or over an element
list, either way with or without its declared bound, and an associative array empty, over its
entries, or over its entries and a default. Which one a call means reaches the backend as how many
operands arrived, and a format specification is told from a bare conversion kind the same way. That
is the shape `value-construction-forms.md` rejects, and its own rule gives the answer twice over: a
form that differs by a _request_ is its own entry, which is what the three file and host-command
pairs above became, while a form that differs by an _optional argument with a default_ is one entry
whose caller materializes the default. A bound has an unbounded spelling and a specification has
field defaults, so those are the second kind and collapse rather than split. Both readings are the
construction subject's to apply, so this change leaves the three counting sites as it found them and
gathers them where they can be seen.

**What this does not reach, stated so nobody assumes otherwise.** A signature taken from the call
agrees about how many values cross and what shape each one has, and that is all it agrees about.
Where a value is a `{pointer, length}` span, what the pointer points _at_ is outside the signature
entirely: the per-axis indices of a hierarchy segment crossed as a run of opaque value handles and
were read back as a run of machine integers, and both sides typed the argument `{pointer, length}`,
so nothing here or in the host compiler could have noticed. Every index was the low half of a heap
address, which made `%m` print one and made a lookup by index match nothing. A span's element type
is a contract of its own, and the only thing holding it today is that one side writes it and the
other reads it in the same words.

## Cross-references

- `architecture/lir.md` (foreign symbols: LIR states the name and the machine types a call crosses
  on; how the name resolves is below LIR).
- `architecture/backend_contract.md` (a backend brings its own type-mapping and value-emission
  rules; decision logic in emission is a design failure upstream).
- `decisions/jit-value-realization.md` (the opaque-handle ABI, which is why every signature was a
  restatement).
- `decisions/builtin-call-identity.md` (the flat builtin identity these entries are named from, and
  where a classification of one belongs).
- `decisions/runtime-effects-as-generic-calls.md` (a runtime effect is an ordinary call, which is
  what lets one rule name every entry).
- `decisions/exhaustive-alternative-consumption.md` (a closed set is consumed by a switch; the
  builtin table was the case its own rejected-alternatives section left to review).
