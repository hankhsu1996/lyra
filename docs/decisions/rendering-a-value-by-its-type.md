# How a value reads is program computation, not data handed to a formatter

Date: 2026-09-11 Status: accepted

## Why this decision matters

LRM 21.2.1.6 renders a value by facts only its declaration carries: a structure prints its members
under the names it declares for them, a union prints only its first declared member, a tagged union
prints the member its tag names, and an enumeration prints the name declared for the value --
including where an aggregate is traversed down to one. None of that is carried by a value, and a
formatter that has reached an element of a container has no way back to the type the element came
from.

[aggregate-names-are-type-content](aggregate-names-are-type-content.md) put the names below the
front end and named the step that remained: carrying them into the print operation. This is that
step, and the question it answers is not where the names live but **what kind of thing a rendering
is**.

## The fork

Two shapes, and the field is split between them.

- **Generate a rendering per type.** Verilator's `V3Common` synthesizes a `VL_TO_STRING` function
  for each structure whose body appends `"'{"`, then `"<name>:" + <recursive call>` per member, then
  `"}"`; Rust expands `#[derive(Debug)]` into a `fmt` method carrying the field names as literals.
  The names are code.
- **Describe the type as data and walk it.** Go's compiler emits a runtime type descriptor per type
  (`cmd/compile/internal/reflectdata`) into a read-only section, and one universal walker --
  `reflect`, which `fmt` uses for `%+v` -- reads value and descriptor together. Swift's reflection
  metadata is the same shape.

What decides between them is whether the walk can see the value's static type where the walk
happens. Verilator emits C++ holding the concrete type; Go has erased it into an `interface{}`.

**Our condition is that both are true at once, on the two backends.** The C++ backend monomorphizes
an aggregate, so the host compiler can walk it; the execution backend realizes every aggregate by
erasure ([jit-aggregate-realization](jit-aggregate-realization.md)), so nothing there can. A
mechanism built for either one alone is a MIR primitive specialized for one backend, which
`../architecture/backend_contract.md` forbids. The shape available to both is the one that needs
neither: state the rendering where the SystemVerilog type is still in hand, as ordinary program
computation, before any realization exists.

**And the data shape is forbidden here for an independent reason.**
[enum-representation](enum-representation.md) bans "an opaque runtime-library helper as the semantic
implementation of an enum method (`EnumName(value, table)`)". Printing the name of an enumeration
reached inside a container is exactly that lookup, so a descriptor carrying the member table would
be the banned shape arriving by another route.

## Decision

**A type that decides how a value of it reads answers with a callable the compiler synthesizes for
it, once per type, whose body is generic MIR; the print operation carries the text that callable
returns.**

1. **The classification is one question with one answer.** A type either decides nothing the value
   does not (an integral, a real, a string, a handle), decides a text _beside_ the value (an
   enumeration, which still reads as its base integral under a radix conversion), or decides the
   only text the clause defines for it (an aggregate, which has no other reading). Every consumer
   reads that classification rather than re-deriving which case it is in.

2. **The rendering is synthesized from the SystemVerilog type, not the MIR type.** The clause is
   stated over source types, and it distinguishes what a lowering is entitled to stop carrying: a
   packed tagged union prints `tag:value` where a packed untagged union prints its first member,
   while both project onto one vector below the front end. Keying the rendering there is what lets
   MIR keep one packed union type, which
   [aggregate-names-are-type-content](aggregate-names-are-type-content.md) point 4 otherwise
   required a second type for.

3. **Each rendering is synthesized on first use and shared by every site in the unit**, homed on the
   class the print site lowers into, exactly as the LRM 6.19.5 `name` callable already is. Two
   values of one type therefore cannot render differently, and no site re-derives a fact belonging
   to the type.

4. **A container's rendering is a loop, not composed text.** That is what reaches an element count
   known only at run time, and it is the whole difference from the alternative
   [aggregate-names-are-type-content](aggregate-names-are-type-content.md) rejected -- "the names as
   an operand of each print site, composed at the lowering" -- which is per-site text where this is
   per-type computation.

5. **The value layer renders leaves and nothing else.** `Formatter<T>` stays for the integral, real,
   string and handle types, whose text genuinely follows from the value and the conversion. The
   aggregate specializations are deleted: with the rendering above them, an aggregate value never
   reaches a formatter.

6. **A format string the program computes carries, per operand, the readings its type has.** Such a
   string reaches no directive until it is parsed, so neither side knows whether a `%p` is coming.
   An enumeration carries both -- its integral, which a radix conversion prints, and its declared
   name. An aggregate carries only the text, because the clause defines no other conversion for it,
   and a directive asking for one is refused by name at the moment it asks. That is the shape a
   handle already has, whose one defined conversion is the assignment pattern too, and reading the
   two axes as one rule is what keeps an aggregate from answering a `%d` with its pattern text.

## Rejected alternatives

- **A per-type description as data, walked by the runtime formatter.** The shape Go and Swift take.
  It needs a compiler-to-runtime data contract that does not exist here, an erased-side formatter
  family that does not exist either, and it puts an enumeration's member table into the library,
  which [enum-representation](enum-representation.md) forbids by name.

- **A closure per element, handed to the runtime's container walk.** Keeps the container walk where
  it is, but a product's elements have different types, so one erased element renderer cannot serve
  them and the closure set becomes the description above with callables in it.

- **Rendering only where a name is involved, leaving leaf-element containers to the runtime.** Two
  mechanisms for one clause, and it does not answer the execution backend at all: `%p` of
  `int arr[3]` refuses there for the same reason `%p` of a structure does.

- **Binding the text to a local of a block expression the operand then escapes.** A block expression
  is an immediately-invoked lambda on the C++ backend, so a name bound in one dies at its return and
  a borrow taken out of it dangles. What the steps have to contain is the call that borrows, not the
  operand alone -- so naming each operand and formatting through those names are the steps of one
  block expression, which is also what keeps an operand the arg mentions twice from being evaluated
  twice.

- **Giving the aggregate's text to the value slot as well, so every operand has one.** It makes the
  slot mean two things -- how this operand reads by conversion, and the one text it has -- and the
  cost is a wrong answer rather than a shape: a `%d` through a computed format string then prints
  the pattern text as though it were a number. The slot is genuinely absent for an aggregate, and
  saying so is what lets the conversion be refused.

## Consequences

- The runtime publishes no entry for formatting an aggregate, and needs none: the three the
  execution backend was missing were never the answer. What it gained instead is two operations --
  an operand carrying the text its type renders beside its value, and an operand that reads only as
  that text -- each named as its own entry rather than reached by reading an argument list.
- A type gaining a rendering is a lowering arm rather than a `Formatter` specialization, which is
  where the type knowledge already is. [format-dispatch](format-dispatch.md)'s open-extension
  consequence is superseded on that point; its output decisions -- decimal integral leaves, index
  labels on an associative array and not on an indexed container, `%p` and `%0p` alike -- all stand.
- The rendering reads the member it has already settled on, so it takes the run a packed member
  occupies rather than the member access a source expression would write, whose LRM 11.9 tag check
  would be this operation asking again what it just decided.
- A container renders by joining strings, which is quadratic in the text it produces. That is the
  accepted cost of the shape; an append-in-place entry is the answer if a design ever prints a
  container large enough to feel it.

## Cross-references

- [aggregate-names-are-type-content](aggregate-names-are-type-content.md) -- the names below the
  front end, which this carries into the print operation.
- [enum-representation](enum-representation.md) -- type-owned semantics realized as generic program
  computation, the model this applies to a second type family.
- [jit-aggregate-realization](jit-aggregate-realization.md) -- the erasure that makes the data shape
  unavailable, and its own prediction that a whole-container operation stated over its elements owes
  the erased half a mechanism.
- [format-dispatch](format-dispatch.md) -- the leaf formatters, which stay, and the aggregate
  decisions this supersedes.
- LRM anchors: 21.2.1.6 (assignment pattern format), 21.3.3 (a format string known at simulation
  time), 6.19.5 (`name`), 7.3.2 / 11.9 (a tagged union's tag).
