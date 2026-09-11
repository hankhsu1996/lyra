# A cast states two types and nothing else

Date: 2026-09-10 Status: accepted

## Why this decision matters

`mir.md` listed among its Forbidden Shapes:

> One cast node standing for every reinterpretation, whose realization each backend selects from the
> (source, destination) type pair. A cast node names exactly one operation [...] so a conversion no
> node covers fails to compile. Under a type-pair-dispatched node a pair a backend never handled is
> instead a silent no-op, indistinguishable from a deliberate reinterpretation.

MIR carried five nodes under that rule -- reduce a value to a machine boolean, re-type a reference,
name a code address as another function type, resize a machine integer, rename a packed value's type
-- and LIR carried four instructions beneath them. This entry reverses that shape. The hazard the
rule named is real and is answered a different way.

## What the five nodes turned out to be

They were not five operations. Two of the five were byte-identical structures holding one operand;
the MIR-to-LIR lowering gave the code-address one and the reference one **the same LIR
instruction**, so those two had already collapsed one layer down while keeping separate names above
it. In the C++ backend three of the five rendered the same `static_cast<T>(x)`, one rendered
`bool(x)` -- which is that cast spelled shorter -- and one rendered nothing at all.

No consumer read the choice to learn something the two types did not already say. That is the test:
a node's data earns its place by answering a question its neighbours cannot. The kind answered
"which conversion is this", and the pair `(operand type, Expr::type)` answers it, because the five
were pairwise disjoint on that pair by construction. A kind beside them was a second copy of a fact
the node already carried.

## Where a cast kind is real, and why not here

The field splits cleanly, and the split is about what the front end has already decided.

**LLVM derives the operation from the pair.**
`CastInst::getCastOpcode(Value *, bool SrcIsSigned, Type *, bool DstIsSigned)` returns the opcode
that must be performed to cast one to the other, and `IRBuilder::CreateIntCast(V, DestTy, isSigned)`
picks truncation, sign extension, or zero extension from the widths and one signedness bit. LLVM IR
has distinct opcodes, but nothing upstream of it chooses them: the pair does.

**Clang and Rust MIR carry a kind, and both carry it for a reason MIR does not have.** Clang's
`CastExpr::getCastKind` distinguishes `CK_DerivedToBase`, `CK_UserDefinedConversion`,
`CK_LValueToRValue` -- the front end's resolution of overloaded C++ conversion syntax, including
which conversion function was selected and which base path was taken, none of it recoverable from
the two types. Rust's `Rvalue::Cast` records a `CastKind` for the same reason: `as` is overloaded,
and a pointer coercion carries an unsizing choice the types alone do not fix.

Lyra already holds that resolution one layer up. HIR carries SV's own conversion vocabulary, and
HIR-to-MIR is where an overloaded source conversion is resolved into a concrete pair. By the time
MIR is reached the ambiguity is gone, which puts MIR in LLVM's position and not clang's.

## Decision

**A cast is one node carrying one operand. The operand's type is what the value comes from,
`Expr::type` is what it goes to, and that pair is the whole statement.**

1. **No kind axis.** Nothing beside the two types says which conversion this is. A consumer that
   needs to know reads the pair, which is type dispatch and belongs with every other question a
   target answers about a type.

2. **A conversion that reshapes a value is a call, not a cast.** Integral resize, real to and from
   integral, packed to and from string: each is a library entry and is unchanged by this entry. What
   reaches a cast moves no simulation value between representations.

3. **A backend refuses a pair it does not realize.** This is what answers the hazard the Forbidden
   Shape named. A backend states a no-op only where the two representations are provably the same --
   in the execution backend, where the two types map to one machine type -- and every pair it cannot
   realize returns `diag::Unsupported`. Totality moves from the compiler's exhaustiveness check to
   the backend's own refusal; it does not disappear, and a pair nobody implemented is a diagnostic
   rather than a silently unconverted value.

4. **LIR verification keeps the invariant that the kinds were standing in for.** Between two packed
   values a cast only renames what the program holds the bits to be, so both sides must structure
   their bits alike; where they do not, the reshape meant to precede the cast is missing and the
   value would silently change width. That check reads the two types, which is where it always
   belonged.

## Rejected alternatives

- **Keeping the five nodes.** They record a classification of the type pair, and a classification
  stored beside the thing it is derived from is a second copy that can disagree with it. The
  evidence that it already had: the code-address node and the reference node lowered to one LIR
  instruction, so the two names had stopped meaning different things while both were still spelled.

- **One node with a kind enumeration on it.** The same defect in fewer nodes. It reads as a
  compromise and is not one: the enumeration is still derived from the pair, and adding an
  alternative to it is adding a name for a pair rather than for an operation.

- **A per-backend table of legal pairs, checked centrally.** The classification each backend needs
  is the machine operation, not a yes-or-no answer, so a central table would be consulted and then
  re-derived. Where the two backends must agree -- a cast that renames a packed value must not
  change its width -- the rule is stated once in LIR verification, over the types.

## The objection this has to survive

`mir.md` also forbids "a primitive that stands for more than one operation, leaving a consumer to
pick which by reading its result type", and a reader will raise it against this entry. It does not
reach: that ban is about a type leaving the choice **open**, where several operations answer to one
result type and the node names none of them, so each backend decides what the program means. A pair
of types admits exactly one conversion, so a consumer reading the pair is translating rather than
deciding and the two backends cannot come out differently. `mir.md` now carries that distinction
beside the ban, because it had been re-derived twice -- here, and where the composite literal
collapsed to one node.

## Consequences

- The C++ backend renders a cast as the C++ cast notation over the rendered destination type, one
  composed expression with no branch and no type named in the renderer. That notation is C++'s own
  "convert this to that", which is the same statement the node makes.
- That notation is the one construct in that backend which is not a primary expression, so the
  render encloses it in parentheses. Without them a `->` or a `[` written after a cast takes the
  cast's own operand instead, and the conversion silently applies to the wrong thing -- it compiles
  wherever the mis-parse happens to name a real member, which is what makes it worth stating here
  rather than only at the render.
- The execution backend answers the pair in one entry: identical machine types convert to nothing, a
  machine boolean is what the value's own domain answers about it, and two machine integers resize.
- Adding a conversion between two types no backend handles is no longer a build break. It is a
  refused compilation with a diagnostic, which is the behaviour every other unimplemented operation
  already has.

## Cross-references

- `../architecture/mir.md` -- the Forbidden Shape this entry replaces, and what is forbidden in its
  place.
- `../architecture/backend_contract.md` -- the test separating a spelling a backend chooses from an
  operation only MIR may state.
- [exhaustive-alternative-consumption](exhaustive-alternative-consumption.md) -- why a closed set is
  consumed by a visit, and what it costs when a set's alternatives are not really distinct.
- LRM anchors: 6.24 (casting), 11.3.1 (operands of logical operators), 12.4 (the condition of a
  conditional statement).
