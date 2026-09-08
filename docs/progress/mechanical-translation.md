# Mechanical Translation

The render refactoring: MIR states each semantic fact once, and every consumer -- the C++ render,
the MIR-to-LIR lowering, the dump -- reads it rather than working it out. Done when no backend entry
decides anything the node did not state, and no closed set holds an alternative no node carries.

The contracts this answers are `../architecture/backend_contract.md` (a backend entry is a fixed
function of one MIR node and chooses a spelling rather than an operation) and
`../architecture/mir.md` (every semantic decision is explicit in MIR's structure, and a backend
never re-derives one). Those two own how an item here is judged -- the test, and the failure the
cross-check predicts. This file owns only which instances are known and what is left.

## Facts now stated on the node

- [x] T1 -- A call states the object it dispatches on, so no consumer works out which of its
      operands is a receiver, and the argument list holds exactly what the source wrote.
- [x] T2 -- A reference names a declared thing through one node whose target says which table
      resolves the name, so what a name reaches is read rather than inferred from a node kind.
- [x] T3 -- A callee's identity states where its code is declared and nothing else; whether the call
      dispatches on an object is the callee's own receiver, stated once.
- [x] T4 -- The unary and binary operator sets hold only the operators a node carries. An operator a
      library performs, and one that names no operation at all, is settled before a node is built.

## A runtime entry's declaration

- [ ] T5 -- Every property of a runtime entry is read off one declaration of it: the name each
      backend spells it with, whether the target reaches it as a free function or through the object
      it acts on, whether it updates that object, whether it hands its argument back, what shape it
      takes as a trailing prototype, and the rest. Today each property is a separate table with its
      own default arm, so adding an entry means editing every one of them and nothing says which.
      One consequence is already a divergence: two entries share one target-language name in the C++
      backend and let its overload resolution re-decide which operation runs, while the execution
      backend emits a distinct symbol for each.

## What MIR can ask a backend to perform

- [ ] T6 -- One namespace names every runtime operation. A second one exists beside it holding
      operations the first does not, so MIR cannot state the realization it asks a backend to
      perform, and reaching one of them needs a node kind rather than a call.
- [ ] T7 -- A designated part of a value is named the same way at every layer. Today an access
      lowered from a call becomes a selector and is turned back into a call to the entry the call
      named, so two layers of vocabulary exist only to be undone. Blocked on T6.

## An aggregate's members

- [ ] T8 -- An unpacked struct keeps its field names through lowering, so a member access names a
      field rather than a position in a structural product. Two render decisions fall out with it:
      choosing an access form by testing the receiver's type, and finding a field's name by walking
      that type through the kinds that can bear one.
- [ ] T9 -- A field's identity splits exactly where the layer below it splits, and no consumer reads
      how the name resolves. Same shape as T3, one node over.
- [ ] T10 -- The aggregate-construction nodes that differ in no field are one node; which product is
      built is already the expression's type.

## Callable and assignment identity

- [ ] T11 -- Callable identity is one space whose entries name a declaration carrying signature,
      implementation form, receiver convention and per-backend spelling, so a call names one
      identity and nothing branches on origin. Implementation form is the half already costing
      something: with no field saying what a declaration is, four sites across the two backends read
      an absent body and each attaches its own meaning -- one calls a class callable with none a
      pure virtual and emits the marker that makes its class abstract, another concludes only that
      no code identity is needed. They agree today because the inputs make both right. **Gated on**
      the external callable form and a co-design with the foreign-symbol contract, which needs the
      same declaration shape.
- [ ] T12 -- A compound assignment states the operation it applies rather than the operator, so the
      lift from operator to library entry happens once where the assignment is built and the
      operator set is exactly what a node carries. Today three shift operators sit in that set
      solely because a compound assignment names them, and every consumer meeting one asks which
      kind it has. This reopens the shape
      [compound-assignment-write-location](../decisions/compound-assignment-write-location.md)
      settled -- one compound node whose "evaluate the target once" is each backend's mechanical job
      -- so it is a decision to revisit, not a defect to fix under it.

## Exhaustiveness

- [ ] T13 -- Every consumer of a closed set says what each alternative means, so gaining one breaks
      the build. A generic catch-all arm switches that off, and where the arm answers instead of
      refusing it answers a new alternative plausibly and wrongly. This is a different axis from the
      rest of this file -- extension safety rather than decision-making -- and it is why several of
      the others went unnoticed: a set that lost a member kept compiling.

## Small and mechanical

- [ ] T14 -- One visit over the expression set decides how an expression is rendered; value position
      and target position are the same walk asking one question, not two walks sharing most arms.
- [ ] T15 -- A backend meeting IR it has not implemented returns the recoverable failure the error
      policy prescribes rather than reporting a compiler bug.
- [ ] T16 -- No peephole in a render. Where one collapses a shape the producer built, the producer
      is what states the collapsed form.
- [ ] T17 -- A closed numeric set is closed in the type rather than left open with a refusing
      default.

## Cross-references

- `../architecture/backend_contract.md` -- the mechanical-translation contract and the cross-check
  that a second backend gives it.
- `../architecture/mir.md` -- what MIR's primitive set is closed under, and the forbidden shapes an
  item here is usually an instance of.
- `../decisions/call-receiver-on-the-callee.md` -- T1 and T3's rationale.
- `refactor.md` -- architectural debt outside this workstream.
