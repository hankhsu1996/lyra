# Mechanical Translation

The render refactoring: MIR states each semantic fact once, and every consumer -- the C++ render,
the MIR-to-LIR lowering, the dump -- reads it rather than working it out. Done when no backend entry
decides anything the node did not state, and no closed set holds an alternative no node carries.

The contracts this answers are `../architecture/backend_contract.md` (a backend entry is a fixed
function of one MIR node and chooses a spelling rather than an operation) and
`../architecture/mir.md` (every semantic decision is explicit in MIR's structure, and a backend
never re-derives one). Those two own how an item here is judged -- the test, and the failure the
cross-check predicts. This file owns only which instances are known and what is left.

## Facts now stated once

- [x] T1 -- A call states the object it dispatches on, so no consumer works out which of its
      operands is a receiver, and the argument list holds exactly what the source wrote.
- [x] T2 -- A reference names a declared thing through one node whose target says which table
      resolves the name, so what a name reaches is read rather than inferred from a node kind.
- [x] T3 -- A callee's identity states where its code is declared and nothing else; whether the call
      dispatches on an object is the callee's own receiver, stated once.
- [x] T4 -- The unary and binary operator sets hold only the operators a node carries. An operator a
      library performs, and one that names no operation at all, is settled before a node is built.
- [x] T18 -- Reaching the one member an active-member value holds is its own operation, distinct
      from reaching a component of a product, so no consumer decides what a member reach means by
      testing what it reaches into. Whether a member that is not the live one answers with a default
      or with a run-time failure is the value's own semantics and travels with its type, so it needs
      no second node. The layer below carries the same split, as a selector kind of its own.
- [x] T10 -- A value that is its own parts is one node, and which value it composes is already the
      expression's type. Two nodes carried it -- a product literal and an element list -- with
      identical fields and emission entries that differed only in the field name they looped over,
      where the generic-language vocabulary has one construct whose meaning is the type it builds.
      Each of those types has exactly one way to be built, so the type answers completely and no
      consumer chooses; the layer that owns storage is where the two separate again. The
      active-member build was a second such pair, one node over, and is now one node too: whether
      the live member is observable and a mismatched reach fails travels with the type and changes
      nothing about the build.
- [x] T5 -- Every property of a runtime entry is read off one declaration of it: what the library
      calls it, whether a call site reaches it as a free function, as a method on the object it acts
      on, or as a factory on the type it builds, whether it updates that object or hands it back,
      and which of its operands carry an index, a spread part, a closure, or a result prototype. It
      had been a table per property, each with its own default arm for the entries it did not list,
      so adding an entry meant editing every one of them and nothing said which. Two divergences the
      scattering had hidden went with it: six entries that are answered where the source is read
      were refused by one backend and given a live entry by the other, and five pairs of entries
      shared one target-language name, leaving overload resolution over the argument list to stand
      in for an identity the pair already carried.

## What MIR can ask a backend to perform

- [ ] T6 -- One namespace names every runtime operation. A second one exists beside it holding
      operations the first does not, so MIR cannot state the realization it asks a backend to
      perform, and reaching one of them needs a node kind rather than a call.
- [ ] T7 -- A designated part of a value is named the same way at every layer. Today an access
      lowered from a call becomes a selector and is turned back into a call to the entry the call
      named, so two layers of vocabulary exist only to be undone. Blocked on T6.
- [ ] T19 -- A container comes into existence through its own constructor, with the element list
      among the arguments, and every container does so but one. A sequence is still built by a
      literal, so the layer below carries one instruction for that and for a machine aggregate
      alike, and the execution backend tells the two apart by testing the result type -- one arm
      naming the runtime entry that allocates, which is a render composing a call the IR never
      stated, while the other backend never re-derives anything because a sequence still has a node
      of its own. [value-construction-forms](../decisions/value-construction-forms.md) settled this
      and named this case as its rejected alternative; what stands in the way is that the other
      backend spells a sequence as a target-language type with no constructor over an element list,
      so closing it settles how a sequence is named when one is built.

## An aggregate's members

- [ ] T8 -- An unpacked struct keeps its field names through lowering, so a member access names a
      field rather than a position in a product. The render decision that falls out with it is
      finding a field's name by walking the receiver's type through the kinds that can bear one.
- [ ] T9 -- A field's identity splits exactly where the layer below it splits, and no consumer reads
      how the name resolves. Same shape as T3, one node over.
- [ ] T20 -- Making a member the live one is stated, not chosen by where the reach stands. Reaching
      an active member is one node, which is right, but writing one has to activate it and reading
      one must not, so the backend picks the activating form from the occurrence's position -- two
      arms a reader can tell apart by running the program. The write designator is what states it: a
      write names an owner place and the descent that reaches the part, so the activating form is
      the descent's own and never travels on a node a read shares.

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
- [ ] T21 -- An operation no layer below the front end can meet is not in the vocabulary those
      layers share. Six enumeration queries and methods are answered from the enumeration's own
      declared members where the source is read, yet they sit in the closed set every later layer
      switches over, so each of those layers carries an arm for an alternative it can never see and
      the runtime-entry declaration has to say that no library declares them.
      [builtin-call-identity](../decisions/builtin-call-identity.md) rejected exactly this shape for
      the one other front-end-only operation, and gave that one a vocabulary of its own; these six
      were never held to the same rule.

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
