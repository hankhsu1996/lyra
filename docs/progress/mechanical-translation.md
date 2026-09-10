# Mechanical Translation

The render refactoring: MIR states each semantic fact once, and every consumer -- the C++ render,
the MIR-to-LIR lowering, the dump -- reads it rather than working it out. Done when no backend entry
decides anything the node did not state, and no closed set holds an alternative no node carries.

`../architecture/backend_contract.md`'s Purpose states the finished shape this is measured against,
and its invariant 8 states the form a reader can check: a value-emission entry names no runtime
library identifier, because every name it emits comes from the target's own syntax or from a
dispatch that owns naming. That is the north star for every item here -- an item is finished when
the entries it touched carry punctuation and nothing else.

**That count now answers zero**, and `tools/policy/check_render_names.py` is what answers it. Which
leaves the count no longer sufficient as a finish line, and the reason is worth keeping: the check
used to match library names by their namespace, so it never saw a runtime method spelling an emitter
had invented for itself -- one written where a receiver already stands carries no namespace at all.
It matches on capitalization now, because the target language's own syntax is lowercase, and it was
run in both directions before being believed. What remains below is the other half of "done": no
backend entry decides anything the node did not state, and no closed set holds an alternative no
node carries.

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
- [x] T18 -- Reaching a part named by its declaration-order position is one operation, whether the
      value is a product or holds one member at a time, so no consumer decides what a reach means by
      testing what it reaches into. Whether every part coexists, whether a member that is not the
      live one answers with a default or a run-time failure, and whether reaching one for writing
      settles which is live are the value's own semantics and travel with its type -- the same way
      one coordinate step already spans an unpacked array, a queue and an associative array. The
      layer below carries one selector for the same reason.

      This reverses the entry as first written, which made the two reaches distinct operations with
      a selector kind each. That split was what put two hard-coded spellings in the C++ render, and
      neither of them was a decision the render was entitled to make.

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
- [x] T21 -- An operation no layer below the front end can meet is not in the vocabulary those
      layers share. Six enumerated type methods are answered from the enumeration's own declared
      members where the source is read, and they had been sitting in the closed set every later
      layer switches over, so each of those layers carried an arm for an alternative it can never
      see and the runtime-entry declaration had to carry a way of saying that no library declares
      them. The one other front-end-answered operation had already been given a vocabulary of its
      own for exactly this reason and these six were never held to the same rule; they now have one
      too, and what a call site reaches is again a closed set of three, every member of which the
      library declares. The record of the earlier one described a mechanism the code had since
      replaced, so that account is corrected in the same change.
- [x] T17 -- A machine scalar's width is one of a closed set its type names, so every consumer says
      what each width means and gaining one breaks the build. It had been an open integer, which
      left each consumer to decide for itself what a width it does not recognize means: two refused
      it as a compiler bug, where what they lack is only a spelling and nothing about the program is
      wrong, and the execution backend answered an unrecognized float width with the wider of the
      two it knows, silently. That backend was also composing a target type out of the width where
      its own type mapping already answers that, once for each constant needing one; a constant now
      asks the mapping, the way every other value does.
- [x] T19 -- A construction states which value it builds and in which form, so no consumer works
      either out. Two things were left for one to work out. A sequence -- the handles a declaration
      standing for several objects holds -- was built by a literal of its own rather than through
      its constructor, so the layer below carried one instruction for it and for a plain-data
      element list alike, and the execution backend told the two apart by testing the result type,
      one arm composing the allocating call the IR never stated. And three constructions were told
      apart by how many operands arrived: a queue's four forms, an associative array's three, and a
      format specification's two, each a partial form whose missing operands the consumer read back
      from the count. An operand a form appeared to omit is that operand at the value the omission
      meant -- a container holding nothing is its own element list with nothing in it, a directive
      writing no modifiers writes each at its default, and a literal with no `default:` clause
      answers a read of an absent key with the element type's own default (LRM 7.8.6) -- so every
      construction of a kind now carries the same operands. The one axis a value cannot state, a
      bounded queue's declared maximum (LRM 7.10.5), is read from the type that declares it. The
      runtime lost the entries those partial forms named, and an associative array's absent-key
      answer stopped being an optional whose absence stood for the element default -- which also
      settled a disagreement between the two realizations of that type about whether the answer is
      part of the value a change is detected against.
- [x] T6 -- A runtime operation is named once, in the namespace of the layer that states it. A
      second namespace sat beside the shared one, private to the execution backend, and seven of its
      entries were second names for the three accesses a capability wrapper defines. MIR states
      reading what a wrapper holds, replacing the whole of it, and installing its declared
      representation as calls; what unmade them was a lowering still realizing them the way a
      superseded decision had, flattening each call into an access, after which the backend had
      nothing left to call it by and named it again per wrapper. Two of the three then stood in the
      shared declaration as entries that backend does not realize, while it realized every one of
      them under another name. The other backend never left the call, so one question was answered
      in two places with nothing holding the answers in step -- and the answer that would have
      drifted was the one no reader is positioned to see. The lowering now leaves the call alone,
      and both backends spell it from the one declaration. What the second namespace holds after
      that is one backend's realization of the instructions and constructions the layer below MIR
      states, which no other backend shares and no layer above can name, so it stays where it is:
      what decides a namespace is the highest layer that can state its members, and folding a
      below-MIR realization into the shared one would give the front end a name for an operation it
      can never write -- the shape T21 measured from the other side.
- [x] T7 -- A designated part of a value is named the same way at every layer. A container element
      and a packed slice were reached one way to read and another to write: a read stayed the call
      MIR states it as, while a write descended through the step the layer below states, so one
      operation had two shapes and which one it took was settled by the path that built it. The read
      a write performs on its way down took the second, so the same read appeared both ways in one
      body. That layer's own vocabulary is one extract and one update over a step naming which
      subvalue is reached -- which a product's component already used and these two did not -- so a
      read now lowers to the same extract its write descends by. Which step an entry names is the
      entry's own property rather than a list each side keeps; there had been one list per side,
      held in step by nothing but their both being short. What this leaves: neither form of either
      step reaches the dispatch that says how the library publishes an entry any more -- the reading
      pair is spelled from the step itself, and the designating pair, which that dispatch used to
      refuse, no longer arrives there at all -- so what it answers for those four is unread.
- [x] T22 -- Which operand carries the shape a call's result takes is stated on the entry's own
      declaration, beside the index and the erased spread part it belongs with. The execution
      backend had been answering it three ways: a hand-kept list of the container constructions, a
      second visit of the result type beside the one already naming the construction entry, and a
      conjunction of two unrelated properties standing in for the LRM 7.12 family. The other backend
      never asks, its target language answering from the named type, so nothing held the three in
      step and a construction added anywhere answered "no shape operand" in silence. Which entry
      builds a value of a type, which of its operands seeds it, and what form that entry takes the
      rest in are now one answer read from that type, which is what a construction naming no entry
      beyond its own result type already meant. What a call's target decides about how its operands
      cross is read from the target once and exhaustively, so a target kind gained anywhere says
      what it encodes or fails to build.

      What the conjunction existed to exclude was the two associative dimension queries (LRM 20.7
      `$low` / `$high`), and the rule it excluded them from was already settled: a value crosses into
      an entry erased exactly where it states a representation, and an index of a keyed container
      states one, that container holding no prototype for an index. Those two answer with exactly
      such an index where the dimension has none allocated, and were crossing as the bare handle of
      their own domain -- which answered correctly only because the entry handed the caller's handle
      straight back on that path, so its two ways out agreed by a coincidence of representation and
      not by anything written down. Both now cross the way that rule says, and the erased container
      gained the pair its monomorphized counterpart already carried, so the entry answering them
      names that operation instead of open-coding it.

- [x] T20 -- Making a member the live one is stated, not chosen by where the reach stands, and T14
      -- one visit over the expression set decides how an expression is rendered -- close with it,
      because both were the same missing fact. A write target was a chain of access nodes rooted at
      an opened place, so every consumer found the owner by walking that chain and asking each
      receiver's type what kind of step it was: two backends deriving one semantic fact, which is
      the shape `../architecture/mir.md` forbids and the one `../architecture/backend_contract.md`'s
      cross-check predicts. Every level of a descent is now a call whose entry the lowering names,
      composed through the receiver, so a consumer meets an ordinary call and decides nothing; and a
      write target is built rooted where the write lands, so nothing below recovers an owner. The
      record is `../decisions/value-descent-as-named-calls.md`.

      What fell out rather than being fixed: the four node kinds that named a part, the second walk
      over the expression set -- it had come to differ from the first only by a check of which forms
      are addressable, which nothing asks any more -- the property that said a call stands for
      whatever its receiver stands for, the walk that found a target's root, the recursion that
      re-rooted one, the one that froze a deferred update's chain, and the parameter a deferred
      effect threaded so its steps could bind names, which no step needs. Below MIR the product and
      union selectors became one, because with one entry above them a split below would have forced
      the layer between to choose from the receiver's type.

      One thing this deliberately does not touch, after a first attempt did: what an interior write
      *does*. Reaching a part and writing it in place is the value model
      `../decisions/owner-transition-and-observation.md` settled with a measurement behind it, and
      the first cut of this work replaced it with a functional rebuild on both backends -- which is
      the thousandfold regression `performance.md` names as the thing to re-measure. Where a
      decision is made and what the operation is are separate axes; only the first was in scope.

- [x] T12 -- A compound assignment states the operation it applies rather than the operator, so the
      lift from operator to library entry happens once where the assignment is built and the
      operator set is exactly what a node carries. Three shift operators had sat in that set solely
      because a compound assignment named them: no expression node carried one, every consumer
      meeting one asked which kind it had, and one of them -- the C++ render -- answered by writing
      the applying form of each shift's library method into an emitter, which is the one library
      name the check could not see. An operator the target applies to two values of one type still
      rides the store; one the library performs is now an ordinary call on the place, against an
      entry that updates what that place holds. Both reach the place once, which is the whole of
      what LRM 11.4.1 asks, so the node's own reason is untouched;
      [compound-assignment-write-location](../decisions/compound-assignment-write-location.md)
      carries the revision and the survey behind it.

      Routing that half through the mutating-call path exposed a defect there, and it was the
      cross-check's own shape: a method that changes what it is applied to lowered its receiver
      twice on the execution backend -- once to call the entry, once to store the answer back -- so a
      subscript with a side effect took it twice, while the other backend named the receiver once.
      The receiver is now named once whatever shape naming it takes.

- [x] T9 -- A field's identity states the declaration that declares it and the slot that declaration
      gave it, one alternative per kind of declaration, so no consumer works out which arena
      resolves the name. Five kinds declare fields; four of them had shared one alternative carrying
      a slot and nothing else, on the argument that the receiver's type fixes the arena uniquely.
      That is a derivation, and all three consumers paid for it. Two performed it -- strip the
      receiver's indirection, then, for the one that needed a name rather than a position, classify
      what it refers to -- each step ending in a refusal for a shape it did not recognize, which is
      the fall-through-and-throw shape exhaustiveness names, here in the one place no closed set was
      being switched over so nothing counted it. The third is the dump, which could not perform it
      and so could not say which declaration an access named. Every producer of one of those four
      had the declaration in hand, and one of them read the identity it then dropped, to look the
      field's type up. Stating which storage an access reaches where the access is written, rather
      than recovering it from the type the access arrived at, is the rule the class kind already
      followed; it had to, because inheritance makes the derivation wrong there, and the other four
      now follow it for the reason the rule was written down rather than for the case that forced
      it. The layer below already named a member this way, so the two layers now split a field's
      identity in the same place.

- [x] T16 -- No peephole in a render. One collapsed a scope whose whole content was a single block
      into the enclosing braces, which decides nothing and states nothing: the emitted artifact is
      not read for its looks, and what it cost was a branch that had to be read and kept correct.
      Rendering the block the producer built is the whole of the rule.

      One branch that reads like a peephole stays, and it is a different question. A class with no
      static property initializer emits no design-init body at all, which the emitter decides by
      finding that body empty -- but what it avoids is a startup hook rather than a shorter spelling,
      and whether MIR says "no static initializer" with an absent body or an empty one is a MIR
      shape decision with a sentence of `mir::Class`'s own behind it. It is listed under
      exhaustiveness below, where the rest of the absent-versus-empty questions are.

- [x] T23 -- A method the runtime library provides for an imported class (LRM 9.7 `process`) is
      declared once, the way every other runtime operation is, so a backend spells it and looks
      nothing up. Four tables had stood over that method set instead: what the C++ target calls it,
      what the C ABI calls it, whether the runtime handle rides along, whether the call suspends.
      The first two are naming and are now one row per method beside every other entry's, so the two
      sides cannot drift apart and the emitter that had been composing the namespace itself composes
      nothing. The last two are lowering facts rather than naming ones and stay where the lowering
      reads them.

## An aggregate's members

- [ ] T8 -- An unpacked struct keeps its field names through lowering, so a member access names a
      field rather than a position in a product. Field names are dropped at the front-end boundary
      today, and a settled decision says they are: an unpacked struct is the generic value product,
      and a product declares its components nowhere. So what this item needs first is that argument
      re-opened -- whether a nominal source aggregate is the same concept as the transient products
      lowering builds -- rather than an implementation.
      [unpacked-struct-representation](../decisions/unpacked-struct-representation.md) holds the
      rationale to argue against.

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
- [ ] T24 -- An operation a runtime library carries out is named in one namespace, whichever library
      class the source reaches it through. A second namespace stands beside the shared one for the
      six methods of the imported `process` class (LRM 9.7), and what decides a namespace is the
      highest layer that can state its members -- which is the front end for both, so the count is
      one. Folding them in retires an arm of the callee set, an arm of the HIR callee set, and the
      per-method suspension flag, since a callee whose completion the caller awaits states that in
      its call's type and every other callee already does. **Gated on** nothing but the AST-to-HIR
      call lowering being free, which another subject holds.

## Exhaustiveness

- [ ] T13 -- Every consumer of a closed set says what each alternative means, so gaining one breaks
      the build. A generic catch-all arm switches that off, and where the arm answers instead of
      refusing it answers a new alternative plausibly and wrongly. This is a different axis from the
      rest of this file -- extension safety rather than decision-making -- and it is why several of
      the others went unnoticed: a set that lost a member kept compiling.

      What separates the twenty such arms is whether the arm's answer can be told from a real one.
      An absent answer can, and a caller that has to decide what absence means then says so at its
      own site; a substituted value cannot, and every caller inherits a guess. Three functions asked
      one container's element type three ways -- one covering three container kinds and refusing the
      fourth, one covering four and answering with the container itself, one over the front end's
      own types answering with absence -- which is the whole failure in miniature. The two over the
      lowered types are now one question with one answer. The third stays separate, because the two
      layers name types in different universes and no single function spans both; it already answers
      with absence, which is the shape this item asks for.

      One of them went with T22: the arm that shaped a construction's operands answered a type it
      did not recognize by handing the operands back unchanged, which is a substituted answer no
      caller could tell from a real one, and that form is now read from the type along with the
      entry that builds it. Two more went with the write target's own shape, one at each end: the
      arm that rebuilt a deferred update's chain is gone, because the lowering builds the target
      where the write lands and has nothing to re-root; and the one deciding whether a target
      reaches into a value stopped being a switch over node kinds and became one question of the
      shared entry declaration, which is the difference between classifying a shape and reading a
      stated fact.

      Where a walker's own idea of what has parts was the whole of the arm, the fix is to ask the
      set once. A type now says which types its values hold -- a container's elements and keys, a
      product's and a union's components, a cell's contents -- so the walk that decides whether a
      format operand hides a chandle (LRM 6.14) is a predicate over that answer rather than a second
      enumeration with a default; and a descent step is an ordinary call, so a consumer reaching
      every coordinate of one walks its operands rather than enumerating a selector set of its own.

      What is left is not one shape. Four arms answer a target-language question with a default --
      how a value of a type is constructed, what a member's declaration initializes to, which
      machine type a lowered type maps to, which timing control a delay-or-event form spells -- and
      each needs its own derivation of what the default was standing in for. Two more walk a body's
      expressions asking which operand names storage, which is a question about value category that
      MIR states nowhere; and the type pool's own hash falls through to "these carry no payload"
      through an `if constexpr` chain the compiler cannot check.

      One more is absent-versus-empty rather than a catch-all arm, and it is the shape a walker's
      own idea of emptiness takes at the top: a class always carries a design-init body, empty when
      no static property declares an initializer, and the C++ backend reads that emptiness to decide
      whether the class needs a startup hook at all. Either answer is defensible -- the empty body is
      the zero case handled by not iterating, and an absent one states the fact -- so what this needs
      is the derivation, not a fix under an existing rule.

## Small and mechanical

- [ ] T15 -- A backend meeting IR it has not implemented returns the recoverable failure the error
      policy prescribes rather than reporting a compiler bug.
- [ ] T25 -- A net's fold (LRM 6.6) reaches its field as construction rather than as type payload. A
      member is (name, type), and the C++ backend composes the fold into the field's initializer out
      of the type it carries, which is the one member-render shape the contract names outright. Both
      backends read it from the type, so nothing has drifted and nothing will until one of them
      stops; what it needs is for the fold to arrive the way every other per-member construction
      state does, as an ordinary call in the constructor body, which reshapes the runtime net and
      the execution backend's member storage together.

## Cross-references

- `../architecture/backend_contract.md` -- the mechanical-translation contract and the cross-check
  that a second backend gives it.
- `../architecture/mir.md` -- what MIR's primitive set is closed under, and the forbidden shapes an
  item here is usually an instance of.
- `../decisions/call-receiver-on-the-callee.md` -- T1 and T3's rationale.
- `../decisions/value-construction-forms.md` -- which form a value crosses into an entry in, and why
  a construction is named by the type it builds. T22 turns on both.
- `../decisions/compound-assignment-write-location.md` -- why a compound assignment is a node at all
  rather than the read-apply-write every peer language lowers it to, and what T12 revised about it.
- `refactor.md` -- architectural debt outside this workstream.
