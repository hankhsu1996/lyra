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
      answers a read of an absent key with the element type's own default (LRM 7.8.6) -- so each of
      those three now carries the same operands whichever form the source wrote. The one axis a
      value cannot state, a bounded queue's declared maximum (LRM 7.10.5), is read from the type
      that declares it. The runtime lost the entries those partial forms named, and an associative
      array's absent-key answer stopped being an optional whose absence stood for the element
      default -- which also settled a disagreement between the two realizations of that type about
      whether the answer is part of the value a change is detected against.
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
      nothing. The last two were left as lowering facts, on the reasoning that a lowering rather
      than a backend reads them; that reasoning was wrong and they are entry properties too, which
      is where the item below put them.

- [x] T25 -- A net's fold (LRM 6.6) is installed on the net at construction, so a member declaration
      is (name, type) with nothing read out of the type's payload. The fold was the last thing a
      field declaration composed a constructor argument from, which is the one member-render shape
      the contract names outright and the only instance it names by itself. Neither backend had
      drifted, because both read the same payload; what had kept the fact in the type was that a
      fold has no spelling as a value a call could carry -- a runtime library's own enumerator is
      not a name a value-emission entry may write, and there is no dispatch that owns naming one. So
      the fold is which operation the install names, one entry per fold, which is the remedy the
      contract already prescribed and the shape the four severity entries had already taken for the
      same reason.

      The fold left both type pools with it, so two nets of one data type are one type whatever
      their net types are, and a driver -- which had carried a copy of its net's fold that no
      consumer ever read -- carries what it contributes rather than how that is combined. It left
      the signature too: what a
      referrer knows about another unit's net is that a value reaches it only through a driver,
      never which truth table folds them, so publishing the net type was publishing a fact with no
      reader and the refusal that translated it moved to the declaration that has one.

- [x] T26 -- What decides whether reaching a wait is an event for it is stated where the wait is
      built, so a wait leaf has one shape and no consumer works out which form a construction is by
      counting its operands. Two independent halves decide it, each present exactly where the source
      wrote one: an expression watched at a stated edge (LRM 9.4.2), and an `iff` qualifier (LRM
      9.4.2.3). All four combinations occur, and they had reached the layer below as one
      construction of one type carrying zero, one, two or three operands -- so one backend recovered
      the form from the count while the other handed the same question to its target language's
      overload resolution, which is the cross-check's own shape: two answers to one question with
      nothing holding them in step. The operand list could not settle it alone either, its first
      operand being the watched expression under two forms and the qualifier under a third.

      A construction names no entry beyond its own result type, so a type has exactly one
      construction -- which is what left the count as the only thing to read. The absent-operand
      rule does not reach the second half here: an absent qualifier is that qualifier at the value
      the omission means, but an absent watch has no value at all, there being no expression that
      stands for watching nothing. So which form it is becomes which entry the construction names,
      one per combination, the shape a fact with no spelling as a value already takes elsewhere. The
      leaf then carries an observation whatever it watches, the leaves of one wait share the one
      that says being reached is the whole condition, and the runtime lost the partial leaf form
      that had stood for carrying none.

- [x] T27 -- Reading a value as another type is one operation, and the two types it sits between are
      its whole statement. Five nodes had carried it, and they were not five operations: two were
      identical structures holding one operand, and the lowering below gave the code-address one and
      the reference one the same instruction, so those two had stopped meaning different things
      while both were still spelled. The C++ render gave three of the five the same cast, one a
      shorter spelling of that cast, and one nothing at all -- which is the reading that says no
      consumer was learning anything from the choice that the pair of types had not already said.

      What made the split look necessary was that it turned a conversion nobody had implemented into
      a build break. That property is worth keeping and does not need the split: a backend states a
      no-op only where the two representations are provably the same and refuses every pair it
      cannot realize, so the unimplemented pair is a diagnostic instead of a value that silently
      crossed unconverted. The invariant the kinds had been standing in for -- that a cast between
      two packed values must not change what the bits structure -- is one rule read off the two
      types where the layer below is verified.

      This reverses a Forbidden Shape, which had named the single node rather than the thing that
      was wrong with the several.
      [cast-is-a-pair-of-types](../decisions/cast-is-a-pair-of-types.md) holds the argument, the
      survey of where a cast kind is real, and why it is not here.

## An aggregate's members

- [x] T8 -- An aggregate the source declared keeps its member names through lowering, so what a
      value renders as is settled by its type rather than left outside it. The argument this item
      said it needed first was re-opened and went the other way from the settled decision: the
      language renders a value by the names its type declares (LRM 21.2.1.6), so the names are
      observable behaviour rather than presentation, and a shape-interned product has no key to hang
      them off. Reaching a member is unchanged and stays positional, which is what every IR that
      carries a record type does -- the item as first written asked for a named access too, and the
      survey did not support it.

      What the item had underestimated is how far the same root reached. It named the unpacked
      structure; the union, the tagged union, and both packed aggregates had lost their names the
      same way, the packed pair by having no type of their own at all below the front end. All five
      now name their members, and each projects to the representation it already had -- the product,
      or the single vector -- so no value operation and no runtime realization changed.

      **The names existing below the front end was the precondition, not the whole of it.** What
      still has to reach the formatter is described in [`display.md`](display.md), whose recorded gap
      names the mechanism: a print item that carries how its elements render. A container's elements
      are counted at run time, so nothing composed at the lowering can reach them -- which is why the
      one place this works today, an enumeration written as the whole operand, works only there.

      Giving the packed pair a type of their own is what turned seven latent wrong answers into
      build-visible ones: every site that had asked "is this integral?" by testing for the plain
      vector stopped seeing a packed aggregate, and each was a question about integrality that had
      been spelled as a question about one alternative. Two more sites had been reading the anonymous
      product to mean a declared structure.
      [aggregate-names-are-type-content](../decisions/aggregate-names-are-type-content.md) holds the
      argument, and what the two superseded records keep.

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
- [x] T24 -- An operation a runtime library carries out is named in one namespace, whichever library
      class the source reaches it through. A second namespace had stood beside the shared one for
      the six methods of the imported `process` class (LRM 9.7), and what decides a namespace is the
      highest layer that can state its members -- the front end for both, so the count is one.
      Folding them in retired an arm of the callee set at three layers and every table that had
      stood over that method set alone.

      The fold did not go through as written, and what stopped it is the reusable part. It said the
      per-method suspension flag would retire, "since a callee whose completion the caller awaits
      states that in its call's type". A user task does; a runtime entry that parks the caller does
      not, and the delay and the value-change wait were already in the shared namespace proving it
      -- each answers with nothing and is awaited because the entry parks, not because a type says
      so. Typing such a call as a coroutine would have been a claim about the value the library
      hands back that is not true of it. So the flag did not retire, it moved: onto the entry's own
      declaration, where the entry it belongs to already carries every other property, and where the
      delay and the wait now state it too. The lesson is that "the type already carries this" needs
      checking against the entries already in the namespace, not only against the ones being added.

      The same move took the other per-method table -- whether the call carries the engine handle --
      and with it the one site that had been answering that question by naming a single entry.
      Because the answer is now a property of every entry rather than of six, it is checked rather
      than trusted: the ABI's own prototype declares the handle or does not, and the policy check
      that already holds each entry's three sides together holds this fourth one against them.

      What the fold needed first was for a built-in call to state the object it acts on. It had been
      argument zero, which the lowering re-read as either a receiver or a discardable bearer by
      looking the entry up -- and neither reading has room for a static method that acts on no object
      and bears no type, which `process::self` is. The call states the object now, so every consumer
      reads it, and the lookup that had stood in for it went with it. It had never chosen the second
      answer in any case: the one entry that would have taken it is routed elsewhere before the
      branch is reached.

## Exhaustiveness

- [x] T13a -- Every consumer of a closed set says what each alternative means, so gaining one breaks
      the build. A generic catch-all arm switches that off, and where the arm answers instead of
      refusing it answers a new alternative plausibly and wrongly. This is a different axis from the
      rest of this file -- extension safety rather than decision-making -- and it is why several of
      the others went unnoticed: a set that lost a member kept compiling.

      What separated the twenty arms the survey found is whether the arm's answer can be told from a
      real one.
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

      The type pool's own hash fell through to "these carry no payload" over a chain the compiler
      cannot check; it now consumes the set the way the layer below already did, one arm per
      alternative, so a type variant gained anywhere says what its identity is or fails to build.

      Four more answered a question with a default, and each is now one arm per alternative carrying
      its own reason. Which machine type a lowered type maps to answered "an address" for every type
      it did not name; that is the right answer for most of them and unfalsifiable for the rest,
      because a pointer is exactly what a wrong mapping produces and the target accepts it. It now
      says per type which of four things it is: a machine type naming itself, a value the runtime
      realizes as an object of its own, storage reached by where it lives, or a value that already
      is an address. How a value of a type is constructed answered "name the type", which is right
      for every type but the three wrappers that bring what they point at into existence along with
      themselves. What a member's declaration initializes to fell through to the translated type's
      canonical default for everything but an unpacked struct and a fixed unpacked array; an
      unpacked union -- which the standard's table gives the first member's type default rather than
      that member's written initializer -- had been answering correctly for a reason nothing stated,
      and a variable-size container had not been answering correctly at all. That is the one arm
      here whose rewrite changed behaviour, and it is what the item predicts a defaulting arm hides:
      the table reads itself again below a container too, so a queue, a dynamic array, and an
      associative array had been taking the value an invalid read answers with from the lowered
      element type, which drops every member initializer. And which timing
      control a delay-or-event form spells refused the repeat form while its one caller had already
      branched to keep that form away from it, which is one decision made in two places; the caller
      now says what each of the four forms does, and LRM 9.4.5's count of none reaches the
      assignment by a loop not iterating rather than by an arm of its own.

      The last of them was the walk that decides which of a body's locals need an address, whose
      catch-all answered "this kind asks for nothing" over every expression kind it did not name --
      including the kinds that designate part of a local to write it. Every kind says so for itself
      now. What that walk is doing at all is a separate question, and it is T13b below.

      The absent-versus-empty question is settled, and the answer is that nothing changes. A class
      carries a design-init body holding whatever it brings up before any process runs, and the
      target emits a startup hook only where that body has statements. Both arms produce the same
      program, so this is a spelling rather than an operation, and it dispatches on the body's own
      statement list rather than on anything worked out elsewhere. Making the body absent instead
      would not remove the derivation, only move it into the producer, which would reach the same
      answer from the same list and hand every consumer a nullable to open first -- because whether
      a class has design-time work of its own is settled while lowering, when a class declared
      inside a structural scope has its statics brought up by that scope's instance instead.
      Emitting the hook unconditionally is the shape where zero falls out of N, and what it costs is
      not looks: the hook is a dynamic initializer per class, every scope of a design is a class,
      and a design's translation unit is compiled unoptimized by default.

- [ ] T13b -- Whether a local needs an address is stated where it is known, rather than recovered by
      a pass that reads the whole body before any of it is lowered. Today the execution lowering
      walks every expression of a body twice over -- once asking which locals a write or an
      address-of reaches, once asking which are lent by reference -- and only then begins. That is a
      decision being made at the layer with the least information about it: what the walk is
      recovering is value category, which the layer above states nowhere, so each operand's position
      has to be read back out of the tree it already sits in.

      This is a different axis from T13a and does not close with it. T13a made the walk total, so a
      new expression kind now says whether it asks for an address instead of silently answering that
      it does not; the walk still exists. Retiring it means a semantic layer that states which
      occurrences are places, which is the same question as whether a value has an address at all --
      so it closes with that decision rather than here, and nothing about it should be built twice.

## Small and mechanical

- [x] T15 -- A backend meeting IR it has not implemented returns the recoverable failure the error
      policy prescribes rather than reporting a compiler bug. Of the sixty-two refusals the two
      backends and the MIR-to-LIR lowering carry between them, sixty-one state an invariant the
      compiler itself established and are compiler-bug reports correctly. The one that named an
      operation not yet carried out was not an unimplemented case either: it guarded a state MIR has
      no producer for, and both consumers of that state guarded it, one calling it a bug and the
      other a refusal -- which is the cross-check's own signature, and here it was pointing at
      something neither of them could see.

      A call carried the scope its callee is reached on beside the type of the value it answers
      with. Every producer of that scope named a static factory, which the runtime declares on the
      type it builds -- so the two were one type, stated twice, and each backend chose which of them
      to read. The second statement is gone from both IR layers, along with three refusals over
      states nothing reaches: two that policed a scope against a target that never carries one, and
      one that answered an absent scope on a factory that always has a type. The execution backend
      had a fourth, inside a rule reading "the value the call qualifies itself with, or an argument
      where it qualifies itself with nothing" -- one question answered twice at one site, now the
      naming set's own alternative, since a factory takes no object and no destination and is named
      by what it builds.

      The absent producer was itself a finding. The builtin-method lowering branched on whether the
      entry was a factory to decide which scope to state, and the front end mints exactly one
      factory, which is routed to a conversion path before that branch is reached -- so the branch
      had never run. It went with the scope it existed to state.

## Naming ownership

- [ ] T23 -- Every name a render emits comes from something that owns naming, and re-viewing an
      object reference has no owner. Three owners exist -- type mapping for a type, place access for
      a wrapper's access protocol, the shared runtime-entry declaration for an operation -- and "the
      same object, seen as another class" is stated as a cast node rather than a call, so a backend
      that cannot spell it in target syntax alone writes the name itself. The source backend now
      does. The obvious move, making it a call so the shared declaration owns the name the way
      `this` already is owned, does not work as stated: the execution backend's naming set
      classifies every entry as naming a library entry or as not realized, and it realizes this cast
      with no instructions at all, so the move would force it to gain an entry it does not need or
      to refuse one it answers today. Two things could be true and the item is which -- the list of
      naming owners is short one owner, or a re-view should not be a cast.

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
