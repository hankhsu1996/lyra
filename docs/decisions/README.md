# Decisions

Logged architectural decisions. Each entry records a decision with its rationale; the entry is
immutable once accepted, and a superseding decision links back to the one it replaces.

Decisions are reserved for choices with real trade-offs: rejected alternatives, load-bearing
invariants, or constraints that bind the codebase going forward. Housekeeping notes (e.g., "this
item is subsumed by an existing surface") do not warrant a decisions entry; record the reason inline
at the point it matters.

## Index

Grouped by subject so a decision is findable by concept, not only by filename. One line per entry;
the detail lives in the entry itself.

### Value types and representation

- [a-constant-is-stated-not-computed](a-constant-is-stated-not-computed.md) -- a unit holds the
  values it was written with and an occurrence names one, because a value fixed before the run costs
  a read rather than a construction, and no optimizer recovers that.
- [a-container-operation-asks-the-type-or-the-values](a-container-operation-asks-the-type-or-the-values.md)
  -- a whole-container operation takes each fact from whichever of the declaration and the values is
  guaranteed to have it, and a type fixing no representation is still realized as what reaches its
  position. Widening a pattern's keys to a common type, reading an element's shape off the first
  element, and carrying a wildcard index's comparison on the keys are rejected.
- [integral-representation](integral-representation.md) -- one fat `PackedArray` carries integral
  shape as runtime fields, not C++ template parameters.
- [value-store-discipline](value-store-discipline.md) -- a value is pure; preserving the
  destination's declared type across assignment lives at the store boundary, not the value.
- [value-type-concepts](value-type-concepts.md) -- the `lyra::value` operator surface is a lattice
  of composable C++ concepts, one per LRM operator family.
- [runtime-shape-and-default-value](runtime-shape-and-default-value.md) -- runtime shape lives on
  `PackedArray`; one OOB shield slot is both the canonical default and the out-of-bounds discard.
- [string-packed-conversion](string-packed-conversion.md) -- a `value::String` holds no NUL;
  packed-to-string strips NUL, `%s` formats bits without a string value.
- [enum-representation](enum-representation.md) -- an enum's semantic type identity is separate from
  its runtime value; the value is the base integral, never a distinct C++ type.
- [a-handle-is-a-value](a-handle-is-a-value.md) -- a class handle is a value domain like any other,
  so every position the language states over a data type admits one; and which managed edges a value
  holds is answered by the value, not by a description beside its storage.

### Aggregate types and access

- [packed-array-representation](packed-array-representation.md) -- HIR represents a packed array
  recursively (one dim per node); MIR stays flat and HIR-to-MIR flattens.
- [packed-shape-belongs-to-the-type](packed-shape-belongs-to-the-type.md) -- a packed value carries
  its width, signedness, state domain and bits; how a declaration divides those bits reaches an
  access that names a position as an operand, which is what every other selectable family already
  did.
- [unpacked-array-representation](unpacked-array-representation.md) -- representation of a
  fixed-size unpacked array.
- [unpacked-struct-representation](unpacked-struct-representation.md) -- an unpacked struct is a
  value product and not an object, positional access, defaults synthesized at lowering.
- [unpacked-union-representation](unpacked-union-representation.md) -- the sibling the struct
  decision left open: overlapping storage is neither a product nor a sum, and this settles which one
  MIR models it as.
- [aggregate-names-are-type-content](aggregate-names-are-type-content.md) -- an aggregate the source
  declared names its members in its own type, whose value-domain projection is the product or the
  vector a type naming nothing already has.
- [unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md) -- an unpacked array's index
  range is part of its type, not a size carried beside it; packed arrays are carved out.
- [selector-coordinate-resolution](selector-coordinate-resolution.md) -- `a[1:7]`, `b[7:1]`, and
  `c[0:6]` are three distinct types, so resolving a subscript to a coordinate is the type's job.
- [jit-aggregate-realization](jit-aggregate-realization.md) -- on the execution backend every
  aggregate is a runtime-owned opaque value (erasure), not structurally monomorphized; the choice is
  below LIR, and LIR's aggregate operations stay realization-agnostic.
- [slice-value-semantics](slice-value-semantics.md) -- a slice read materializes an owned value; the
  access model is value, not borrow.
- [value-projection-write](value-projection-write.md) -- a value-aggregate interior write is an
  owner-relative functional whole-value update through the owner, not a place store; the in-place
  write a target may realize it as is a property of that target. How the update is stated is
  superseded by [value-descent-as-named-calls](value-descent-as-named-calls.md).
- [value-descent-as-named-calls](value-descent-as-named-calls.md) -- every level of a descent into a
  value is a call whose entry HIR-to-MIR names, composed through the receiver, so no node names a
  part and no consumer classifies a step or recovers an owner. Supersedes the selector-path node of
  [value-projection-designator](value-projection-designator.md), which was never built.
- [queue-operators](queue-operators.md) -- queue access operators lower to built-in method calls;
  read and write are distinct methods chosen at lowering. Where it places the receiver is superseded
  by [call-receiver-on-the-callee](call-receiver-on-the-callee.md).
- [concatenation-realization](concatenation-realization.md) -- a join is a call rather than a node
  of its own, over every operand family, and reaches MIR already folded to the two operands every
  entry that performs it takes.
- [a-value-states-its-own-bits](a-value-states-its-own-bits.md) -- which bits a value makes is the
  same question as how many, so a value answers both, and a bit-stream cast and a streaming operator
  are the same pack and unpack stated against those entries plus one that re-orders a bit vector.
  Unrolling the traversal at the lowering, a direction operand, a type reference in place of the
  prototype, and typing a dynamically sized stream by its fixed part are rejected.
- [value-construction-forms](value-construction-forms.md) -- a construction says which form it is; a
  value that is its own parts is a primitive, a container built from one is a call, and what names a
  call is the type's own answer.
- [array-method-dispatch](array-method-dispatch.md) -- LRM 7.12 array-method runtime semantics;
  per-family dispatch superseded by [builtin-call-identity](builtin-call-identity.md).
- [array-manipulation-entry-stream](array-manipulation-entry-stream.md) -- LRM 7.12 locator /
  reduction / `map` operate over an ordered `(index, element)` stream; ordering stays in-place.
- [format-dispatch](format-dispatch.md) -- value formatting dispatches through `Formatter<T>` and
  `FormatArg`.
- [rendering-a-value-by-its-type](rendering-a-value-by-its-type.md) -- how a value reads under LRM
  21.2.1.6 is a callable synthesized per type, not a description handed to a formatter. Where that
  callable is homed is revised by the next entry.
- [a-type-owned-computation-has-no-object](a-type-owned-computation-has-no-object.md) -- a
  computation a type owns takes no object, so the unit's namespace owns it and its identity is the
  position it sits at; a name is a relation over that position, held only where the source wrote
  one.
- [a-types-readings-exist-because-the-type-does](a-types-readings-exist-because-the-type-does.md) --
  which readings a type owns follows from the type alone, so they are settled with the unit's
  declarations rather than at whichever site asks first; and a type that is not a value has none.

### Lowering and IR shape

- [exhaustive-alternative-consumption](exhaustive-alternative-consumption.md) -- a closed set of
  alternatives is consumed by a switch, never by `==`, so a fact the front end resolved cannot be
  dropped silently.
- [lowering-organization](lowering-organization.md) -- how lowering passes organize their internal
  objects (facts, registries, builders, walk frame).
- [storage-access-as-place-formation](storage-access-as-place-formation.md) -- a dereference of a
  capability wrapper's place names the storage it represents, and each backend supplies the protocol
  from the place's type; naming it that way is never a call, while reading it and replacing the
  whole of it are, per [owner-transition-and-observation](owner-transition-and-observation.md),
  which supersedes both of those answers here.
- [declarations-before-bodies](declarations-before-bodies.md) -- every structural declaration's
  identity and shape is CU-global and queryable before any executable lowering begins.
- [a-name-arrives-with-the-identity](a-name-arrives-with-the-identity.md) -- what a declaration is
  called is fixed when the source names it, so a pool minting identities ahead of values answers it
  the whole time and holds it in one place; and a scope's names are complete before any body it owns
  is walked, its variables' initializers and its classes' bodies included. Ordering the signature
  after the declarations, recomputing the name from the front end, checking whether the declaration
  has landed, and reading part of an unsettled value are rejected.
- [reporting-every-gap-in-one-run](reporting-every-gap-in-one-run.md) -- a refusal is collected and
  the stage goes on, so one run accounts for every unit and every member rather than for the first
  that stopped; a stage that reported anything is the last one that runs, and what it produced is
  discarded.
- [the-request-names-its-products](the-request-names-its-products.md) -- what a compilation step
  answers with is decided by what was asked of it, so no caller asks whether a product it requested
  is there and the only absence left means the run failed; a depth fixed at compile time, one entry
  answering with an alternative per depth, a refusing accessor, and lowering only the units that
  root objects are rejected.
- [foreach-lowering](foreach-lowering.md) -- the lowering shape of `foreach`.
- [compound-assignment-write-location](compound-assignment-write-location.md) -- one node per write
  target, evaluating the left-hand side exactly once (LRM 11.4.1); revised so an operator the
  library performs is applied by the entry that performs it, and superseded for value interiors by
  [value-projection-write](value-projection-write.md).
- [conversion-folding](conversion-folding.md) -- when type conversions are folded.
- [shape-from-types-contents-from-expressions](shape-from-types-contents-from-expressions.md) -- a
  lowering reads a number it needs before run time from a type, never from an expression; an operand
  stays the expression it is and is never evaluated or matched for a literal, and a pattern key is a
  designator rather than an operand.
- [variable-initialization](variable-initialization.md) -- LRM 10.5 variable initialization as a
  constructor-scope statement.
- [variable-lifetime-storage](variable-lifetime-storage.md) -- storage of static-lifetime body
  locals.
- [lifetime-extended-automatic-scope](lifetime-extended-automatic-scope.md) -- an automatic scope a
  process may outlive is a shared-owned activation object; a detached branch captures the handle by
  value.
- [read-set-inference](read-set-inference.md) -- read-set inference via slang flow analysis.
- [runtime-effects-as-generic-calls](runtime-effects-as-generic-calls.md) -- runtime effects lower
  to ordinary `CallExpr` with the engine handle as one argument.
- [ambient-runtime-services](ambient-runtime-services.md) -- generated code reaches the runtime
  through a thread-local `current_runtime()` the attached Runtime publishes for its lifetime;
  `RuntimeEffects` is the narrow capability surface, `Runtime final` adds host orchestration; the
  receiver-based route is retired.
- [callable-receiver](callable-receiver.md) -- every callable body's first binding is `self`; how it
  is supplied differs per callable form.
- [unified-callable-model](unified-callable-model.md) -- one callable concept: callable code vs
  callable value (code + bound environment), with no kind tag on the body.
- [block-expression](block-expression.md) -- several steps in value position are a block expression,
  the one node that lifts any statement sequence into an expression; it sequences and nothing else,
  so its steps do not return. A closure invoked where it is built is rejected.
- [closure-environment-and-activation-frame](closure-environment-and-activation-frame.md) -- a
  closure (`ClosureType`, an anonymous concrete callable value: captures plus one invoke) and a
  promoted automatic scope (`StructType` reached via `Shared<>`, fields only, no invoke) are two
  distinct nominal categories sharing only the field substrate, not one fused type with an optional
  invoke; the callable value has a concrete `ClosureType` level and an erased
  `ErasedCallableType<Sig>` level with an explicit erasure.
- [builtin-call-identity](builtin-call-identity.md) -- built-in method calls carry a flat
  closed-namespace identifier (`support::BuiltinFn`) shared by HIR and MIR. Its positional receiver
  convention is superseded by the next entry.
- [call-receiver-on-the-callee](call-receiver-on-the-callee.md) -- the object a call dispatches on
  is a field of the callee, not the first of its arguments, so no consumer works out which operand
  is a receiver, and no callee target exists only to say whether there is one. Splitting every
  target by receiver-ness, putting the receiver on the call node, and verifying the positional
  convention are rejected.
- [address-of-primitive](address-of-primitive.md) -- MIR carries an explicit place-to-pointer
  operator (`AddressOfExpr`), dual to `DerefExpr`; the backend never injects `&`.
- [cast-is-a-pair-of-types](cast-is-a-pair-of-types.md) -- a cast is one node whose operand type and
  result type are its whole statement, so no kind sits beside them; a backend refuses a pair it
  cannot realize rather than passing the value through. One node per conversion, and a kind
  enumeration on the node, are rejected.
- [event-control-unification](event-control-unification.md) -- unified treatment of event control:
  every value-change wait (`always_comb` / `@*`, `@(...)`, `wait (cond)`, a continuous assignment)
  is one shape over a per-leaf `(observable, bit_range)` set. Its MIR carrier is superseded by the
  next entry, and what a leaf's bit range decides by the owner-transition entry.
- [value-change-wait-as-runtime-call](value-change-wait-as-runtime-call.md) -- that wait is an
  ordinary runtime call taking the trigger set, awaited like every other suspending call; MIR
  carries no event-control node, and one enum for the edge specifier is shared by compiler and
  runtime. A dedicated MIR statement, a per-leaf registration call, and an engine subscription verb
  are rejected.
- [update-due-on-an-event](update-due-on-an-event.md) -- an assignment whose update is due on an
  event states no slot where it stands, so the update is carried by an execution of no lineage that
  waits, places itself in the NBA region of the slot the event lands in, and writes there. A
  membership naming a closure, a `fork` branch, and submitting a closure after the event are
  rejected.
- [generic-lowering-machinery](generic-lowering-machinery.md) -- generic arena and shared
  context-free expression-handler templates over the pass class; node types stay typed.
- [arena-reference-lifetime](arena-reference-lifetime.md) -- `Arena::Get` is a transient view; the
  `Id` is the only durable handle, so lowering projects value facts before mutating.
- [mir-type-interning](mir-type-interning.md) -- the MIR type pool is a structural-equality
  interner; each semantic type has one canonical `TypeId`, enabling recursive class types.
- [lir-type-interning](lir-type-interning.md) -- the same for LIR, the layer that had not taken it:
  a LIR type's identity is its content, so a type built by the lowering and one translated from MIR
  are one type. Nothing needs excluding from the key, because LIR carries no source-language
  concept; the one field that did had no reader and is deleted.
- [hir-type-interning](hir-type-interning.md) -- the same for HIR, so a type published by one unit
  and read by another lands on the entry the reader already had; identity is the unit's own, never
  the frontend's.
- [context-free-call-lowering](context-free-call-lowering.md) -- one expression dispatcher template
  per boundary; the call family becomes a template once the `with`-clause element / index are
  co-equal closure parameters.

### References and construction

- [hierarchical-reference-routing](hierarchical-reference-routing.md) -- one semantic shape per
  hierarchical reference; per-segment classification by layout visibility; sealed endpoint on the
  hot path.
- [hierarchical-callable-dispatch](hierarchical-callable-dispatch.md) -- a subroutine a hierarchical
  name enables is the route to the declaring scope plus whatever answers the name there; a module
  publishing its subroutines is excluded because the dependency graph must stay acyclic, so its
  subroutines are answered by the scope itself. What a `disable` names is the same route ending at
  the scope's own activity, which no unit publishes and which needs no name.
- [binding-graph-resolution](binding-graph-resolution.md) -- resolution and sealing respect
  dependencies between references; forwarding chains collapse end-to-end.
- [hierarchical-reference-resolution](hierarchical-reference-resolution.md) (superseded) -- the
  prior decision, replaced by the two entries above.
- [specialization-identity](specialization-identity.md) -- a specialization's identity is the module
  name plus a content hash of its parameter bindings, computed independently by producer and
  consumer.
- [reference-as-data-type](reference-as-data-type.md) -- a reference is a direction at HIR and a
  data type at MIR; one type serves `ref` formals and `ref` ports, preserving the observable-cell
  protocol.
- [reference-binds-a-cell](reference-binds-a-cell.md) (reopened) -- what that protocol makes a
  reference on the execution backend: the address of a value cell, so every referent is one and a
  local whose storage is lent gets a cell where it is declared. Being lent decides what kind of
  storage a local gets and the declaring scope decides how long it lives, so the cell is a slot of
  the body's own frame, begun and ended by the compiler on every way out -- including the one a
  suspension takes when its driver ends the execution rather than resuming it. A callee has one
  formal, so the reference cannot vary with the storage its caller lends; the address of the
  referent's own storage, a runtime reference object, and a polymorphic storage core are rejected.
  Reopened 2026-09-03: it answers how a place is lent by deciding how every place is represented,
  which is the wrong way round, and the wider reference its rejections argue against is what the
  other backend already runs. The ownership question its reopening left open is answered by
  [storage-owns-its-value](storage-owns-its-value.md), and the contract it asked for is written by
  [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md).
- [a-declared-local-is-storage](a-declared-local-is-storage.md) -- a local has storage because the
  source declared a variable, so the lowering asks nothing about what the body does with it; where
  that storage lives follows the local's own lifetime. Deciding a variable need not exist is a
  saving taken by whoever sees the whole function, and taken upstream it refused legal programs
  wherever its idea of a write was narrower than a write.
- [a-declared-variable-is-one-storage](a-declared-variable-is-one-storage.md) -- the other half of
  that same pass, and the last question the translation answered from reading a whole body. A
  variable is one storage its declaration decides, owned by the execution that declared it and ended
  by the compiler on every way out including the one no statement spells. Read it for why the
  obvious argument for emitting nothing there expires, and for the test that separates an answer
  that survives the work ahead from one that does not.
- [a-body-holds-a-value-or-its-execution-stores-it](a-body-holds-a-value-or-its-execution-stores-it.md)
  -- the third cut of that same question, and the one the standard settles outright. A variable is a
  value of its body where the body holds the whole of that value and storage its execution owns
  where it does not, which puts a class handle on the storage side and a chandle on the other for
  the reason Table 8-1 gives. Which types those are is one classification answering for every type,
  since a list of the ones somebody remembered had already answered wrongly twice. Read it also for
  what a variable naming an object is waited on for, and for why holding a reference in the target's
  own frame is the wrong place rather than the unbuilt one.
- [inline-member-slots](inline-member-slots.md) -- a storage block whose owner cannot move holds its
  slots inline, one allocation for the block rather than one per member: an object's properties and
  a scope's members qualify, and a closure's captures did not until
  [construct-in-final-home](construct-in-final-home.md) stopped the closure value moving -- the
  per-slot indirection was what made that block movable at all. A slot stays non-movable, since a
  cell's identity is its address. Keeping per-slot allocation everywhere, making slots movable, a
  `std::vector` of slots, and a separate block type for closures are rejected; variant space
  amplification is the value representation's problem, not this one's.
- [construct-in-final-home](construct-in-final-home.md) -- a long-lived runtime object whose
  execution state binds to its own address is constructed where it will live, so a closure value is
  built into the region or the execution that will own it rather than in the arena and moved. One
  construction entry takes the home to fill, captures become inline, and a coroutine body's frame is
  built at construction, so the deferral that existed only to survive the move disappears. A
  nonblocking assignment costs one allocation instead of N + 3. Build-then-move, an entry per
  destination, a permanent capture indirection, and relocatable slots are rejected.
- [referenceable-objects-have-stable-addresses](referenceable-objects-have-stable-addresses.md) -- a
  class object a reference can point into neither moves nor is reclaimed while that reference lives,
  so a property reference is an ordinary interior pointer. Non-moving stops being a mere realization
  choice, and a future relocating collector pins rather than changing what a reference is. That an
  outstanding `ref` keeps the object alive is Lyra policy where the standard is silent, framed as a
  property of the object -- the analogue of LRM 13.5.2's detached container element -- so a
  reference still never owns. An indirect collector-aware property reference, pinning adopted now, a
  dangling property reference, and making a property reference a managed edge are rejected.
- [container-element-storage](container-element-storage.md) -- a variable-size container stores
  logical membership and ordering while its elements live in stable storage it does not move: a
  queue keeps the ordering of element slots, an associative array maps keys to entry storage,
  removal separates membership from lifetime, every traversal walks membership so a detached element
  is never visited, and recreating an associative key makes a new identity. A linked list, the
  current contiguous buffer, storage held in the map's own nodes, per-container keep-alive
  machinery, refcounting each slot, and a stable-element library container are rejected.
- [array-element-storage](array-element-storage.md) -- a fixed and a dynamic array give each index a
  persistent slot and a reference binds the slot, so ordering methods permute values among existing
  slots, and a dynamic array generation is a contiguous run that resize replaces whole, retaining
  the old generation while a reference into it lives. The discriminator against the queue and the
  associative array is single-element removal, which only those two have. Element identity for
  arrays, per-element slots for the dynamic array, per-element detachment, and splitting the two
  array kinds are rejected; LRM 7.12.2 does not say whether an ordering method moves elements or
  values, so that half is Lyra policy rather than a requirement.
- [call-scoped-borrow-registration](call-scoped-borrow-registration.md) -- who keeps a detached
  element alive: nobody takes ownership. The arena goes on owning the storage and a call extent
  registers a borrow, so retired storage is reclaimed once membership has ended and the last
  borrowing invocation has. The unit is the live call extent, deduplicated per storage identity
  within one invocation; forwarding a reference is not a bind, and nothing touches the access path.
  Shared-pointer counting, transferring ownership to a frame, a container-side retire list, a
  carried lifetime token, and block-granularity registration are rejected.
- [update-events-are-per-variable](update-events-are-per-variable.md) -- a write anywhere inside a
  variable is one update of that variable, emitted by the compiler from the place it already knows;
  a wait is an expression whose dependency set and previous result live in the subscription, and an
  event is reported only where the result changed. LRM 4.3 puts the update on a net or variable,
  9.4.2 puts the filter on the expression and permits reevaluating more often, and 13.5.2's
  enumeration says a component is not a variable -- so there is no ancestor chain, only one variable
  and several expressions reading it. It closes the hole `storage-owns-its-value` opened by taking
  the component write off the containing cell's store path. Parent pointers on component storage, an
  update per component level, deriving the update set at run time, and restoring the whole-value
  store are rejected.
- [event-subscription-model](event-subscription-model.md) -- a subscription holds a waiter and a
  list of event-expression leaves, each owning its dependencies, its previous evaluated result, its
  change-or-edge predicate and an optional `iff` gate. A dependency's update reevaluates a leaf; a
  gate is read only after the leaf triggers and is never depended on. Edge and bit-range become fast
  paths over reevaluate-and-compare rather than primitives, dependencies are typed as event sources
  rather than variables, and a level-sensitive `wait` needs no leaf of its own -- it is already a
  loop around change leaves, which is why a compound `wait` condition is safe today while a compound
  `@` expression is refused. One dependency set per subscription, an `iff` operand as a dependency,
  edge and bit range as primitives, a `wait`-specific subscription kind, and variable-typed
  dependencies are rejected.
- [object-is-an-event-source](object-is-an-event-source.md) -- the second kind of event source the
  entry above named and did not define: an object carries one coarse source covering all of its
  properties, a write publishes to the innermost object its place dereferenced and otherwise to the
  declared variable, and a leaf depends on the sources its evaluation reached rather than on the
  ones its syntax names, recollected at every reevaluation so a handle write rebinds. LRM 9.4.2
  permits reevaluating for members the expression never reads, which is what makes one source per
  object conforming, and the clause's own example fires `@(p.status)` on the handle write itself
  when the new object's member already differs. A null handle is an illegal access rather than a
  state the model represents. A per-property source, observable property storage, deriving the
  object dependency from the syntax, static and dynamic as two kinds of dependency, depending on the
  handle alone, and a parent pointer from property storage are rejected. It sharpens
  `update-events-are-per-variable` invariant 4 rather than reversing it: a place may name a target
  whose identity is resolved at run time.
- [event-source-has-two-realizations](event-source-has-two-realizations.md) -- what a source costs
  to have, settled by counting rather than by intuition: a declared variable's source is provisioned
  because 83% of a design's cells are genuinely subscribed to, while a class object's is
  materialized on demand because almost none are. One semantic concept, two physical realizations,
  and no realization difference may reach the layer that reasons about dependencies. A hash table
  keyed by the storage address is refused for the declared variable outright -- it would be paid on
  83% of writes to buy the 17% that need nothing -- which is the shape an intuition about sparsity
  leads to. Go's address-keyed `semtable` and HotSpot's inflate-on-contention mark word are the
  precedents for the sparse side and are wrong for the dense one. One representation for both, a
  lazy declared-variable source, and intrinsic state on every object are rejected. The physical
  shape of the declared-variable source is deliberately left open: most cells need one, so shrinking
  it is worth a multiple of making it conditional.
- [source-anchors-an-intrusive-ring](source-anchors-an-intrusive-ring.md) -- the physical shape the
  entry above left open: a source is a 16-byte intrusive ring anchor holding nothing else, a
  membership is 24 bytes of two links and one payload pointer, and the predicate lives once on the
  leaf. Measured field by field, 32 of the 48 bytes a cell spends today are ones a list header is
  defined never to have, and 24 of the 48 on every membership are the leaf's fire condition riding
  along -- including on the scheduler-queue memberships that churn every delta cycle. Ibex's
  observation machinery falls 38-48%, from 231 KiB to 143-121, the spread being whether a
  single-leaf subscription and its one membership are one allocation. A one-pointer `hlist` anchor
  is strictly smaller and is rejected anyway, because the node shape is exclusive and only the ring
  splices a whole queue onto another in constant time, which the scheduler does every delta cycle. A
  zero-byte source, the fire condition on the membership, and a per-target membership type are also
  rejected; compile-time fanout survives as a specialization above the runtime shape.
- [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md) --
  referenceability does not imply observability: a storage object carries no subscriber metadata
  merely because something may pass it by `ref`, plain and observable storage are two forms, and
  observability is a simulator concern separate from storage identity. A class property is not made
  observable by `@(p.status)` having to work, because LRM 9.4.2 requires the event expression to be
  reevaluated and explicitly permits reevaluating more often than the members are referenced. One
  unified cell, per-property subscribers, and demand-driven observability under separate compilation
  are rejected.
- [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) -- a reference is one machine
  word: a pointer plus a tag in its low bits naming the storage form, never an owner plus an index
  or a path to re-evaluate, with the tag check gone wherever the form is statically known. Every
  referent LRM 13.5.2 admits is enumerated and resolves to two kinds rather than the six the prior
  warning anticipated. A generic descriptor, two pointers, a type per form, and keeping one unified
  cell are rejected. It requires the address answer to `container-element-storage`'s open fork, and
  conflicts with `../architecture/lifetime.md`'s claim that a moving collector would change no
  invariant.
- [object-model](object-model.md) -- a module / scope and an SV class are one generic nominal object
  type; an SV class handle is a managed reference via precise tracing GC.
- [object-model-storage](object-model-storage.md) -- a compilation unit owns one canonical registry
  of local nominal object declarations; identity, lexical name resolution, and backend emission
  nesting are separate relations; the lexical-tree-only storage and a second identity are rejected.
- [entering-a-class-construction](entering-a-class-construction.md) -- a class's runtime definition
  carries no constructor, because the allocation site's static type chooses one and nothing a caller
  chooses belongs on a record every object shares: the runtime allocates, and the code that wrote
  the `new` enters the constructor, so its arguments cross as themselves. A registered entry taking
  a span of arguments, a per-arity signature on the definition, and splitting in the backend are
  rejected.
- [constructing-another-units-class](constructing-another-units-class.md) -- a construction reaches
  its constructor by the identity it reaches the class's declaration by, so the declaring unit
  decides where each answer is read and nothing after it; the constructor is named the way every
  other cross-unit method is, a construction reads nothing about it, and a class states the complete
  argument list its base construction carries where its own declaration is read. Putting what a base
  construction needs on the promise, letting an empty list mean two things, filling a default from
  the declaring scope, and a call target of LIR's own are rejected.
- [type-associated-storage-is-the-declarers](type-associated-storage-is-the-declarers.md) -- storage
  a type owns rather than an object of it is brought up by whatever brings up the thing that
  replicates its declaration, never by a mechanism of its own; a cell a name can reach outside every
  body is an observable cell, every one of them takes both a declared representation and a value
  whether or not the source wrote one, and below LIR it is storage under a symbol with nothing of
  the class left. A per-class startup body, a program-startup trigger, deciding what the storage is
  from the shape of the access, and a per-class cell list at LIR are rejected.
- [dispatch-position-is-a-lineage-coordinate](dispatch-position-is-a-lineage-coordinate.md) -- a
  class states the behaviors it introduces and the ones it takes over, never its lineage's; a
  behavior is named by the declaration that introduced it plus an ordinal within it, and flattening
  a lineage into positions is a layout question answered where the whole lineage is in hand rather
  than where a call is written. The runtime answers which body a value holds and the asking code
  enters it. An absolute position assigned while lowering, a record listing every body a class
  declares, and generated code reading the class record are rejected.
- [instance-array-multiplicity](instance-array-multiplicity.md) -- an array of children is one
  member whose type is a sequence of the child pointer, carrying multiplicity but no length, so
  which element a reference names is an operand of a projection rather than part of a member's
  identity; an interface port carrying a range is the same member over a borrowed pointer. A member
  per element with the coordinate in its name, a fixed-size aggregate carrying the count, an index
  step in the place vocabulary, one published member per element, and a simulation-value container
  are rejected. Which element binds where is left open there and settled by
  [pairing-a-connection-with-a-port](pairing-a-connection-with-a-port.md).
- [pairing-a-connection-with-a-port](pairing-a-connection-with-a-port.md) -- a connection binds one
  object at each position the port stands for, paired left index to left index from the two declared
  ranges alone, so a forwarding port compiles from its own declarations however it is instantiated;
  an actual names a member and a region of it, which makes whole, part, and one element one shape.
  Binding the actual's sequence whole, taking the pairing from the front end for every actual form,
  a shared helper computing it once, and carrying the ranges beside the port's type are rejected.
- [procedural-storage-scope](procedural-storage-scope.md) -- HIR carries a lexical procedural scope
  tree (downward ownership, no backrefs) alongside its statement tree; a HIR-to-MIR two-pass
  scope-tree fold decides which named begin/ends materialize as runtime hierarchy children and where
  each static's storage physically lives; lexical owner and physical owner are distinct so an
  unnamed scope nested in a named one places its statics in the named scope's class without exposing
  them to cross-unit by-name lookup.
- [elaboration-lifecycle-phases](elaboration-lifecycle-phases.md) -- a generated constructor only
  allocates; elaboration is a staged build / resolve / initialize / activate protocol.
- [net-driver-resolution](net-driver-resolution.md) -- a net is a resolution node with node-owned
  driver contributions and capability-handle drivers, with the topology frozen at a Seal barrier;
  single-driver is N=1.
- [net-type-is-a-fold-and-a-contribution](net-type-is-a-fold-and-a-contribution.md) -- a net type
  states a fold and the contribution it makes to its own resolution, so strength decides between
  levels and the truth table within one; a per-bit strength on the resolved value, a value-only fold
  per net type, compile-time strength resolution, and the single-driver check at a barrier are
  rejected.
- [joined-nets-are-one-resolution](joined-nets-are-one-resolution.md) -- an `inout` port joins the
  nets on both sides into one resolution over pooled contributions; both sides must state the same
  net type, checked where the join happens, and re-driving the opposite side, a runtime dominance
  table, contribution migration, and a Seal-time join are rejected.
- [procedural-continuous-assignment](procedural-continuous-assignment.md) -- `assign` / `force` take
  a target over at one of two precedence levels through a call on its capability type, evaluated by
  the loop a continuous assignment already uses; a forced value is not a driver and needs no shadow
  storage, so reads cost nothing.
- [front-end-semantic-boundary](front-end-semantic-boundary.md) -- slang owns semantic resolution
  and sensitivity extraction; Lyra translates resolved facts to executable route and endpoint
  capability; sensitivity uses the correct per-consumer slang surface and never reclassifies from
  `ValueSymbol + global table + HopsTo`; reading, writing and observing one target consult the one
  translation instead of each recomputing it, so an assignment's target is validated nowhere.
- [cross-unit-class-translation](cross-unit-class-translation.md) -- AST-to-HIR splits class
  interning into a top-down `InternLocalClass` (never asks "which CU?") and a boundary
  `ResolveClassRef` (walks slang's parent chain only when a class is not already cached);
  design-wide precomputed maps and single-conflated interning are rejected.
- [class-declared-in-a-structural-scope](class-declared-in-a-structural-scope.md) -- a class a
  module, interface, or generate block declares is a type of that scope's instance (LRM 6.22, 23.9),
  so the object records which instance it belongs to and construction supplies it; the reference
  vocabulary is untouched because the hop is resolved once per body rather than once per reference.
  Putting the object in the runtime tree, reaching the scope by lexical capture, and refusing a
  construction reached through another unit's generic are rejected.
- [object-identity-is-carried-not-derived](object-identity-is-carried-not-derived.md) -- an object's
  identity is fixed when it is created and carried unchanged by every reference to it, and equality
  and null read it alone; a reference is one storage shape at every program point, because two sides
  hold one cell under different static views wherever a name resolves at elaboration, so the view
  decides which operations are available and never what the reference is. The class moves from
  declarations to uses. Keeping the fused typed owner, deriving the view from identity, a reference
  type per static view, and publishing a read and a write beside each cell are rejected.
- [structural-access-on-an-opaque-object](structural-access-on-an-opaque-object.md) -- a name
  reaching a property or a behavior through a reference whose class belongs to the instance resolves
  where the instance is known, and what crosses into the body compiled once per specialization is
  the same coordinate a referrer that could name the class would have formed, applied to whichever
  object the reference holds; resolving a virtual behavior's name names its dispatch position and
  never its body. A witness record, a lookup at each access, a specialization per endpoint class,
  and relying on the two sides' representations agreeing are rejected.
- [a-settled-access-is-ordinary-operations](a-settled-access-is-ordinary-operations.md) -- a class
  answers a name it declares no dispatch position for with the body itself, and every access whose
  class no signature publishes is written out of the operations it is -- a runtime call, a
  conversion, then a dereference or an entry -- with no alternative in any layer, because a body
  with no name for the record has no member to refer to and a callee form has one receiver where the
  operation needs two values. Reverses
  [structural-access-on-an-opaque-object](structural-access-on-an-opaque-object.md) D5, keeping its
  concern. Giving every method a dispatch position, letting each backend recover the object its own
  way, putting the handle in every entry's first parameter, and resolving the name at each access
  are rejected.
- [a-dynamic-cast-asks-the-type-or-the-object](a-dynamic-cast-asks-the-type-or-the-object.md) --
  whether an assignment the two declared types would not otherwise allow is valid for a particular
  value is answered by whoever fixes the values the destination accepts: the type where its
  declaration fixes them, and the object where the classes extending one are open across compilation
  units. The construct is a run of steps ending in the answer, the reporting spelling is those steps
  plus a report, and no semantic layer gains an alternative for it. One checked-cast node dispatched
  on the type pair, a per-class table of the subclasses a destination accepts, binding the
  destination as an output argument, and refusing a statically impossible pair are rejected.
- [unit-scope-naming](unit-scope-naming.md) -- the anonymous `$unit` scope (LRM 3.12.1) is a
  namespace unit named by its compilation-unit input identity, recomputed table-free by producer and
  consumer; a design-wide unit id, a fixed name, a collection ordinal, and a content digest are all
  rejected.
- [interface-conformance-realization](interface-conformance-realization.md) -- inherited interface
  satisfaction (LRM 8.26.2) is resolved at AST-to-HIR and realized as a synthesized forwarding
  method (backend renders, never fabricates); the full method-to-slots dispatch representation is
  deferred until a physical-vtable backend reads it.
- [generated-behavior-boundary](generated-behavior-boundary.md) -- generated behavior reaches the
  runtime through an explicit, backend-neutral per-specialization unit definition (native lifecycle
  entries + a method dispatch table + constant metadata), not a backend-language object ABI; the C++
  subclass / vtable and a per-backend adapter are rejected as the boundary. Lifecycle and SV-virtual
  dispatch share a representation but are separate concepts; the definition holds the schema, never
  instance values.
- [jit-value-realization](jit-value-realization.md) -- the JIT represents every runtime value as an
  opaque handle into the runtime library (the baseline realization), and a `GeneratedCallScope` owns
  the transient values one generated entry creates -- the JIT counterpart of C++ stack/RAII.
  Physical-layout / in-frame value lowering is a later optimization, not a correctness prerequisite;
  cross-suspension and managed-value lifetime is out of scope for the call scope. Its revisit
  condition has fired, and the ownership half of it -- invariant 6, that a handle may be aliased so
  nothing writes into a value object -- is answered by
  [storage-owns-its-value](storage-owns-its-value.md); the ABI half stands.
- [runtime-entry-naming](runtime-entry-naming.md) -- a runtime entry is named by the operation it
  performs and typed by the call that reaches it, so neither its symbol nor its signature is written
  down a second time; the symbol has one form, what the library does not realize is stated per
  builtin rather than inferred from a call, an overload set is two identities rather than one arity,
  and a check holds the prototype, the definition, and the binding to each other.
- [jit-process-suspension](jit-process-suspension.md) -- coroutine-ness is the callable's result
  type, a suspension is a generic LIR control edge whose wakeup is registered by preceding runtime
  calls, and the LLVM backend states where a body suspends while LLVM's coroutine passes derive the
  frame, resume state, and spills. The engine resumes a runtime-owned adapter, never a generated
  frame; a hand-rolled state machine in the emitter and an is-coroutine flag are rejected.
- [cross-suspension-value-storage](cross-suspension-value-storage.md) -- a value-typed non-managed
  procedural local in a suspending body is an activation-frame value: overwritten in place, owned by
  the activation (which also RAII-owns the generated coroutine), reached through a frame-held handle
  so its value outlives the per-stretch scope. Every coroutine value local that is not lent gets one
  (no liveness analysis); the cell shares a storage core with the signal cell but is not observable,
  and the access is a `ValueCellTarget` LIR call so the backend stays mechanical. Native in-frame
  layout, a backend-private arena, and a narrow liveness pass are rejected.
- [managed-value-realization](managed-value-realization.md) -- a managed value never lives in
  storage this compiler does not describe, because the coroutine frame is delegated to LLVM and its
  contents are not enumerable. Three described storages -- the static instance tree, activation
  frames, and scheduler-held closures -- are the root set; the activation frame gains the slot
  description it lacked; safepoints coincide with runtime calls generated code already makes, so no
  instruction is added for the collector. Stack maps, a shadow stack, conservative scanning, moving
  the static tree into the heap, and a second traceable-frame path are rejected.
- [activation-frame-and-transient-scope](activation-frame-and-transient-scope.md) -- naming and the
  escape invariant: `RuntimeProcess` is the lineage/scheduler node, `activation` is the control
  identity, `ActivationValueStore` is one execution's cross-suspension value storage (named a store
  rather than a frame, because the generated body already has a frame and this is not it), and
  `GeneratedCallScope` is the per-stretch transient. A transient may not escape its stretch; every
  escaping store copies/promotes (the one non-copying path, a method return, stays in the caller's
  scope). A speculative slot/trace/GC shape is rejected. The entry's own rejection of a fused
  activation record is **withdrawn**: it left the storage nowhere to live but a coroutine body's
  local, which dies one step before the frame around it, and both readings that forbid it were
  already in `lifetime.md`.
- [root-unit-elaboration](root-unit-elaboration.md) -- design elaboration is the synthetic `$root`
  unit's `construct` entry, which builds the top-level modules as its owned children; there is no
  design-level free function. Engine / bind / run stay host runner policy and never enter MIR; both
  backends' host shells collapse to creating the engine, calling the root construct, then bind /
  run.
- [member-slot-storage](member-slot-storage.md) -- a member is a logical place (a base plus a
  projection chain, named by load, store, and address-of); a unit definition declares a member
  storage schema and a generic instance owns one storage object per member. The C++ backend realizes
  a member as a native field, the execution backend as runtime-owned storage; a cell is only ever
  addressed, never read as a value. Physical in-frame layout is a later optimization, the
  member-storage counterpart of the opaque-handle value baseline.
- [inherited-member-reference](inherited-member-reference.md) -- a member projection names the
  declaration that declares the member and the slot it gave it, so which storage a shadowed name
  reaches is stated rather than re-derived from the type the chain arrived at; an inherited member
  keeps its slot, so a base's unpublished addition moves nothing. Every kind of declaration that
  declares fields is named this way, not only the class. Flattening during lowering, a base
  subobject as a place step, and a per-access base offset are rejected.
- [closure-value-realization](closure-value-realization.md) -- on the execution backend a closure
  declaration publishes a definition (its body and its capture storage schema) and a closure value
  is an instance of it, so a capture is member storage, a capture read is a member place, and
  building the value is where a captured value is copied out of the stretch that made it. The invoke
  takes its receiver uniformly; a code-address-plus-product environment and an erased callable type
  are rejected.

### Foreign-language boundary

- [dpi-foreign-boundary](dpi-foreign-boundary.md) -- DPI-C is the foreign arm of the one callable
  model: a foreign symbol is a bodyless or bodied callable the unit owns, marshaling is a cross-ABI
  carrier conversion expressed in MIR at each call, and an export's context is a thread-local
  ambient handle.
- [dpi-open-array-boundary](dpi-open-array-boundary.md) -- an open array crosses as a canonical
  boundary object owning its own storage, never as a borrow of the actual; the formal's unsized
  shape rides the ABI carrier rather than the type system.
- [foreign-code-is-linked-not-loaded](foreign-code-is-linked-not-loaded.md) -- a program has one
  linker, so a design's foreign code is an artifact that linker takes rather than an environment
  around it; a scope publishes what it answers per name space, and a foreign call that can suspend
  is an execution of the process with a value store of its own. Publishing the design's symbols to
  the system loader, handing the foreign side a pointer per export, and a second resolver for the
  inward direction are rejected.
- [a-departure-stops-at-a-foreign-frame](a-departure-stops-at-a-foreign-frame.md) -- nothing that
  ends an execution crosses a frame this compiler did not emit; the boundary answers the standard's
  disable-active int instead, the departure is derived again where control comes back, and an
  execution holding an unreturned foreign call is asked to stop rather than settled. Unwinding
  through the foreign frames, carrying the departure across, and a second departure state at the
  boundary are rejected.

### Compile-time model and specialization

- [program-facts-belong-after-compilation](program-facts-belong-after-compilation.md) -- the
  design's link-level unit reads signatures and nothing else, so what is genuinely program-wide
  moves to whoever runs after compilation: a namespace's initializers order themselves by claiming
  their one bring-up and calling what they read, a foreign symbol no unit owns is defined by every
  unit that declares it and the assembling party keeps one, the union of the foreign name space is a
  step of the build, and whether a target can realize a unit is asked as that unit is rendered.
  Keeping the per-unit record, computing the order at compile time, lazy initialization on first
  read, and a linkage that permits dropping an unreferenced definition are rejected.
- [unit-signature](unit-signature.md) -- what each unit kind publishes and how that set is known to
  be complete; a signature member is named where the referrer compiles, a name past a signature
  resolves at elaboration; the signature is an artifact separate from code, and that split decides
  what a change recompiles.
- [only-a-base-links-two-signatures](only-a-base-links-two-signatures.md) -- a unit emits its
  declarations and its bodies as two files compiled separately and linked. The declarations reach
  another unit through a pointer and so name the class without its file; a base is the one name
  needing a complete type, so it is the only edge between two units' declarations, and a cycle of
  those is refused by whoever assembles the program. Ordering one file's contents, a third
  forward-declaration artifact, and publishing a unit's cells as header definitions are rejected.
- [a-build-is-told-how-wide-to-run](a-build-is-told-how-wide-to-run.md) -- how many units a build
  compiles at once is stated by whoever invoked it and never chosen by the build, and told nothing
  it runs one at a time; each unit compiles to its own object, which concurrency requires rather
  than reuse, and every compile is attempted so every failure is reported. Defaulting to one per
  processor, a field in the design declaration, emitting a graph for a build tool, and giving a
  foreign source a schedule of its own are rejected.
- [a-precompiled-header-is-an-attempt](a-precompiled-header-is-an-attempt.md) -- making a build
  faster may not change whether it succeeds or what it produces, so a header compiled in advance is
  offered to a compile and never required by one: a compile that failed with one is run again
  without one before its output counts, and a header that turns out to have been refused is dropped
  rather than reported. That fallback is the whole guarantee, which leaves content-addressed
  currency and leaving an unchanged file alone as speed rather than correctness. Disabling the
  compiler's own validation, making the key cover what it checks, proving the header acceptable
  before every build, and recognising the compiler's complaint are rejected.
- [a-prepared-header-carries-the-work](a-prepared-header-carries-the-work.md) -- a unit pays for
  what it contains, not for what the library it uses could offer, so the templates the runtime
  surface reaches are instantiated into the header prepared in advance and no unit performs them
  again. Measured: a unit holding the umbrella header and no design code cost 0.63 s, 605 ms of it
  instantiating and 37 ms reading, and 0.11 s after. The header is therefore named by how it was
  prepared as well as by what it was prepared from. Merging a design's units into one, compiling
  several at a time to hide the cost, and header modules are rejected.
- [a-published-class-is-emitted-once](a-published-class-is-emitted-once.md) -- a class the runtime
  publishes states at least one virtual function its own source file defines, so the library owns
  that class's dispatch table and whatever its destruction reaches and a unit that builds or catches
  one produces its own class and a call. Measured: a unit adding one scope class to the shipped
  surface produced a 1,600,704 byte object with 1,503 bytes of code, and 24,016 after. Building
  optimized by default, including less of the surface, suppressing the instantiations rather than
  moving them, and reshaping what a published class holds are rejected.
- [waiting-is-an-operation](waiting-is-an-operation.md) -- the declaration both backends read names
  an operation and never one target's own protocol object, so a call that may park its caller does
  the whole operation and answers whether the caller must give up control; a body then stops to wait
  in one way, which takes what is being waited for, and an execution holds that for as long as it
  waits, which is what lets process control desensitize and resensitize it without running a
  statement. Making a wait and making it again are separate questions because the standard answers
  them separately, so a `wait (cond)` registers through a call of its own. MIR still states a
  suspension the way a source language writes it, and each backend realizes that in its own terms.
  Per-construct shells over shared bodies, decomposing the await in MIR, a suspension carrying its
  own wakeup, not desensitizing at all, re-entering the body, and a taxonomy the scheduler branches
  on are rejected.
- [reaching-past-a-published-class](reaching-past-a-published-class.md) -- a class promises what it
  declares and the class it extends, never what it inherited, so a referrer resolves an inherited
  property or behavior by walking that chain and the coordinate names where the walk landed; reading
  each promise is what records the dependency, so reaching past a class makes the introducer's unit
  a real dependency, and nothing bounds in advance which promises it may read. Flattening the
  promise at publish, resolving the chain at elaboration, a pre-computed consumed set, and
  publishing what a class keeps to itself are rejected.
- [published-member-placement](published-member-placement.md) -- a published member's position is
  its position in the signature, computed by producer and consumer and carried by neither; the
  referrer records the object it compiled against in its own IR, in a registry separate from the
  classes it compiles, so no pass below the one that consumes a signature reads one. Carrying the
  position on the reference, handing signatures to a lower pass, and a by-name lookup are rejected.
  **Its D1 and D5 are reversed by
  [a-referrer-calls-rather-than-navigates](a-referrer-calls-rather-than-navigates.md)**, which
  removes the position rather than deciding where it is computed.
- [a-referrer-calls-rather-than-navigates](a-referrer-calls-rather-than-navigates.md) -- a unit
  promises what it offers and never how it is laid out: its promise is a class with no storage and
  one behavior per published member, subroutine and entry, which the unit's object realizes, so a
  referrer calls and never navigates and what a unit kept to itself cannot move what a referrer
  compiles against. A promise carrying the published storage, an interface class, a per-member
  offset, a by-name lookup, and free entries over an opaque handle are rejected.
- [publishing-part-of-a-member](publishing-part-of-a-member.md) -- a connection point names a
  projection of a published member, a folded value, or nothing; a signature carries a closed
  selector path and never an expression, an interface publishes its modports, and the referrer
  applies the projection as an ordinary access so no endpoint category is added. A synthesized cell
  per point, carrying the source expression, reading it off the frontend, a member per point, and a
  per-modport member list are rejected. The write half of its D5, which published a name a view
  offers as a subroutine assigning to it, is superseded by
  [names-a-view-offers](names-a-view-offers.md).
- [names-a-view-offers](names-a-view-offers.md) -- an identifier a modport did not rename is the
  interface item and nothing about the view crosses; one it renamed is the storage it designates
  where the view admits a write, and the subroutine evaluating it where it does not, so every
  assignment form the language allows reaches it with no arm per form. Defining each failing form in
  terms of a subroutine pair, publishing both representations, reading the internal symbol off the
  frontend, and flattening the interface are rejected.
- [publishing-an-owned-instance](publishing-an-owned-instance.md) -- an interface publishes the
  interfaces it instantiates, so a name continues past a port into one; continuing through a
  published member is the step form of ending on one, and every route to a published name becomes
  typed at once. Carrying the inner unit's members inline, recording its object eagerly, walking
  bodies to bound the read set, and letting the reach fall to a by-name lookup are rejected. Its D3,
  bounding what a lowering may read, is superseded by
  [reaching-past-a-published-class](reaching-past-a-published-class.md).
- [identity-is-not-a-rendering](identity-is-not-a-rendering.md) -- what must distinguish is stored
  as its parts and composed into a name only by whoever knows the spelling rules; an identity splits
  exactly where the layer below it splits; naming another unit's object and holding what it
  published are two facts with two vocabulary items. Mangling at the composition site, tagging one
  conflated arm, and recording every transitively reachable unit are rejected.
- [a-name-is-a-relation-not-an-identity](a-name-is-a-relation-not-an-identity.md) -- a callable
  carries no name, because being reachable by one is a relation the name space holds; a program-wide
  symbol is self-delimiting parts under a category rather than names joined by a separator; what a
  declaration is called in a target is that target's to mint, into a range no source name reaches;
  and an entry another unit must reach but the source never named is reached by which entry it is.
  Reserving a separator character, reserving a minted word, length-prefixing without a category,
  naming every synthesized declaration anyway, and hashing the parts are rejected.
- [calling-a-subroutine-on-another-units-object](calling-a-subroutine-on-another-units-object.md) --
  an interface publishes its subroutines, and enabling one is a route that ends at the object plus a
  name resolved against what that unit promised, so no sealed-endpoint category for a callable is
  needed; whether such a method takes a receiver is its declaration, carried down rather than
  re-derived below. Naming the instance the frontend resolved to, a callable endpoint category, and
  a by-name lookup for a published name are rejected. How that fact is carried is superseded by
  [call-receiver-on-the-callee](call-receiver-on-the-callee.md).
- [interface-port-binding](interface-port-binding.md) -- an interface port's declared type names the
  unit whose instance belongs there, by name, so it crosses a signature; the member holds a borrowed
  reference the parent binds once during elaboration, a fourth published storage kind; an interface
  publishes every net and variable it declares; and which interface a port carries feeds the
  module's specialization identity, without which two differently bound modules collide on one name.
- [parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) -- every
  parameter is treated as code-shape-affecting for now (conservative over-approximation);
  classification and constructor-input threading are deferred.
- [generate-variable-specialization](generate-variable-specialization.md) -- a generate variable is
  a specialization input like a parameter; demoting it to a runtime input is a proof-gated
  optimization. Its conservative default is superseded on the loop axis by the next entry.
- [one-body-built-at-every-index](one-body-built-at-every-index.md) -- the loop axis of that record,
  with its default inverted: a loop generate's blocks are one compiled class the constructor builds
  once per index, because the index reaches the block as a value construction supplies rather than
  as a constant folded into it. Read it for the shape that makes that safe -- one mechanism decides
  which blocks qualify and a second proves the decision by lowering the rest and comparing, so a
  hole in the first costs a build rather than a wrong program -- and for why a question asked of the
  source can never be completed. The measurement that removed speed from the argument is there too,
  as is what a scope's construction receives: one entry for every class, because the constructing
  site may hold only the definition.
- [an-elaboration-time-value-is-an-input](an-elaboration-time-value-is-an-input.md) -- the rule the
  entry above turned out to be one case of. The front end evaluates whatever it can, so an already
  computed answer sits beside nearly every expression, and taking one decides where that value
  enters the artifact rather than what the program means. No position a body states is read for its
  settled value, with no exception for one the standard fixes: a clause requiring a constant says
  the value is known, never that the artifact has to hold it, and the question that actually decides
  is whether a different value there would be a different class or the same class holding a
  different number. Read it also for why the check enforcing that is a proxy -- it matches spellings
  of taking the answer, and a parameter's own accessor is a third one that walked past it for as
  long as the entry claimed the list was complete. Read it before adding any site that reads a
  folded value, for why a width passes the citation test and still is not a class, and for the
  entry's own reversal -- it once filed a sampled value's depth as class-level on a sentence that
  was true in both halves and wrong in its conclusion.
- [a-structural-expression-may-write](a-structural-expression-may-write.md) -- an expression a
  construction evaluates may write, and both lowering boundaries used to refuse one on the stated
  grounds that the language admits none outside a procedure. LRM 27.4 gives a loop generate's step
  three forms and every one of them writes. What is really procedural is deferring an update to a
  later region, which sits above a store both positions share. Read it before writing anything that
  treats a constructor-time expression as read-only, and for why a loop's step is carried as written
  rather than read for the value it names next.

### Runtime execution and scheduling

- [static-initializer-draws-from-its-container](static-initializer-draws-from-its-container.md) -- a
  randomization call draws from the generator installed for whatever is running, and a static
  initialization installs one just as a process does, seeded from the container the standard names
  rather than from whatever process happens to be executing.
- [dpi-context-scope-is-an-extent](dpi-context-scope-is-an-extent.md) -- the scope a `context`
  import makes current is stated as an entering call and a cleanup that gives it back on every way
  out, never as a value whose destructor does it, and the chain it pushes onto belongs to whatever
  is running rather than to a process.
- [owner-transition-and-observation](owner-transition-and-observation.md) -- every mutation of
  observable storage reports one thing, whether the owner transitioned, and publication is a
  function of that alone; forming a designation is itself such a mutation; an event control's
  detection belongs to the armed observation, which compares its own expression against the baseline
  it took when it armed. Information about what a write could have affected may only eliminate
  candidate observations, never decide one.
- [activation-registration](activation-registration.md) -- an activation's membership in a wake
  target (an observable, an event, a join condition, a region queue, a delay slot) is one record the
  activation owns and the target merely links; the activation-side set and the target-side list are
  two indexes over it, revoking is a detach rather than a search, and the two-authoritative-copies
  shape is rejected.
- [activation-disposition](activation-disposition.md) -- an activation has one authoritative
  disposition (Executing / Runnable / Blocked / Suspended(saved) / Terminal); a wait is a retainable
  pending capability distinct from its registration (enrollment), supplied uniformly by each
  construct; suspension saves the prior disposition; a central wait-kind taxonomy,
  `Runnable(region)`, and mirroring the wait's state are rejected.
- [disable-scope-invalidation](disable-scope-invalidation.md) -- `disable` (LRM 9.6.2) invalidates a
  cancellation source's generation; every affected execution reconciles at one uniform validity gate
  before its next statement, reusing the registration entitlement substrate. Membership in a target
  is carried by the running process, so it spans a call, and is captured at a spawn. A local goto, a
  per-thread extent frontier carried in a `DisableUnwind` exception, an explicit resume-reason, a
  dedicated entitlement object, and membership rebuilt per callable from lexical scope are rejected.
- [run-time-failure-is-not-an-outcome](run-time-failure-is-not-an-outcome.md) -- an activation's
  terminal outcome carries only what the source language can consume, which is the produced value
  and the departure a region lands; SystemVerilog has no spelling for a failure, so a failure is the
  tool speaking and not an outcome. Failures are told apart by who acts on them, not by when they
  surface: an unsupported construct is a lowering's answer whenever it is found, a design's run-time
  error is a severity report whose level the tool chooses (LRM 20.10 admits one that does not end
  the run), and an internal inconsistency does not borrow the language's ending. Ending the run
  walks `$finish`'s tail so `final` procedures execute (9.2.3). Reverses the third alternative of
  `activation.md` invariant 2; the awaiter-side delivery every async runtime uses, a Rust-style
  error value, and a check after every fallible operation are rejected.
- [ending-a-run](ending-a-run.md) -- a report, a stop, and an unclaimable departure are three
  primitives, and every ending composes them; a design's run-time error is a report Lyra writes for
  the design rather than a failure crossing the engine. `final` procedures run when the simulation
  reached its end -- exhaustion, the design asking, or such an error -- and not when the tool could
  not carry on, while the cover report, the output drain and the exit status are owed either way.
  `$stop` ends the run where nothing can resume it and the Table 20-1 diagnostic is the whole of
  what separates it from `$finish`; that diagnostic prints where the task executes and carries no
  severity. The simulation's boundary follows the elaboration phases, so a time-zero initializer's
  error is a run-time error rather than an escape. Completes the reversal of `activation.md`
  invariant 2's third alternative. Catching the throw at the engine loop, skipping `final` after
  `$stop`, `$stop` as a failure, a fifth severity, and aborting at the raise site are rejected.
- [deferred-report-queue](deferred-report-queue.md) -- a pending report stays in the region's own
  list and withdrawal is a refusal to act, so it records the sources that can withdraw it and acts
  only if all still stand: two `disable` targets through the accepted cancellation-source
  generation, and the creating process's execution pass as a liveness token, because a source
  outlives a report and a process does not. Maturity discards the record, which is the one-way
  latch; what each construct records is how one queue serves LRM 16.4 and 12.4.2.1 with different
  flush-point sets. A per-process container the engine enumerates, a single per-process token (which
  cannot express `disable` of one assertion), and a notify-style cancellation token are rejected.
- [sampled-value-and-its-clock](sampled-value-and-its-clock.md) -- what a sampled value function
  reads is retained by the cell at the transition boundary every mutation already reports through,
  armed per instance at Activate, so cost follows changes rather than time slots and the time-zero
  rule falls out of the value the cell is armed with. The clocking event is the trigger set a
  value-change wait already builds and the front end resolves which one it is, so LRM 16.9.3's
  ordering and 16.14.6's conditions are not restated here. The sampler is a synthesized process
  because a history must record every tick, not only the ticks something waited for, and it commits
  in Postponed so that "strictly prior" holds by construction rather than by ordering. A Preponed
  scan, per-leaf history with replay, a second implementation of the clock inference, a node kind
  for the sampled read, and an armed observation are rejected.
- [concurrent-assertion-evaluation](concurrent-assertion-evaluation.md) -- derived from Annex F's
  formal semantics. One start rule covers both placements: an attempt begins where the clock ticks
  and the enabling condition holds, which is 1 for a declarative assertion and "control reached this
  statement" for a procedural one, so where an assertion sits in HIR follows from whether it has a
  condition to record rather than from the grammar. Three levels, and what may be pooled at each is
  decided by the quantifier over it: an evaluation is a set of positions in an automaton the
  HIR-to-MIR lowering builds, pooling because a sequence's operators are existential; an attempt is
  a list of evaluations that never pool because implication quantifies universally over match
  points; an assertion holds attempts that never pool because each reports its own result. A finite
  trace admits four answers, so an attempt reports when the trace settles it and is otherwise
  pending, and weak versus strong is which answer the statement demands. A synthesized process per
  assertion and clocking event submits each tick's advance to Observed and each resolved attempt's
  action to Reactive. A coroutine per attempt, one merged position set for the whole assertion, an
  interpreted transition table, building the automaton at AST-to-HIR, a MIR node kind for a
  sequence, and evaluating where the process wakes are rejected.

### Diagnostics

- [diagnostic-construction](diagnostic-construction.md) -- a diagnostic's kind is derived from its
  code at construction; construction is infallible; the `UnsupportedCategory` axis is removed.
- [qualified-statement-violation-check](qualified-statement-violation-check.md) -- `unique` /
  `unique0` / `priority` state two independent assertions, uniqueness and totality; an explicit
  `else` or `default` discharges totality, and what the live assertions need decides what the
  statement evaluates, so a `priority` carrying a catch-all lowers as the unqualified statement.

### Compiler inputs

- [project-file](project-file.md) -- `lyra.toml` is a manifest of the design, carrying what is true
  of it for everyone who builds it and never an invocation or machine property; material accumulates
  and selection is replaced, a path resolves against its own manifest, and naming sources on the
  command line uses no manifest at all. A project mode, a merged cascade, and a flag to suppress
  discovery are rejected.

### Conformance testing

- [conformance-case-shape](conformance-case-shape.md) -- a case is a self-checking SystemVerilog
  program indexed by LRM clause; it names no path, states no expected variable, and carries no
  manifest, and what a path cannot do is recorded once per path.
- [conformance-diagnostic-claims](conformance-diagnostic-claims.md) -- a requirement whose whole
  observable is a report is stated as a directive and written in both directions; a claim of silence
  is the run writing nothing rather than the absence of one wording, and golden output stays
  reserved for cases whose subject is the output channel.

### Measurement

- [benchmark-case-shape](benchmark-case-shape.md) -- a case fixes the shape of its work and takes
  the amount as a runtime argument, so the harness picks the amount and reports a rate; no iteration
  count is written down anywhere.

## File Naming

`kebab-case.md`. The name describes the decision, not when it was made; the date lives inside the
file. Existing example: `integral-representation.md`.

## Shape

There is no fixed template. The existing `integral-representation.md` is the reference for shape:
title, date, status, the model or findings that shaped the decision, the decision itself, and the
consequences that follow. Let the subject drive the structure; a decision with no rejected
alternative or no load-bearing invariant probably should not be a decision entry.
