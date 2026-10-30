# A rewrite of MIR says whether a program needs it

Date: 2026-10-18. Status: accepted; D1 and D3 revised 2026-10-26; why the conversions are MIR's
re-derived 2026-10-30. Revises the stage `a-process-stopping-only-at-its-loop-head-is-called.md`
added.

The 2026-10-26 revision: a policy stated on each rewrite let a required rewrite and the phase it
brings the unit into be two facts. Required is now where a rewrite stands -- inside a conversion
into a phase -- and the unit records its phase, so every backend refuses one that is not legalized.
A third phase, between the two, held a unit once MIR's generic definitions were made concrete; it
went with them
([an-operation-over-many-types-is-built-by-the-lowering](an-operation-over-many-types-is-built-by-the-lowering.md)).

## The requirement

> Between stating what a unit means and handing it to every backend, the unit passes through an
> ordered list of rewrites, each stating whether the program needs it, each skippable when it does
> not, and the unit's form at every point between them is one a check can name and test.

The stage had three rewrites, composed as one nested call. Each walked every body of the unit by
itself, and the three walks had already drifted apart: one left out a record's methods and a type's
operations, one carried a constructor's base arguments and one dropped them when a body was rebuilt.
None could be skipped, and nothing recorded which a correct program needs. The verification held a
unit to a fact only the last required rewrite establishes -- no frame holds a value too large for
one -- so it could run only once, at the end, and the form HIR-to-MIR states was never checked on
its own. A rewrite could not build an operation over constants, because the builders that fold one
belonged to HIR-to-MIR.

## Decisions

**D1. What a correct program needs is a conversion into a phase, and every other rewrite is
optional.** A conversion is the rewrites that bring a unit from one phase into the next (D3), and a
later stage reads the form it leaves, so no build skips it -- as Swift's mandatory pipeline is what
commits a SIL stage, and an MLIR lowering is required where the next stage refuses what it leaves;
one conversion may take several rewrites. A rewrite a correct program needs that brought the unit
into no phase would leave a form no check names, which the requirement above refuses. Making the
order an evaluation needs explicit, and then deciding where each value lies -- a value too large for
a frame, or one the frame has no room left for, held through a box -- bring a unit into the
legalized phase. Every other rewrite runs inside a phase and leaves the unit in it: skipping it
leaves the same program at another cost, so a build may. Replacing a whole select with the whole,
and calling a process that stops only at its loop head, are such rewrites. That a correct program
does not depend on any optional rewrite is then a property a run can show, by skipping all of them.
A required rewrite is no more than its place in the pipeline says; nothing states it a second time.

**D2. The rewrites are one ordered list, stated once.** The stage runs the list; a rewrite is a
function from a unit to a unit, named as its function is. Every optional rewrite runs unless a build
skips it by name, in every build, since none is dear enough to keep out of the default build and the
merge gate builds only the default way: a rewrite only an optimized build ran would be one nothing
tests. Where a rewrite dear enough to keep out of the default build appears, the level a build asks
for is what decides whether it runs.

**D3. A unit's phase is named, the unit records it, and a check asks of a unit what its phase
requires.** Each phase is named by the conversion that brings a unit there, as LLVM names a machine
function's `Legalized` and `Selected` properties: built, as HIR-to-MIR states it; legalized, with
the order an evaluation needs explicit and no frame holding a value too large for one or more than a
frame keeps together, which is the form every backend reads. Only the pipeline moves a unit from one
to the next, and every backend refuses a unit that is not legalized. The unit is checked once as it
leaves the stage, unless the command only shows it to a person, and, where a build asks, as it
arrives, after every optional rewrite and on reaching each phase, so a defect is reported against
what made it; between the rewrites of one conversion a unit is in neither phase, as rustc validates
at a phase change and Swift commits a SIL stage only once its pipeline is done.

**D4. Which code a unit holds is stated once, beside the unit.** Every code -- a constructor with
the nodes it enters its base's constructor with, a class's and the unit's callables, a record's
methods, a type's operations, a closure's body, an exported entry -- is at one named source, and a
rewrite local to a body says only how it rewrites one, answering nothing where it changes nothing.
Every code is asked of the unit as it was before any was replaced.

**D5. A rewrite builds through MIR's builders.** The builders that fold an operation over constants
are MIR's, so a rewrite and the lowering build the same node for the same operation.

**D6. A value handed to something of a type it states is of that type.** An assignment hands its
target's type, a whole write through a wrapper or a reference to bits the type held or reached, a
local's initializer the local's, a return what the body answers, an arm of a choice and the value a
block ends with the node's own, a capture its field's, an argument of a callable the unit declares
its parameter's, and that callable its call the type it returns -- the relations LLVM's verifier
holds every operand to, and what LRM 10.8 calls assignment-like contexts. A value only alike in
representation -- a signed vector's whole select, a reference taken where a `const ref` formal binds
it -- is held as the other type by a conversion the program states, as clang states a qualification
conversion and rustc a reborrow, never handed over as it is. Without this a rewrite that dropped the
conversion a write needs produced a unit every check accepted, since the bits came out the same.

**D7. A cast joins the pairs every target writes in its own conversion syntax, and MIR says which.**
A cast is a value read as another type, with the value unchanged or read as the machine boolean a
condition tests (LRM 12.4): an integral value at a type laying it out alike, a machine integer at
another, a pointer at another pointee and a code address at another signature, and a reference at
the same referent made read-only. The verification refuses every other pair, so a backend translates
each -- C++ in cast notation, LLVM by a cast instruction -- and has nothing to decide or refuse. A
handle seen as another class's is not one of them, since only the object's layout answers it: it is
a call, toward a base (LRM 8.14) or the checked one `$cast` asks for (LRM 8.16). Two arrays of
equivalent types are not one either, because they are one type: the declared range is a coordinate
system a select resolves where it is lowered (LRM 6.22.2, "the actual range bounds may differ"), so
MIR's array type is an element type and a count, as LLVM's is.

## What the field does

- **rustc** (`compiler/rustc_mir_transform/src/pass_manager.rs`): a `MirPass` has a name taken from
  its type and a `PassPolicy`, `Required` ("cannot be disabled via any means") or `Optional`. A
  required pass need not change the phase: it implements "parts of the MIR semantics" or brings MIR
  "into a shape that is easier to deal with" for what follows, and several run where no phase
  changes (`AbortUnwindingCalls`, `AddCallGuards`, `MentionedItems`); `-Zmir-enable-passes=+A,-B`
  overrides an optional one and an unknown name is reported with the valid ones. `run_passes` gives
  each pass a profiler span and validates after each only under `-Zvalidate-mir`, always at the
  change to the final phase. `MirPhase` (`rustc_middle/src/mir/syntax.rs`) names the dialects MIR
  narrows through, and the validator branches on the body's phase. `InstSimplify` is enabled at the
  debug level too.
- **LLVM's new pass manager** (`llvm/include/llvm/IR/PassManager.h`): `RequiredPassInfoMixin` and
  `OptionalPassInfoMixin`; `-O0` still runs the required passes; `-verify-each` adds a verifier
  after every pass; a function pass reaches every function through an adaptor. One IR form, so the
  verifier has no phase.
- **clang** rewrites its tree through `TreeTransform` (`clang/lib/Sema/TreeTransform.h`), which
  rebuilds each node through Sema's `Build*`: a tree rewrite builds anew, through the builder the
  front end uses.
- **MLIR** (`mlir/lib/Pass/Pass.cpp`) verifies after every pass by default, and has no phase: what a
  stage may hold is the legality one conversion states (`ConversionTarget`, `applyFullConversion`),
  and a required lowering is one whose leftovers the next stage refuses.
- **Swift** names SIL's stages Raw, Canonical and Lowered (`SILStage`, in the Swift repository's
  `SILModule.h`), each by what it may hold, and its driver commits a stage only once the mandatory
  pipeline that reaches it has run (`runSILDiagnosticPasses`, `runSILLoweringPasses`); the
  performance pipeline is the part a build may skip.
- **Casts.** clang's Sema decides each `CastExpr`'s kind
  (`clang/include/clang/AST/OperationKinds.def`) and `CastExpr::CastConsistency`
  (`clang/lib/AST/Expr.cpp`) holds the kind to the types, so CodeGen meets no kind it cannot emit
  (`llvm_unreachable` in `CGExprScalar.cpp`). rustc's validator
  (`compiler/rustc_mir_transform/src/validate.rs`, `Rvalue::Cast`) checks every cast kind against
  its types, and codegen refuses none. Both decide which conversions exist once, upstream of every
  backend.

Where Lyra differs: rustc's required conversions run at the control-flow level, the layer below the
one its THIR sits at, and clang rewrites nothing after Sema; here they are MIR's, the layer clang's
AST and rustc's THIR correspond to, because one consumer writes MIR out as C++, whose own rules
would otherwise decide the order operands run in and where a value lies, so a correct program needs
those settled before either consumer reads it. Re-derived 2026-10-30 on whether every rewrite should
instead be one a build may skip, as LLVM's are: that would make the order and placement each
backend's own, which is the disagreement the conversions exist to prevent. MIR is a tree whose nodes
are each reached once, so a rewrite builds the body that replaces one rather than editing it in
place, as clang's does. Where rustc states a policy on each pass, a required one here is one inside
a conversion, because each form between rewrites has to be one a check names, and rustc's validator
names only its phases. rustc calls its backend-facing dialect runtime MIR; here "runtime" names the
library a program links, so each phase is named by the conversion that reaches it instead.

## Rejected

- **Each rewrite walking the unit itself.** Three walks over one set had already disagreed twice,
  and a fourth, the verification's, covered what none of them did.
- **Optional rewrites only in an optimized build.** None is dear enough, and the gate would never
  run them.
- **A cast kind every backend switches on.** Derived once in MIR it is no second copy, but a switch
  per backend is still a table of operations in each, and the arms a target cannot write in its own
  syntax -- a handle's view, a value moved between representations -- were calls all along.
- **Checking after every rewrite in every build.** A unit leaves the stage in the one form the
  backends read, and a check of that form is what every consumer depends on; checking each rewrite
  is asked for where a defect has to be placed, and the conformance gate asks for it on every case.
  Checking on reaching each phase in every build is refused for the same reason: the final check is
  of the last phase, which is the one every consumer reads.
- **A policy stated on each rewrite** (this record's first D1, and rustc's shape). It stated beside
  the pipeline what the pipeline's shape already said, and the two could disagree: a required
  rewrite outside any conversion, which leaves a form no check names, or an optional one that
  narrowed a phase.

## Consequences

- `--mir-enable-passes` and `--mir-verify-each` are options of every command that takes a design to
  MIR. Lowering is never kept, and what it produces keys every kept object, so neither needs a key
  of its own.
- A whole write of a signed vector by its whole select is the vector's own write; the conversion it
  needs is folded where the written value is a constant.
- Each rewrite is a span of `--time-trace`.
- A `ref` or `const ref` actual is converted to the formal's reference type (LRM 13.5.2).
- Neither backend refuses a cast, and the C++ backend writes every one in cast notation.
- MIR no longer has a node for a checked handle conversion; `$cast` of a class handle calls the
  library's entry for it.

## Cross-references

- `a-process-stopping-only-at-its-loop-head-is-called.md` -- the stage and its first rewrite.
- `a-value-too-large-for-a-frame-is-held-through-a-box.md` -- the two rewrites of the conversion
  into the legalized phase.
- `an-evaluation-runs-its-operands-first-to-last.md` -- the order the first of them makes explicit.
- `an-operation-over-many-types-is-built-by-the-lowering.md` -- why every body MIR holds is
  concrete, so no conversion makes one so.
- `a-select-names-a-position.md` -- what a deferred write restates rather than freezes.
- `unpacked-range-belongs-to-type.md` -- the range as the static type's, which at MIR is HIR's.
