# A descent into a value is calls the lowering names, one per level

## Date

2026-09-09

## Status

Accepted

## Why this decision matters

Reaching part of a value reached MIR two ways at once. A product component and a union member were
node kinds; a container element and a slice were calls. A consumer meeting a descent therefore had
to classify each level before it could say what the level meant, and both backends did that
classification separately from the same information -- the same question answered in two places,
which is the shape `mir.md` names.

The write side was worse. A write target arrived as a nested expression, and the layer below
recovered the owner by walking that expression back until a step stopped being one. That walk is the
first rejected alternative of [value-projection-write](value-projection-write.md), still in place:
the boundary was found rather than stated, and two consumers can walk to different answers.

This entry settles where a descent step's operation is decided and what MIR carries as a result.

**It settles nothing about what an interior write does.** That is
[value-projection-write](value-projection-write.md) D2 -- a functional whole-value update through
the owner -- and [owner-transition-and-observation](owner-transition-and-observation.md), which
fixes how a partial write reports its owner's transition and what it may compare. Both stand
unchanged, and a target that can be written where it stands still is. The two questions are
separate: one is which operation a step is, the other is how a target realizes it.

## What established compilers do about it

The question is where a semantic decision is made, and the answer is the same everywhere it has been
asked.

- **Clang** resolves a member access, an overloaded operator, and every implicit conversion in Sema.
  CodeGen consumes an AST in which those are already decided and performs no lookup of its own; a
  `MemberExpr` names the resolved `FieldDecl`, not a name to be resolved again.
- **Rust** builds MIR after type checking, so every projection in a `Place` is already typed.
  Codegen reads the projection and never re-runs inference to find out what a field access meant.
- **LLVM** puts an aggregate index on the instruction as a constant -- `extractvalue`'s and
  `insertvalue`'s indices are not operands and not derived by a target. The target spells what it
  was handed.
- **Swift's SIL** is explicit about the same split: the type checker picks between a stored-property
  access and an accessor call, and SIL carries the choice as a different instruction, so IRGen
  chooses nothing.

The rule is one rule: the layer that holds the types decides the operation, and every layer below
spells what it was given. Our conditions differ in only one way, and it does not touch that rule --
we have two backends whose value representations differ, so one operation may be _realized_ two
ways. Which operation it is stays one answer.

## The decision

### D1. Every level of a descent is a call, and which entry it calls is named at HIR-to-MIR

Reaching a product's component, an active-member value's member, a container's element and a value's
slice are four calls, not two calls and two node kinds. HIR-to-MIR holds the type of the value each
level descends into, so it names the entry; no layer below asks what a level meant.

No MIR node names a part. What was `FieldAccessExpr`'s component arm, `UnionMemberExpr`,
`TaggedIsExpr` and `UnionExpr` is an entry each, and a consumer meets an ordinary call.

### D2. Two entries per level, one answering with the part's value and one with the part

Which of the two a source position calls is settled where the source is read, because a consumer
that had to work it out from the position could work it out differently. Composition is the
receiver: a call whose receiver is another call is the descent, so nothing carries a path and a
descent of any depth is these entries applied one per level.

### D3. A part's static position rides on the callee, never among the operands

A component index names the part rather than being a value handed to the operation, and the part's
type is a function of it -- which is why every typed IR requires it to be static. So it travels
where the entry's own identity travels.

The tell that this is the right placement is arithmetic. A fact placed among the operands that does
not belong there costs one special case at every site that walks operands, and every layer walks
operands. Moving it to the callee removed three such cases and added none.

### D4. A write target is built as a descent, so no consumer recovers an owner

The lowering that peels a write target holds the descent as its own structure: one step per level of
the source's nesting, each naming the entry that reads the part, the entry that reaches it, the
operands both take, and the part's type. It builds the target rooted where the write lands, rather
than building it rooted at the source's spelling and then walking back to re-root it.

That structure is the lowering's alone and reaches no layer below, because a consumer that met it
would have to decide which operation each step is. What MIR carries is what it lowers to: the
owner's place, one reaching call per level, and one store.

### D5. A target that must realize a reaching call differently reads that from the entry's declaration

A value crosses to the execution backend as an opaque handle
([jit-value-realization](jit-value-realization.md) invariant 6), so a call that answers with the
part has nothing there to answer with, and that target realizes the write as a read of the whole, a
rebuild, and a store back. That it must is a property of the target's representation, not a fact
about which entries exist -- so the entry declares that it answers with the part, and the target
reads it there instead of keeping its own list of which entries are.

This is what separates a realization from a decision. Both targets are handed the same operation;
neither chooses which operation it is.

### D6. One selector below MIR covers a positional part, whatever the value's family

LIR named a product component and a union member with separate selectors. They differ in what the
value does about the access -- whether every part coexists, and whether a write settles which member
is live -- and that follows from the aggregate's own type, exactly as the entry realizing a
coordinate step does. So they are one selector, the way one selector already spans an unpacked
array, a queue and an associative array.

## Decisions this reverses

- [value-projection-designator](value-projection-designator.md) D1 and D2. The designator node was
  never built and is not the answer: a closed selector set below the lowering is a vocabulary every
  consumer must interpret, which is the defect under another name. The descent lives in the lowering
  and MIR carries calls. Its D5 asymmetry falls with it -- the read chain and the write chain are
  the same vocabulary, differing in which of D2's two entries each level names.
- [unpacked-union-representation](unpacked-union-representation.md) D4's union-specific reference
  concept, already reversed in mechanism by that entry and now in fact: a member is reached by the
  same pair of entries a product component is, and which value domain realizes them is what differs.
- [queue-operators](queue-operators.md) D2's lowering flag, under which an element select picked a
  write-side method. There is no flag: the two entries are named outright.

## Rejected alternatives

- **One entry read two ways, with the position deciding.** Rejected: the position is exactly what a
  consumer would have to interpret, and two consumers can interpret it differently. This is the
  defect the entry exists to remove, moved one level down.
- **A designator node carrying a selector path.** Rejected above, and by its own argument failing:
  it was introduced because a write's innermost step cannot name the root place, which is true of a
  bottom-up composition and false of a descent built from the owner outward.
- **Letting each backend classify a step from its receiver's type.** Rejected: it is the two-places
  answer with extra steps. The type is known where the source is read, and that is where the answer
  is written down.
- **Keeping the product and union selectors separate below MIR.** Rejected: with one entry above, a
  split below forces the lowering between them to choose from the receiver's type -- a decision at
  the layer with the least information, which is what D1 is for.

## Consequences

- A new value family adds its entries and its domain, and touches no consumer. Nothing switches on a
  node kind to find out what an access is.
- The MIR-to-LIR lowering loses the walk that recovered an owner and the per-node-kind
  classification beside it; what replaces the classification is one question of the shared entry
  declaration.
- The C++ backend's value-emission entries name no runtime library identifier at all, which
  `tools/policy/check_render_names.py` checks. Every name they write comes from type mapping, place
  access, or the entry declaration.
- A `ref` bound to a value interior is still refused on the execution backend, unchanged by this
  entry: what it needs is a live alias into a value, which no representation there provides.
