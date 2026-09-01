# LIR type pool is a structural-equality interner

Date: 2026-08-27 Status: accepted

## Context

A LIR type is structural: it names the logical shape a value has during execution, and two shapes
that look the same are the same shape. Its consumers already read it that way -- the verifier
decides a store by comparing its value's type identity with its place's, and codegen keys a target
type by identity -- so identity equality is being used as type equality throughout.

The pool did not provide that. It was a plain append-only arena: every construction minted a fresh
`TypeId`, so one type reached by two routes arrived as two identities. What follows from that is not
a latent tidiness problem but two live defects, and they look nothing alike:

- A whole-value store between two unpacked arrays whose declared ranges differ is position-wise (LRM
  7.6), and the declared range deliberately does not travel below MIR -- so both sides are the same
  LIR type. They arrived as unequal identities, and the store was refused by name.
- A reference names the cell its referent lives in, and that cell type is built both by translating
  a MIR observable type and by the lowering itself, for a local whose storage is lent. The two
  arrived as unequal identities, and the address-of failed its own verification.

The second was patched by keying the cell and the reference through hand-written caches on the
lowering, which is [mir-type-interning](mir-type-interning.md)'s rejected alternative -- a plain
arena plus manual caches -- reintroduced one layer down. That is what identifies the shared cause: a
concept that is missing does not announce itself once, it appears once per site that needs it.

MIR and HIR both settled this already. LIR was the layer that had not.

## Decision

**The LIR type pool is a type interner: arena storage for the canonical representatives plus a
content-keyed index that maps each type to one canonical `TypeId`.** Interning is how a type is
obtained; raw append is not.

1. **A LIR type's identity is derived from its content**, so two requests for one content return one
   `TypeId` and a `TypeId` comparison is a type comparison. A caller cannot tell whether its request
   was the first.

2. **The key is the type's whole content, and the type's own equality is that key.** Every LIR type
   compares by its members, so the equality the pool interns by is the one a reader would write.
   There is no exclusion list, which is a property of the layer rather than an accident: LIR carries
   no source-language concept, so it holds nothing that is present for the front end's benefit and
   absent from the type's meaning.

3. **The one field that did not meet that description is deleted rather than excluded.** A packed
   array carried the syntactic form it was declared in -- `int` against `bit signed [31:0]`. Nothing
   below MIR-to-LIR ever read it: not the code generator, not the runtime ABI, not the dump. A field
   with no reader is not a key question.

4. **A nominal type keeps its declaration identity as its content.** An object type is its class
   identity, and two classes with identical members stay distinct types, exactly as MIR's are.

## Consequences

- The unpacked-array store refusal is gone: the two sides now arrive as one type, and the store
  lowers. That refusal was also the only thing keeping a container's slice write away from a runtime
  entry nobody defines, so the operation is now refused where it is actually missing -- a stated
  diagnostic instead of a link failure, which is what a gap on this backend owes its reader.
- The lowering's hand-written caches for the borrowed pointer, the machine boolean, the void type,
  the cell, and the reference are gone. Each is a one-line interning call, and none of them is a
  second authority for an identity the pool already answers to.
- MIR-to-LIR keeps its memo from MIR type identity to LIR type identity. That is a cache of a
  translation, not of an identity: it saves re-walking a MIR type, and interning would give the same
  answer without it.
- A LIR dump's type numbering is denser and no longer records the order types were requested in.
- The index holds a copy of each type's content beside the arena's, which is the cost of a key.

## Rejected

- **Keying only the types that a caller happens to build twice.** The shape this replaces, arrived
  at by patching one site. It makes the pool's answer depend on which route a caller took, leaves
  every other type duplicated, and puts the authority for an identity in a cache beside the pool
  that owns it.
- **Comparing types structurally at each consumer instead of interning.** Every consumer would carry
  the comparison, the verifier's identity check would have to become a deep one, and a consumer that
  forgot would be wrong in a way the others hide.

## Cross-references

- [mir-type-interning](mir-type-interning.md) -- the same decision for MIR, and the source of the
  rejected alternative this repeats.
- [hir-type-interning](hir-type-interning.md) -- the same for HIR.
- [reference-binds-a-cell](reference-binds-a-cell.md) -- the consumer that needed one identity for a
  cell built two ways.
- `architecture/lowering_organization.md` -- conferred against derived identity, and why the two
  admit opposite treatment.
