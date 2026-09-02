# A member is named by the declaration that declares it

Date: 2026-09-02 Status: accepted

## Context

A class inherits its base's members, and may declare a member of its own with the same name. IEEE
1800 states both storages exist at once and that an access names one of them by a fact known where
the access is written, never by the object's dynamic type: with `Packet p = lp;` where `lp` holds a
`LinkedPacket` that redeclares `i`, `p.i` reads the `Packet` one (LRM 8.14). `super.v` reads the
member one level up (8.15), and `::` reaches a public or protected member of any superclass from
within a derived class (8.23). Methods are the other half of the same picture and behave the
opposite way: a `virtual` method is selected by the object (8.20). Storage selection is static;
method selection may be dynamic. They are two mechanisms, not one.

A third clause decides the shape. A `local` member is not visible within subclasses, while the
base's own inherited methods still reach it on a derived object (8.18). So a derived class carries
storage it cannot name, and cannot count what its base declares. Across compilation units it cannot
see unpublished members at all, which is the same statement the North Star makes about incremental
and separate compilation.

The front end already states what this needs. `LocalClassPropertyTarget` carries the declaring class
beside the property, and MIR keeps it as `FieldTarget`'s owner. Both consumers then drop it: the C++
backend uses the owner only to look the name up and renders an unqualified `recv->name`, so the
target language's own lookup finds the derived class's member; and MIR-to-LIR keeps the slot alone,
leaving a member projection whose index is read against the type the chain arrived at. Two recorded
defects are the same fact seen twice -- reading a shadowed property through `super` yields the
derived class's own, and storing into an inherited member builds a place the LIR verifier rejects.

## Decision

**A member projection names the declaration that declares the member, and the slot that declaration
gave it.** Which storage an access reaches is stated at the access site, never re-derived from the
type the projection arrived at. The type the chain arrives at is checked against the declaring
declaration -- it must be that declaration or one that extends it -- rather than consulted to find
the member.

**An inherited member keeps its slot.** A declaration's storage is its base's storage extended with
its own members, so a member declared by a class sits at the same position in that class and in
every class that extends it. Two things follow, and the second is the one that keeps the model
simple: adding a member a base does not publish moves nothing in any class that extends it, and a
member's position is a property of the declaration that declares it alone, so resolving the pair
yields one position rather than an offset an access site has to compute.

**The pair is resolved where the layout is known, and which place that is belongs to the backend.**
The C++ backend qualifies the member with its declaring class and lets the target language resolve
it against the base subobject it already emits. The execution backend resolves it against the
storage schema the runtime builds, where the whole lineage is in hand. Neither spelling is a fact
the IR carries: LIR states the pair and nothing else.

## Rejected alternatives

- **One flattened member list computed during lowering, base members first.** A derived class cannot
  count its base's `local` members (8.18) and cannot see another unit's unpublished members at all,
  so the position it would compute is not knowable where it would have to be computed. It also makes
  a base's private addition move every derived member, which is the fragile base class problem and
  contradicts the reason `published-member-placement.md` fixes a published member's position ahead
  of every unpublished one.

- **A base subobject as a step in the place vocabulary.** Nothing in SystemVerilog names a base
  subobject as a value: `super` is not an expression, `super.new` is a call, and `$cast` yields a
  handle to the whole object. The step would be one no chain ever ends on, which is two steps saying
  what one step should say. Its length would also track inheritance depth, while `::` names an
  ancestor directly (8.23).

- **Deriving the declaring class from the type the chain arrived at.** This is what the code does
  today. It cannot distinguish a shadowed member from the one that shadows it, because the
  arrived-at type is the derived class in both cases -- which is exactly the recorded wrong answer.

- **Computing a base offset at each access.** Adding the declaring class's inherited member count to
  the slot at every access does not use the rule above: the addition belongs where the schema is
  built, once per declaration, not on the access path.

## Consequences

- LIR's member projection carries the declaring declaration and the slot, and the slot type is named
  for what it is -- a position within one declaration, meaningless alone. The verifier checks that
  the projection's arrived-at type extends the declaring declaration, which is where the static
  selection rule is enforced.
- MIR-to-LIR carries the owner MIR already holds instead of discarding it.
- The C++ backend renders a class member qualified by its declaring class. This is uniform: a class
  may always qualify a member it declares, so there is no shadowed-or-not branch.
- The runtime builds a class's member storage schema by extending its base's, so an inherited member
  is reached on a derived object at the position its declaring class gave it.
- A declaring class in another compilation unit is not resolvable at compile time, since the members
  it does not publish still take positions. That is the same case the path already refuses; when it
  lands, the position is what the runtime supplies rather than what lowering computes.
- Not this decision: which method a call selects, and when a base's constructor and a class's
  property initializers run (8.7). Both are stated elsewhere and neither changes how storage is
  named.

## Cross-references

- `docs/decisions/member-slot-storage.md` (a member is a logical place; the storage schema this
  extends)
- `docs/decisions/published-member-placement.md` (why a position a unit promised may not move)
- `docs/architecture/object_model.md` (one nominal object type; inheritance is one concrete base)
- `docs/architecture/lir.md` (a place names storage by logical identity; physical layout is derived
  below LIR)
