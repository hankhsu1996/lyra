# A reference is a pointer-sized tagged pointer

Date: 2026-09-11 Status: accepted

## Context

[storage-owns-its-value](storage-owns-its-value.md) fixed that a reference is pointer-like to stable
storage, and [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md)
fixed that there are two storage forms it must be able to name. What remains is the physical
representation, which [reference-binds-a-cell](reference-binds-a-cell.md)'s reopening asked for and
deliberately did not write: "a reference carries the referent's address plus enough erased
place-class information to perform the three place operations -- load, store, and re-lend -- without
exposing value representation."

That reopening also warns against the shape decided here, and the warning has to be answered rather
than passed over: "Plain-versus-signal is not the permanent set; an array element, a class property,
an interface member and force/release-aware storage are all place classes this compiler will meet,
so a two-valued tag would force the ABI open again for each."

Two things settle it. Measurement: across four reference representations, cost tracks the number of
dependent loads and nothing else -- two loads to three costs 1.07 ns, two to five costs 2.8 ns --
and the arm test of a two-armed reference is free, measuring 3.206 ns with the arms randomly mixed
against 3.210 and 3.226 homogeneous, and 2.986 for a raw pointer. And an enumeration of every
referent the language admits, below, which finds that the place classes the warning names do not in
fact need distinct reference kinds.

## Decision

**A reference is one pointer, tagged where the target form requires it, and never a descriptor that
must be decoded by chasing further pointers.**

**D1. Pointer-sized.** `sizeof(Ref<T>) == sizeof(void*)`. The tag occupies low bits of the pointer,
which the storage's alignment leaves free.

**D2. The tag names the storage form, not the container or the path.** Today two forms: plain
storage, and observable storage whose value sits at a fixed offset after its subscriber record. A
read resolves the value address from the pointer and the tag, then loads. A write does the same, and
for the observable form enters that form's store, which is where change detection and subscriber
wakeup live.

**D3. Never an owner plus an index or a path to re-evaluate.** A reference resolves its access path
once, at the bind, and what it retains is storage. Anything that would have to be walked or decoded
per access is excluded, which is what the measurement says the cost actually is.

**D4. A statically known form needs no tag check.** Where the lowering knows a reference's form --
the common case, since a `ref` formal at a given call site binds the same kind of storage every call
-- it may lower reads and writes to the direct form, with the tagged representation as the general
case rather than a per-access cost.

**D5. The tag space is bounded by alignment and is not a two-valued commitment.** Eight-byte-aligned
storage leaves three low bits, so the representation admits up to eight forms. What it cannot admit
is a form needing more information than a pointer carries.

## Invariants

1. A reference is one machine word. A representation that needs more is not this reference.

2. The value's address is computable from the reference alone -- pointer and tag -- with no load.
   Only reading the value itself is a load.

3. A write through a reference reaches the storage form's own store. The form decides whether that
   raises an update event; the reference never decides it and never performs it.

4. The tag distinguishes storage forms only. It never encodes which container an element belongs to,
   which position it occupies, or which expression found it.

5. **Referenceable storage guarantees the minimum alignment the tag bits need.** This is an ABI
   obligation on storage, not an observation about what the current representations happen to be.
   Every representation in the runtime today is eight-byte aligned, which would make the guarantee
   easy to leave unstated and easy to lose; a future native value model that places a one-byte
   element inline would break the reference without touching it. Storage that cannot meet the
   alignment cannot be referenceable.

## Every referent, and which kind it needs

LRM 13.5.2 is exhaustive -- a variable, a class property, a member of an unpacked structure, or an
element of an unpacked array -- and adds that "nets and selects into nets shall not be passed by
reference", which removes the only other storage in this runtime carrying subscriber metadata.

| Referent                                                               | Resolves to                                                                                                                                   | Kind                                            |
| ---------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------- |
| automatic local, class-method static local                             | the storage itself                                                                                                                            | plain                                           |
| module or package variable, interface member, design-body static local | storage plus its subscriber record                                                                                                            | observable                                      |
| variable whose type is a class handle                                  | the handle's own storage -- 13.5.2: "a `ref` of an object handle allows changes to the object handle"                                         | plain or observable, by the variable            |
| class property                                                         | its slot in the object's storage block                                                                                                        | plain                                           |
| unpacked structure member                                              | its member storage in the aggregate                                                                                                           | plain                                           |
| fixed unpacked array element                                           | its element storage in the aggregate                                                                                                          | plain                                           |
| dynamic array, queue, associative element                              | its stable element slot                                                                                                                       | plain                                           |
| an element detached by removal                                         | the same slot, outside membership                                                                                                             | plain -- unchanged by detaching                 |
| a `ref` formal lent onward                                             | the alias it already holds                                                                                                                    | unchanged                                       |
| force / release-aware storage                                          | a static variable; 6.21 bars force from automatic variables, dynamic-array elements and non-static properties, and nets are not referenceable | observable, with the force state in the storage |

So the four place classes the reopening warned about resolve to two kinds, not six. The warning
assumed that a place class multiplies the reference; what actually multiplies is _where storage
lives_, which the reference does not encode.

## Rejected

- **A generic descriptor of owner plus id plus kind.** Its real advantage is that a dead id makes an
  outdated reference detectable rather than dangling, which is the safe side of an LRM silence.
  Rejected on cost and on scope: measured at five dependent loads for the element kind (1.94x a raw
  pointer) and three for a tuned one, against one for a tagged pointer, and its detection benefit
  applies to a case the language already bounds to the callee's scope. Reconsider only if detached
  storage turns out to need detection rather than lifetime.

- **Two pointers, one per arm.** What the C++ backend has: 16 bytes, one branch, both arms loaded.
  It measures the same as a tagged pointer, so it is rejected for width alone -- and width is not
  free where references live in memory, which the measurement showed by doubling the reference
  array's footprint at 4096 references.

- **A separate reference type per storage form.** It removes the tag, and it is not expressible: a
  callee has one formal, lowered once, and its type cannot vary with the storage a caller lends.
  That is [reference-binds-a-cell](reference-binds-a-cell.md)'s own argument and it is unaffected by
  anything decided since.

- **Keeping one unified cell so that the reference needs no tag.** The shape in place, rejected by
  [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md): it buys a
  free branch by putting a subscriber record on every borrowed local, and the branch was measured to
  cost nothing.

## Consequences

- **This constrains the open fork in [container-element-storage](container-element-storage.md).**
  That entry left open whether an element slot is an address in non-relocating storage or an index
  into a relocatable arena. An index needs the arena as well, which is more than a pointer carries,
  so **D1 requires the address answer**. The measurement already showed the two within 3% on element
  access, so nothing is lost; what remains open is only how the ordering structure names slots,
  which the reference does not see.

- **It places a constraint on the collector that `../architecture/lifetime.md` currently says does
  not exist.** That document records the initial collector as non-moving and notes that "non-moving
  keeps borrowed receivers, virtual-interface handles, and foreign pointers stable", then states
  that "generational, incremental, or moving strategies are later collector choices that change none
  of the invariants above". A reference to a class property is a pointer into a managed object, so a
  moving collector would invalidate it. Either the collector stays non-moving, or an object with an
  outstanding reference is pinned, or class-property references become a form the collector can
  update. **The standing direction is to keep the collector non-moving and pinning-friendly rather
  than pay for a future moving collector on the reference hot path**, which is a direction for the
  lifetime contract to confirm and not a choice this entry may make for it.

- **The fast path in generated code is a load or a store.** Where D4 applies the tag check
  disappears entirely, and where it does not the cost is one predictable test, measured at 1.07x a
  raw pointer.

- **The runtime ABI narrows rather than widens.** A reference already crosses as one `void*`; it
  stays one word, and what changes is that the word no longer implies a single cell kind.

## Cross-references

- [reference-binds-a-cell](reference-binds-a-cell.md) -- the contract this writes, and the warning
  against a two-valued tag that the referent enumeration answers.
- [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md) -- why there
  are two forms to name.
- [storage-owns-its-value](storage-owns-its-value.md) -- that a reference is pointer-like to stable
  storage.
- [container-element-storage](container-element-storage.md) -- the open fork this constrains.
- [reference-as-data-type](reference-as-data-type.md) -- a reference is a data type at MIR; this is
  its physical representation, not its semantics.
- `../architecture/lifetime.md` -- the collector contract this conflicts with.
