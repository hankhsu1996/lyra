# A union is one storage its members overlay

Date: 2026-10-13 Status: accepted. Supersedes points 3, 4, 5, 7 and 8 of
[unpacked-union-representation](unpacked-union-representation.md); its points 1, 2 and 6 stand.
Reading a member rests on
[an-expression-names-storage-or-makes-a-value](an-expression-names-storage-or-makes-a-value.md).

## Context

An unpacked union was an active-member value: which member it held, and that member's value. The C++
backend realized it as a library variant, the execution backend as a library object holding the
member behind a type-erased handle, and a read of any member but the held one was refused on both.
The standard asks for more than that and less: one storage the members are read through (LRM 7.3), a
common initial sequence readable through any structure that shares it (7.3), and a read of another
member that is legal and is no error (7.3.2: "can be updated using a value of one member type and
read as a value of another member type, which is a potential type loophole").

## How the shape got here

1. **An overlay was unrealizable**
   ([unpacked-union-representation](unpacked-union-representation.md), 2026-06-27). The value
   library's types were rich objects -- an X/Z plane, heap storage -- that could not share bytes,
   and every cross-member read was taken to be undefined. An active-member value served every
   program that read only what it wrote.
2. **The common initial sequence was found to be defined** (that entry's 2026-10-05 revision), and
   the overlay it needs became realizable in principle once a value is its machine data
   ([a-value-is-its-machine-data](a-value-is-its-machine-data.md)).
3. **Every held value was laid out from its type** (the entries listed under "How the shape got
   here" in
   [an-expression-names-storage-or-makes-a-value](an-expression-names-storage-or-makes-a-value.md)),
   which leaves the union the last value a library object holds. Laid out the same way, it is an
   overlay.

## What the standard requires, read whole

- One storage reached through any member's type; one member used at a time; no required
  representation (7.3).
- A variable with no initializer holds the first member's default (Table 7-1).
- A common initial sequence of unpacked structures reads what was written through any of them (7.3);
  any other read through a member not last written is legal, is no run-time error (11.9 prescribes
  one for tagged unions only), and answers a value the standard does not give. 7.3.1 grants reading
  back what another member wrote to packed untagged unions alone ("unlike unpacked and tagged
  unions"), which withholds the value and not the read.
- An unpacked union of unpacked structures is "C-compatible, members in ascending address order"
  (7.3.1).
- Dynamic types -- a string among them (6.16) -- and `chandle` only in a tagged union (7.3, 7.3.2).
- A tagged union carries its tag; a read or write of a member the tag does not name is a run-time
  error (7.3.2, 11.9); the tag changes only by assigning a whole tagged value.
- `%p` prints an untagged union's first member (21.2.1.6); streaming streams it (11.4.14.1); `%u` /
  `%z` and `$fread` act on it (21.2.1.7, 21.3.4.4).
- A union member is never a reference target (13.5.2). The front end refuses one, and a part below
  one ([language_conformance](../architecture/language_conformance.md), "A member of a union, or a
  part of one, passed by reference").

## What established compilers do

- **clang** lays a union out with every member at offset 0, size the largest, alignment the
  strictest (`RecordLayoutBuilder.cpp` `LayoutField`, `CGRecordLayoutBuilder.cpp` `lowerUnion`). A
  member is the union's own address retyped (`EmitLValueForField`: "For unions, there is no pointer
  adjustment"). A copy is the whole storage (`EmitAggregateCopy`). A union literal initializes one
  member, and in C the remaining bytes are zeroed because programs read them
  (`shouldZeroInitPadding`); in C++ they are left undefined. Reading a member whose bits are invalid
  for its type is undefined by default, and `-fno-strict-bool=truncate` defines it by truncating at
  the load. A union with a member that needs construction or destruction has those operations
  deleted: nothing tracks which member is live at run time.
- **rustc** lays a union out the same way (`layout_of_union`), reaches a field with a plain `Field`
  projection, and requires every field to need no dropping (`check_union_fields`), so a union never
  tracks its member either; reading a field is `unsafe`. An enum is a tag plus its variants: the
  check is a `Discriminant` read and a switch before a `Downcast` projection that itself checks
  nothing; building a variant stores its fields and then the tag; dropping and cloning switch on the
  tag. Its tagged layout puts the tag first, at offset 0, and lays each variant out after it
  (`layout_of_enum`, `StructKind::Prefixed`), which lets a variant's several fields pack against the
  tag and lets a niche stand in for it.

## Decision

### D1. An untagged union is the bits of its widest member, every member a view of them

Its storage is one integral value of as many positions as its widest member has, 4-state where any
member has a 4-state bit, as a packed union's is (7.3.1, 7.2.1). A member's positions are numbered
as everything a net resolves or a procedural continuous assignment takes is numbered -- an integral
value's bits from its least significant, a real's IEEE 754 bits (6.12), a structure's and an array's
parts in turn -- and every member's start at the storage's first position. In MIR the union is a
record of one component, its storage, and keeps its members' types as the views it is read and
written through; below MIR it is a structure of that one component. A copy is the storage, and an
end does nothing.

### D2. A member is read as the value its positions make, and written as its positions

Reading a member is the storage's first positions made into the member's type: an integral member's
bits converted to its declared type, a real's from its IEEE bits, and any other member's by its
type's from-positions operation. Writing one replaces those positions of the storage, through a
reference to them, by the member's positions; a write below a member -- a bit of it, a member or an
element of a structure or array it is -- replaces the positions of the storage that part takes, an
element index naming no element moving the write past the storage, where it lands on nothing (LRM
7.4.5). The common initial sequence (7.3) lies at the same positions in every structure that shares
it, with no rule of its own, and a read through another member answers what its positions hold
(7.3.2).

### D3. A union literal names its member and zeroes everything else

As C does, because every bit may be read. The default is the first member at its default.

### D4. Every whole-value use of an untagged union reads its first member

`%p`, streaming both ways, `%u` / `%z` and `$fread` as the standard says, and equality, which it
does not name, by the same rule. Whether a holder changed, and the positions a net resolves or a
takeover holds, are the storage's, since every member is a view of it. Each of these operations is a
method the union's declaration states, as a structure's are
([a-structures-operations-are-stated-in-mir](a-structures-operations-are-stated-in-mir.md)), and a
tagged union's answer the same questions through the member its tag names.

Revision 2026-11-17: D1 and D2 were C's union (every member at offset 0 of one byte storage, each
laid out by its own type, read by an overlaid load clearing above each width). A net resolves bits,
and members of different widths put the same bit at different bytes (a 4-state value's planes are
sized by its width), so no numbering of a union's bits wrote back right: a release, a force, or a
whole transfer of members of one byte size lost bits, measured on both backends. Holding the union
as its bits, the shape 6.7.1's "a net is composed entirely of 4-state bits" and 7.3.1's soft packed
union describe, makes a net of one a net of those bits and removes the overlaid place kind, the
overlaid load and the library union type from every layer. No unpacked union crosses DPI (35.5.6
admits packed forms only), so no C layout is owed.

### D5. A member that owns something is refused in an untagged union

The standard admits none. The front end accepts a string, a class handle and an event, and Lyra
refuses each as unsupported where the type is translated, until the front end refuses it.

### D6. A tagged union is its members overlaid, then its tag

Its storage is the largest member's size at the strictest member's alignment, every member at offset
0, and after the members the tag, the smallest unsigned integer that counts them, at its own
alignment. A tagged union may hold what owns something (7.3), so its members are laid out by their
own types and reached by a field step under the tag check of D7. Held in an untagged union, its
positions are the member it holds from the first and the tag above its widest member's, as a packed
tagged union lays them out (7.3.2); positions whose tag names no member make the first member at its
default.

### D7. MIR checks the tag before every member read and write

`assert(discriminant(u) == k)` with LRM 11.9's message, then the field step, of no-address kind. The
tag is read by its own node, rustc's `Rvalue::Discriminant`, and compared with the member's position
like any other machine integer (revised 2026-10-16: one call asked whether the tag named k, which
the lowering to LIR turned into a read of the tag). The check is rustc's `Assert`: a call that
answers nothing, stated before the access, so the access reaches the union where it lies rather than
through a value the check handed back. A read states it among the steps of the read, since it is
part of evaluating the access (LRM 11.9), and the whole read -- every select written below the
member -- is taken and its value kept among those same steps, so nothing else the expression
evaluates runs between the check and the read: a sibling operand retagging the union would otherwise
have the read land on a member the union no longer holds. A call that only reads what the member
selects is entered on it where it lies among the same steps, after its own operands, as rustc
borrows `(u as V).0` after the discriminant test and calls on the borrow. A write states it as a
statement before the write lands. A packed tagged union is checked the same way, over the tag bits.
The step checks nothing, and neither does the library. A whole tagged value is built by storing the
member and then the tag, and assigning one ends the old value first; copying and ending follow the
tag.

## Where Lyra differs, and why

| Point                         | Field                                 | Lyra                                | Reason                                                                                                                               |
| ----------------------------- | ------------------------------------- | ----------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| a read through another member | undefined (clang, rustc)              | answers the bits its positions hold | SystemVerilog defines the read and has no undefined behaviour                                                                        |
| bits a literal does not write | zero in C, undefined in C++ and Rust  | zero                                | every bit may be read                                                                                                                |
| how members share storage     | bytes from offset 0 (C, clang, rustc) | bits from position 0                | a net resolves bits, and members of different widths lay a bit out at different bytes                                                |
| where the tag lies            | first (rustc)                         | after the members                   | an SV member is one value and no member type has a niche, so a tag first buys nothing, and members at 0 keep one rule for both kinds |
| the tag check                 | a test and a switch (rustc)           | the tag read, then compared with k  | the tag's only reader compares it to one member, and MIR has no multiway switch                                                      |

## Rejected

- **The active-member value.** It cannot read the common initial sequence, and it refuses a read the
  standard makes legal.
- **Recording which member an untagged union holds.** It serves only a union holding something that
  owns, which the standard forbids; refusing that keeps the answer that stays right once the front
  end refuses it too.
- **The tag first, as rustc lays an enum out.** Its reasons -- several fields packed against the
  tag, a niche standing in for it -- do not reach an SV member, and every member at 0 is one rule
  for both kinds.
- **C's union, every member at offset 0 of one byte storage** (D1 and D2 until 2026-11-17). A
  4-state value's planes are sized by its width, so two members keep one bit at two bytes: a net of
  the union, which resolves bits, could number them only per member, and then every write back of a
  range -- a release, a force, a whole transfer -- rewrote members nobody drove.
- **Recording which member an untagged union presents in a net.** It is state per union occurrence
  that every write back consults, and it still loses what a whole transfer carries outside that
  member; holding the bits leaves nothing to record.

## Consequences

- The library's untagged union type, the overlaid place kind, the overlaid load and the clearing of
  bits above each width leave every layer: below MIR an untagged union is a structure of its one
  storage, and its whole-value operations are code compiled for its type, on both backends.
- A member read is a slice of the storage made into the member's type, and a write a store of the
  member's positions through a reference to them; a net of a union resolves those positions as any
  net's, so no rule of nets names a union.
- The common initial sequence, a cross read, `%p` of a union holding another member and streaming an
  untagged union run; a property read before its initializer holds the type's default.
- A union too large for a frame is held through a box, as every value is: MIR's frame size counts
  the storage, and a tagged union's overlay and the tag after it.
- A union of any member count is one shape on both backends; nothing about it is stated as one
  expression over every member, so no compiler's bound on such an expression limits a design.
