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
  one for tagged unions only), and answers a value the standard does not give.
- An unpacked union of unpacked structures is "C-compatible, members in ascending address order"
  (7.3.1).
- Dynamic types -- a string among them (6.16) -- and `chandle` only in a tagged union (7.3, 7.3.2).
- A tagged union carries its tag; a read or write of a member the tag does not name is a run-time
  error (7.3.2, 11.9); the tag changes only by assigning a whole tagged value.
- `%p` prints an untagged union's first member (21.2.1.6); streaming streams it (11.4.14.1); `%u` /
  `%z` and `$fread` act on it (21.2.1.7, 21.3.4.4).
- A union member is never a reference target (13.5.2).

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

### D1. An untagged union is C's union

Its storage is the largest member's size at the strictest member's alignment, and every member lies
at offset 0. A member is a field step of the union's place (MIR's field step, overlaid). A write
stores the member and leaves every other byte as it was. A copy is the whole storage, and an end
does nothing.

### D2. Reading a member answers the member's type made from the bits there

The read is the overlaid read: the bits where the member lies, with the positions its type's
representation keeps clear cleared. That is the common initial sequence with no rule of its own, and
a read of any other member answers what its bits say (7.3.1's "C-compatible").

### D3. A union literal names its member and zeroes everything else

As C does, because every byte may be read. The default is the first member at its default.

### D4. Every whole-value use of an untagged union reads its first member

`%p`, streaming both ways, `%u` / `%z` and `$fread` as the standard says, and equality and net
resolution, which it does not name, by the same rule. Whether a holder changed is whether its bytes
did, since the bytes are what a read of any member sees.

### D5. A member that owns something is refused in an untagged union

The standard admits none. The front end accepts a string, a class handle and an event, and Lyra
refuses each as unsupported where the type is translated, until the front end refuses it.

### D6. A tagged union is its members overlaid as in D1, then its tag

The tag is the smallest unsigned integer that counts the members, after the overlay at its own
alignment. Every member therefore lies at offset 0 in both kinds of union, so a member is one field
step whichever kind it is in, and the tag is the only thing a tagged union adds.

### D7. MIR checks the tag before every member read and write

`require(tag_matches(u, k))` with LRM 11.9's message, then the field step, of no-address kind. The
step checks nothing. A whole tagged value is built by storing the member and then the tag, and
assigning one ends the old value first; copying and ending follow the tag.

## Where Lyra differs, and why

| Point                          | Field                                | Lyra                                       | Reason                                                                                                                               |
| ------------------------------ | ------------------------------------ | ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------ |
| a read through another member  | undefined (clang, rustc)             | answers the bits, cleared above each width | SystemVerilog defines the read and has no undefined behaviour                                                                        |
| bytes a literal does not write | zero in C, undefined in C++ and Rust | zero                                       | every byte may be read                                                                                                               |
| where the tag lies             | first (rustc)                        | after the members                          | an SV member is one value and no member type has a niche, so a tag first buys nothing, and members at 0 keep one rule for both kinds |
| the tag check                  | a test and a switch (rustc)          | one node asking whether the tag is k       | MIR has no multiway switch; the tag's only reader compares it to one member                                                          |

## Rejected

- **The active-member value.** It cannot read the common initial sequence, and it refuses a read the
  standard makes legal.
- **Recording which member an untagged union holds.** It serves only a union holding something that
  owns, which the standard forbids; refusing that keeps the answer that stays right once the front
  end refuses it too.
- **The tag first, as rustc lays an enum out.** Its reasons -- several fields packed against the
  tag, a niche standing in for it -- do not reach an SV member, and every member at 0 is one rule
  for both kinds.
- **A cross read answering the raw bytes.** A value whose positions above its width are set compares
  wrong everywhere storage is compared
  ([a-packed-value-is-its-words](a-packed-value-is-its-words.md)).
- **A read operation for unions alone.** The clearing belongs to the read of an overlaid place
  ([an-expression-names-storage-or-makes-a-value](an-expression-names-storage-or-makes-a-value.md)),
  where both backends meet it.

## Consequences

- The library's union objects and their entries leave both backends; a union is laid out by its type
  like a structure.
- A member access is a load or a store where the member lies, not a library call, and writing a
  member no longer rebuilds the union.
- The common initial sequence, a cross read, `%p` of a union holding another member and streaming an
  untagged union run; a property read before its initializer holds the type's default.
- A union too large for a frame is held through a box, as every value is: MIR's frame size counts
  the overlay, and the tag after it for a tagged union.
