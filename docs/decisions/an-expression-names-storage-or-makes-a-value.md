# An expression names storage or makes a value, and MIR says which

Date: 2026-10-13 Status: accepted. Supersedes, for a part of storage the language lays out, D1 of
[value-descent-as-named-calls](value-descent-as-named-calls.md) and D1 of
[value-projection-write](value-projection-write.md); narrows D2 of the first to the parts whose read
and write the standard makes two operations; reverses the first rejected alternative of
[a-part-of-storage-is-reached-where-it-lies](a-part-of-storage-is-reached-where-it-lies.md) and
moves its "which parts are storage" from MIR-to-LIR into MIR.

## Context

MIR stated a step into a part of a value as a call, one entry answering the part's value and one
answering the part, and nothing on any node said whether an expression names storage. The layers
below needed that answer anyway -- to read a part where it lies, to keep a large value out of a
frame, to verify that one is -- so it was recovered after MIR was built, by two visits over finished
expressions that did not agree with each other. The C++ render had no point at which "the value is
read out of storage" happens at all, because C++ reads an lvalue implicitly; an unpacked union's
member read through another member, which must clear the positions above each width
([a-packed-value-is-its-words](a-packed-value-is-its-words.md)), had nowhere to do so on that
backend.

## How the shape got here

Each step below was a sound derivation under the representation it was made against. What changed
each time was the representation.

1. **Places existed for whole storage only.** A variable, an object's member and a dereferenced
   referent were places; LIR named storage with a local and a projection chain, as Rust's MIR does.
2. **A part of a value was not storage** ([value-projection-write](value-projection-write.md),
   2026-07). A value aggregate was an object of the value library that the generated side held as a
   handle and could not see into ([jit-value-realization](jit-value-realization.md) invariant 6), so
   a struct component, an element, a union member or a string character was a "value projection",
   written by replacing the whole. Right while values were handles.
3. **Every level of a descent became a call**
   ([value-descent-as-named-calls](value-descent-as-named-calls.md), 2026-09-09), a read entry and a
   part entry, named where the source is read, so no backend classified a step. Its D5 rests on the
   same premise: a union and a string cross to the execution backend "as values only the library can
   see into".
4. **A part that is storage became a place step below MIR**
   ([a-part-of-storage-is-reached-where-it-lies](a-part-of-storage-is-reached-where-it-lies.md),
   2026-09-24), once a value was an object in its maker's frame. MIR-to-LIR read the entry and the
   receiver's type to decide which calls were place steps. That entry rejected the call answering a
   part's address, because a dereferenced pointer to a value then meant a value cell.
5. **Every held value was laid out from its type**
   ([a-value-is-its-machine-data](a-value-is-its-machine-data.md),
   [a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md),
   [a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md)), a value too
   large for a frame held through a box whose dereference is ordinary storage. No part of a held
   value is behind the library any longer, and a cell is a wrapper type of its own, distinct from a
   pointer: what a dereference reaches is read off its type.

The premise of 2, 3 and 4's rejection is gone, and what was left of it was the recovery. This entry
states in MIR what the recovery worked out.

## What established compilers do

- **clang** stores a value kind and an object kind on every expression, set by Sema at construction
  (`Specifiers.h` `ExprValueKind` / `ExprObjectKind`). A read of an lvalue is an explicit
  `ImplicitCastExpr<LValueToRValue>` (`Sema::DefaultLvalueConversion`). Code generation loads by the
  lvalue's kind (`EmitLoadOfLValue`: simple, vector element, bit-field) and converts the memory form
  at the load (`EmitFromMemory` truncates a `_BitInt`). A field is a `MemberExpr`; a bit-field is an
  lvalue with no address, and `&` of one is refused. A library container's element is an lvalue
  because `operator[]` answers `T&`, and `std::map::operator[]` inserts on a read.
- **Neither has bits of a value as a node.** In clang a bit-field is a `MemberExpr` whose offset its
  record type fixes, and bits a program computes the position of are a library's: libc++'s
  `bitset::operator[]` answers the proxy `__bit_reference` (`libcxx/include/__bit_reference`), whose
  `operator=` writes the bit, so the AST holds only `CXXOperatorCallExpr`. rustc's `ProjectionElem`
  has no bit projection either; the `bitvec` crate answers a `BitRef` the same way.
- **rustc** decides place or value from the kind of a THIR expression alone (`category.rs`
  `Category::of`: `Field`, `Index`, `Deref`, a variable are places). A place used as a value is read
  by `Operand::Copy` in MIR; a value used as a place gets a temporary (`as_place.rs`). A library
  container's element is `*Index::index(&v, i)` or `*IndexMut::index_mut(&mut v, i)`, the mutable
  one chosen by how the place is used -- an assignment target, a `&mut`, a mutating receiver
  (`place_op.rs` `convert_place_derefs_to_mutable`). THIR marks no read.

MIR sits where clang's AST and rustc's THIR sit; LIR sits where rustc's MIR does. A place expression
in MIR becomes a LIR place in MIR-to-LIR, the move `as_place` makes from THIR to MIR.

## Decision

### D1. Whether an expression is a place is the kind of the node

A variable, a dereference of a value of pointer or reference type and a field step on a place are
places; every other node makes a value. A call that names storage answers a reference, and the
dereference of that reference is the place, so no call is a place by itself and nothing records
category beside the node.

### D2. A field step is one node on a place

A product's component, a union's member, and a sum's member after the tag check are reached the same
way for a read and for a write, so each is a step on the place holding the aggregate, by the
member's position. Where the member lies is the aggregate type's layout, which each backend's type
mapping answers.

### D3. An element of an array is two calls, each answering a reference

The standard gives a read and a write of an element different outcomes. A read through an invalid
index answers the element default and a write through one is ignored (LRM 7.4.6), and the default of
a structure element carries its members' initializers (7.2.2), so the outcome belongs to the step
and not to a guard after it: reading `a[9].f[0]` of `struct { int f[2] = '{5,5}; } a[4]` answers 5.
An associative entry is allocated by a write, a `ref` actual or a read-modify-write, never by a read
(7.8.6, 7.8.7). So every array -- fixed, dynamic, queue, associative -- has a read form, answering
the element or the element default, and a write form, answering the element, a discard slot or a new
entry. HIR-to-MIR takes the write form where the place is an assignment target, a `ref` actual, a
mutating receiver, a read-modify-write or a nonblocking target, and the read form otherwise.

### D4. Bits, a window and a character are no places

Bits of a packed value are reached through the value's own operations, as every generic language
reaches them. A read is `slice<T>(v, start)` on the value, named where it lies. A write is a store
through the reference to the bits that `slice_mut<T>(v, start)` answers, taken on the place the
value lies in, as `bitset::operator[]` answers a reference to a bit and `b[i] = x` assigns through
it. A packed value's parts nest, each with bounds of its own (LRM 7.2.1, 7.4.1) -- `v[i][j]`,
`s.f[3]` -- so a part of a part is `slice_mut` taken on the reference to the outer part. Within a
write in progress the reference is taken on the designation of the value, so the write hears which
bits moved. An invalid start reads x and writes nothing (LRM 11.5.1), which is how the operations
behave. A window of several elements is not one storage, and a partly invalid one writes only its
valid elements, which no reference can carry: it is read by a step answering it as a value of its
own and written by the array's own operation on the place the array lies in, as rustc copies into
`*index_mut(&mut a, range)` by a method. A string's character has no identity of its own either: a
read is a call on the string read, a write a mutating call on the string's place.

### D5. A place carries what kind of storage it is

Ordinary storage has an address. A union's member and everything below it has none of its own (LRM
13.5.2). An untagged union's member and everything below it is also overlaid: its bits may be
another member's. A place's kind follows from its node and the kind of the place it is taken on -- a
field step on a union gives no-address or overlaid and on anything else its operand's kind, a
dereference gives ordinary -- so it is computed from the node rather than stored beside it, as rustc
reads such facts off a place's projections. A reference the program holds ends only on ordinary
storage; a borrow and the object a call is entered on take storage with none by reading it first and
holding its value for the full-expression.

### D6. A read is a node

`read(place)` yields the value the place holds, and how it is realized follows the place's kind: an
ordinary or no-address place is loaded; an overlaid one is loaded and its positions above each width
are cleared, so the value read holds what its type's representation requires.

### D7. A write is an assignment to a place

Its target is a place, which the verifier checks. A store into an overlaid place stores the leaf
whole, which leaves its positions above its width clear.

### D8. A value where a place is needed is held for the full-expression

As a borrow of a value already does, and as rustc gives a value used as a place a temporary.

### D9. A capability wrapper answers with a reference, and a write into it stays a write in progress

Reading a wrapper asks it for what it shows, a call answering a reference whose dereference is an
ordinary place. Writing into it goes through the write in progress, each step into a part that is
storage taken on that write so the write hears it (LRM 4.3); the dereference of the last designation
is the place, and a step past the last part with an identity is a field step of D2.

### D10. What an operation takes an operand as is fixed by the operation

A place stands where a value is wanted only under a read. A write's target, what a borrow or an
address-of takes and the object a field is reached on are the storage itself, never a read, which
would act on a copy. The object a call is entered on is named where it lies, as clang binds the
implicit object argument and rustc autorefs a receiver; an entry that takes it by value, as a Rust
method taking `self` does, is handed it read by each target's realization of the call, and no target
realizes the two differently, so nothing beside the entry states which. A dereference reads the
pointer it opens, as clang reads the operand of builtin `*p`, and enters an owning pointer as an
object, as C++ `*unique_ptr` enters `operator*` on it. A read and a move each take a place that
holds a value, as rustc's `Operand::Copy` and `Operand::Move` do.

## Where Lyra differs from both, and why

| Point                         | clang                  | rustc                 | Lyra                                        | Reason                                                                          |
| ----------------------------- | ---------------------- | --------------------- | ------------------------------------------- | ------------------------------------------------------------------------------- |
| a read                        | explicit node          | implicit at THIR      | explicit node                               | two consumers read MIR, and the C++ render has no context to decide a read from |
| an invalid index              | undefined              | a check that panics   | the element step answers default or discard | the standard defines both outcomes                                              |
| an element                    | one form, `operator[]` | `index` / `index_mut` | read form / write form                      | a read must not allocate (LRM 7.8.6)                                            |
| what a write tells its holder | nothing                | nothing               | the write in progress hears each step       | a write wakes what waits on the part it reached                                 |

## Rejected

- **Reads left implicit, decided by each consumer from context** (rustc's THIR). MIR-to-LIR could
  decide as rustc's builder does, but the C++ render has no context to decide from, and the two
  would hold one answer in two places.
- **A guard placed before an element step** (rustc's bounds check). It answers the default of the
  type read, where the standard answers the part of the element default the path names.
- **One element form answering a reference** (clang's `operator[]`). A read would allocate an
  associative entry.
- **Keeping the entry pairs and recovering category below MIR.** The two recoveries already
  disagreed, and the C++ render cannot recover a read point at all.
- **Bits of a place as a place node with no address**, one per part, as a C++ bit-field is an
  lvalue. Neither compiler has such a node -- a bit-field's offset is fixed by its record, and bits
  at a computed position are a library's reference -- and every consumer then had to decide which
  places could be bound, read where they lie, or entered on, which the value's own operations never
  ask.
- **A read operation for unions alone**, ending a read path through an untagged member. It patches
  the one consumer the missing read point hurt and leaves the recovery in place.

## Consequences

- The read / part entry pairs for a component and a union member, and the part entries after a
  designation, go; the array read / write forms stay, answering references.
- `IsPlaceExpression`, MIR-to-LIR's `NamesStorage`, `ReadsWhereItLies` and `StoragePartAccess` go:
  category is the node kind, and the kind of storage follows from the place's nodes.
- MIR-to-LIR stops rebuilding a whole value to change a part of storage; extract and insert remain
  for a part of a value that lies in no storage.
- The verifier gains a rule: an assignment's target is a place.
- Bits are written through a reference type of their own over the bits' type, which no backend names
  as a type: it lives only as the receiver of `slice_mut` and of `set`, and each target realizes the
  store by following the `slice_mut` steps to what the first was taken on. `designate_slice` and
  `designate_element_slice` go: designated bits are `slice_mut` on the designation, and a window
  within a write is the array's `assign_element_slice` on it.
- An operation over an operand settles how it takes it once it is known which operation it is, as
  clang's Sema settles an operand's conversion after overload resolution: a struct's method borrows
  a place where it lies, a library entry takes its value.
- A union's member is a field step, overlaid when the union is untagged, so an unpacked union needs
  nothing of its own to be read.
