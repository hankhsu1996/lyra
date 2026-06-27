# Unpacked Struct and Union

Tracks the unpacked struct and unpacked union data types (LRM 7.2 / 7.3). An unpacked struct is a
composite value with named, heterogeneous fields; an unpacked union overlays its members on shared
storage with value semantics. Both are genuinely different from their packed counterparts, whose
"treated as a single vector" projection is a separate, completed workstream in `packed.md`. Fixed
unpacked arrays and the variable-size aggregate family (dynamic array, queue, associative array) are
also separate (the latter in `aggregate.md`); this file's scope is unpacked struct and union only.

Done when:

- `datatypes/unpacked/unpacked_struct` and `datatypes/unpacked/unpacked_union` archive items
  reproduce on the current pipeline.
- The dependent items in [Unblocks](#unblocks) can proceed.

When the last item lands, this file is deleted.

## Actionable

The unpacked struct surface comes first: it is the composite-value shape the union and the dependent
workstreams build on. The union is the same composite value with overlaid members and a first-member
default. The numeric IDs are stable references and do not imply execution order beyond S1 preceding
the rest.

## Unpacked Struct

LRM 7.2: a struct groups members of possibly different types under one name. Without the `packed`
qualifier the struct is unpacked -- its members are stored independently rather than collapsed into
one vector -- so it is a composite value, not an integral one. Default value is member-wise (LRM
Table 7-1).

- [x] S1 -- Type infrastructure, declaration, member read, and blocking member write (LRM 7.2).
      Whole-struct assignment is value-semantic: a copy is independent of its source. Members may be
      any supported integral, packed, real, or string type; an unpacked struct nests as a member of
      another unpacked struct, and a struct is usable as an unpacked-array element and may itself
      hold an unpacked-array member.
- [x] S2 -- Assignment-pattern literals (LRM 10.9): positional `'{a, b, c}`, named `'{field: v}` in
      any order, type-key and `default:` fill (including mixed field widths), and the type-prefixed
      self-determined form `T'{...}`.
- [x] S3 -- Default initialization (LRM Table 7-1, 7.2.2). Each member defaults to its own type's
      Table 6-7 value, recursively, unless the member's declaration carries an initial assignment,
      in which case that value is used. The 2-state / 4-state distinction propagates per member. A
      member's initial assignment may itself be an aggregate literal (a struct- or array-typed
      member default).
- [x] S4 -- Aggregate copy and equality (`==` / `!=` / `===` / `!==`), and member access composing
      with element / range selects on packed members and on either side of an expression
      (`s.f[3:0]`, `s.f + 1`). A struct field reads and writes correctly in arithmetic, conditional,
      and other expression contexts. Case equality on a struct with a real / shortreal leaf is
      rejected, per the LRM (Table 11-1).
- [x] S5 -- Members with managed lifecycle (string, and any future value type with non-trivial copy
      semantics). Whole-struct copy gives each copy independent member storage; assignment releases
      the destination's prior member value; multiple and nested managed members compose. A struct is
      also usable as an unpacked-array element with the same value-independent copy on element
      write.

## Unpacked Union

LRM 7.3: a union holds one of its members at a time over shared storage. An untagged union is not
type-checked -- reading a member other than the one last written is a type loophole (LRM 7.3) whose
result is undefined and is not relied upon here. Default value is the first member's default (LRM
Table 7-1).

- [x] N1 -- Type infrastructure, declaration, member read and write for an untagged unpacked union
      (LRM 7.3). Writing then reading the same member round-trips; writing one member then another
      overwrites. Two- and three-member unions of integral / real / shortreal members. A tagged
      union is a distinct concept and is rejected for now.
- [x] N2 -- Default initialization to the first member's LRM Table 7-1 default. A declared union
      must default-initialize to be usable, so this lands with N1.
- [ ] N3 -- Whole-union value-semantic copy (independent copies) and member use in arithmetic and
      other expression contexts.

## Unblocks

- `datatypes.md` `default_init` closure: recursive LRM Table 7-1 defaults for unpacked struct and
  union members.
- `functions.md` F8 (a struct or union as a subroutine argument or return) and F10 (a reference
  formal bound to an unpacked-struct member).
- `hierarchy.md` E10 (an unpacked-struct port connection).

## Blocked

| Item                                                  | Blocked on                                                                                                                                                                                                          |
| ----------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Tagged unions (LRM 7.3.2)                             | A tagged union stores a tag plus value and is read through tagged expressions and pattern matching (LRM 11.9, 12.6). Needs tag-bit storage and the pattern-matching surface, which untagged unpacked unions do not. |
| `chandle` and dynamic-typed union members (LRM 7.3.2) | Forbidden in untagged unions by LRM 7.3.2; they ride on tagged-union support.                                                                                                                                       |

## Out of Scope

- Packed struct and union (`packed.md`, complete). Their single-vector projection is a different
  representation problem.
- Class types as struct / union members (a separate workstream).

## Cross-references

- LRM anchors: 7.2 (structures), 7.2.1 (packed structures), 7.2.2 (assigning to structures and
  member initialization), 7.3 (unions), 7.3.1 (packed unions), 7.3.2 (tagged unions), Table 7-1
  (value read from a nonexistent entry and the unpacked struct / union default), Table 6-7 (default
  initial values), 10.9 (assignment patterns), 11.9 / 12.6 (tagged-union expressions and pattern
  matching).
- Archive items: `datatypes/unpacked/{unpacked_struct, unpacked_union}`.
- The value-semantic copy of a member whose type has non-trivial copy semantics (string, real)
  follows `../decisions/value-type-concepts.md`.
- The MIR representation of an unpacked struct (the generic `TupleType`, positional member access,
  defaults synthesized at lowering rather than stored on the type) follows
  `../decisions/unpacked-struct-representation.md`.
