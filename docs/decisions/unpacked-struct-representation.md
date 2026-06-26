# Unpacked struct is a generic product type (MIR `TupleType`)

Date: 2026-06-25 Status: accepted

## Why this decision matters

HIR carries `UnpackedStructType` (LRM 7.2) as a faithful source construct. HIR-to-MIR must give it a
generic programming-language representation or reject it. MIR already has two aggregate homes that
could plausibly host it: `TupleType`, the heterogeneous product type (the Rust / Python tuple, C++
`std::tuple` / `std::pair`), and `ObjectType`, the nominal object the model uses for a SystemVerilog
class. A comment in `mir/type.hpp` anticipated a third option -- a new MIR variant minted just for
unpacked struct / union. This entry settles the choice and records the alternatives it was made
over, because the "give it its own variant" option is the one a future reader will re-propose.

## The axis that decides it: value versus reference

A SystemVerilog unpacked struct is a **value**: whole-struct assignment copies (LRM 7.2.2 "A
structure can be assigned as a whole and passed to or from a subroutine as a whole"), and an
uninitialized struct's default is a member-wise constructed value (LRM Table 7-1), not `null`. It
has no handle, no null, no identity, and no aliasing. A SystemVerilog `class`, by contrast, is a
managed reference (LRM 8.3, see [object-model](object-model.md)): null is legal, identity is
comparable, copies are shallow. Table 7-1 states the contrast directly -- a struct defaults to a
value, a class defaults to `null`.

MIR's type system already separates these two: `TupleType` is the value product ("MIR's only
heterogeneous aggregate", `mir.md`), and `ObjectType` is reached only through a pointer with managed
/ owning / borrowed ownership. The value semantics of an unpacked struct put it with the value
product, not the object model.

## Decision

1. **HIR `UnpackedStructType` lowers to MIR `TupleType`**, whose component types are the struct's
   member types in declaration order. No new MIR type variant is introduced; the `mir/type.hpp`
   "future unpacked variant" note is retired. This reinforces `mir.md`'s invariant that the tuple is
   MIR's single heterogeneous product type.

2. **Member access is positional, by declaration-order index.** Field names are dropped at
   HIR-to-MIR, exactly as a packed struct drops its fields to bit offsets. The index is the carrier
   (`MemberAccessExpr.field_index` in HIR, `TupleGetExpr.index` in MIR). Field names are not MIR
   execution-type identity; any source / debug presentation that needs them (`%p`, `$typename`)
   reads them from separate source-schema metadata, never from a field on `TupleType`.

3. **Default initialization is synthesized at HIR-to-MIR as an ordered product literal, never stored
   on the type.** Per-member defaults (LRM Table 7-1, with a member's own declaration initializer
   taking precedence per LRM 7.2.2) are lowered into a `TupleExpr` at each site that default-
   constructs the struct. The interned `TupleType` carries component types only: two source structs
   with identical member types but different member initializers share one `TupleType`, so storing
   defaults on the type would break canonicalization and leak source-level initialization into
   generic type identity.

4. **The runtime realization is one generic `lyra::value::Tuple<Ts...>`** that satisfies `LyraValue`
   by composing its components' capabilities -- member-wise `==` / `!=`, member-wise
   `IsBitIdentical`, member-wise default -- the same way `UnpackedArray<T>` composes its element's
   contract (see [value-type-concepts](value-type-concepts.md)). Tuple copy is a shallow copy of the
   component representations; a component that owns variable-size storage (`String`, `DynamicArray`,
   `Queue`) carries its own copy / copy-on-write semantics internally, so a write to one struct
   field does not force a deep clone of an unrelated container sibling. There is one realization for
   both SV struct storage and the transient products MIR already builds with `TupleType` (the
   associative `(key, value)` pair, the task-completion output pack); the transient uses satisfy the
   contract for free.

5. **A module-level struct signal is observable whole-cell.** HIR-to-MIR wraps it as
   `ObservableType{TupleType}`, rendered `Var<Tuple<...>>`, reacting under `wait` / `always_comb` /
   `@*` / `@(s)` as a whole value -- identical to how the variable-size aggregates (dynamic array,
   queue, associative array) already behave. Field-granular reactivity (waking only the processes
   sensitive to a written field) is a separate, cross-aggregate concern that applies equally to
   unpacked arrays and is **not** part of this decision.

## Rejected alternatives

- **A distinct `StructType` MIR variant.** An unpacked struct and a tuple are the same
  generic-language concept: a heterogeneous value product. The only thing a struct needs beyond a
  transient product -- the `LyraValue` runtime contract -- is a realization concern, satisfied by
  the composing `Tuple<Ts...>` above, not a reason to mint a second structural type. Two
  near-identical product types would violate `mir.md`'s "one heterogeneous product" identity and
  split member-access / construction lowering across two shapes for no semantic gain.

- **The object model (`ObjectType`).** That is the reference machinery: a managed handle reached
  through a pointer, with null, identity, and shallow-handle copy. A struct is a value (LRM 7.2.2,
  Table 7-1). Hosting it on `ObjectType` would either silently give it reference semantics -- so
  `x = s` would alias instead of copy, contradicting LRM 7.2.2 -- or force a "value object" special
  case onto a type whose entire purpose is to model references. It also collapses the
  value/reference axis the type system deliberately keeps separate.

## Consequences

- `mir.md`'s contract that `TupleType` is MIR's only heterogeneous aggregate is reinforced; no
  architecture-doc change is needed.
- `struct-union.md` S1-S5 build on this representation; the whole-cell observability matches the
  container model recorded in `aggregate.md`.
- New runtime work: the generic `lyra::value::Tuple<Ts...>` value type, composing `LyraValue` from
  its components.
- **Union is out of scope here and not pre-built for.** A tagged union (LRM 7.3.2) is a sum type --
  a genuinely new generic-language concept MIR does not yet have -- and may use `TupleType` for a
  multi-field payload when that work lands; an untagged unpacked union (LRM 7.3) is overlapping
  storage, neither a product nor a sum. Neither shares the struct representation, and this decision
  does not anticipate their shape.

## Cross-references

- `../architecture/mir.md` -- `TupleType` as the single heterogeneous product type; the value-type /
  object-type split.
- [value-type-concepts](value-type-concepts.md) -- the `LyraValue` concept lattice the runtime
  `Tuple<Ts...>` composes.
- [object-model](object-model.md) -- the managed-reference object model an unpacked struct is
  deliberately not part of.
- [unpacked-array-representation](unpacked-array-representation.md) -- the sibling value-aggregate
  representation whose composition pattern `Tuple<Ts...>` mirrors.
- LRM anchors: 7.2 (structures), 7.2.2 (assigning to structures, member initialization), Table 7-1
  (unpacked struct default), 8.3 (class handles), 11.4.5 (equality on any data type).
