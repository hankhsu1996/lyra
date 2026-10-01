# Unpacked struct is a nominal product type (MIR `StructType`)

Date: 2026-06-25 (revised 2026-09-29). Status: accepted. The first form lowered an unpacked struct
to the structural `TupleType`, and a later one gave that tuple an optional declaration to carry the
struct's identity and methods. This revision makes the struct a product type of its own, nominal
where the tuple is structural; points 2, 3 and 5 stand as they were.

## Why this decision matters

HIR carries `UnpackedStructType` (LRM 7.2) as a faithful source construct. HIR-to-MIR must give it a
generic programming-language representation. MIR has three aggregate homes that could plausibly host
it: the structural product `TupleType` (the Rust / Python tuple, C++ `std::tuple`), a nominal
product `StructType` (the Rust / Swift / C struct), and `ObjectType`, the nominal object the model
uses for a SystemVerilog class. This entry settles the choice and records the alternatives it was
made over, because each of the other two has been the answer once.

## The two axes that decide it

**Value versus reference.** A SystemVerilog unpacked struct is a **value**: whole-struct assignment
copies (LRM 7.2.2 "A structure can be assigned as a whole and passed to or from a subroutine as a
whole"), and an uninitialized struct's default is a member-wise constructed value (LRM Table 7-1),
not `null`. It has no handle, no null, no identity of storage, and no aliasing. A SystemVerilog
`class`, by contrast, is a managed reference (LRM 8.3, see [object-model](object-model.md)). That
puts the struct with the products, not with the object model.

**Structural versus nominal.** Two unpacked struct types with the same members are still two types:
an unpacked structure matches only itself (LRM 6.22.1) and is equivalent only to itself (LRM
6.22.2), and a whole-value operation such as `==` is defined on the declaration's type (LRM 11.4.5).
A tuple is the opposite: two with the same components are one type. That puts the struct with the
nominal product, and it is the axis every language that has both draws the same way -- Rust's `Adt`
and `Tuple`, LLVM's identified and literal `StructType` (read in `llvm/IR/DerivedTypes.h`).

## Decision

1. **HIR `UnpackedStructType` lowers to MIR `StructType`**, naming the struct's declaration: this
   unit's own by its position in the unit's struct registry, another unit's by that unit's name and
   the struct's name there. The declaration lists the member types in declaration order and a method
   for each operation the language defines on the whole value
   ([a-structures-operations-are-stated-in-mir](a-structures-operations-are-stated-in-mir.md)).

2. **Member access is positional, by declaration-order index**, with the part operation every
   product is reached by -- a tuple's component and a struct's member alike. Member names are
   dropped at HIR-to-MIR, exactly as a packed struct drops its members to bit offsets; what the
   language reads a name for (`%p`, LRM 21.2.1.6) is computation built from the source type before
   MIR.

3. **Default initialization is synthesized at HIR-to-MIR as an ordered product literal, never stored
   on the type.** Per-member defaults (LRM Table 7-1, with a member's own declaration initializer
   taking precedence per LRM 7.2.2) are composed into a value at each site that default-constructs
   the struct.

4. **Each backend realizes a struct as it realizes a product.** The C++ backend emits a struct of
   the struct's name built on the library's `lyra::value::Tuple<Ts...>`, whose member functions are
   the declaration's methods; the execution backend lays it out as it lays out a tuple, and its
   table carries the methods
   ([a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md)).

5. **A module-level struct signal is observable whole-cell**, reacting under `wait` / `always_comb`
   / `@*` / `@(s)` as a whole value -- identical to how the variable-size aggregates already behave.
   Field-granular reactivity is a separate, cross-aggregate concern and is **not** part of this
   decision.

## Rejected alternatives

- **The structural `TupleType`.** Makes two struct declarations with the same members one type,
  which LRM 6.22.1 forbids, and leaves nowhere to state the struct's methods. The revision that
  added an optional declaration to the tuple kept the tuple's name over a nominal type: identity
  became a field some tuples had, every consumer asked which kind of tuple it held, and the
  declaration's own members were never listed anywhere a unit other than the declaring one could
  read.

- **The object model (`ObjectType`).** That is the reference machinery: a managed handle reached
  through a pointer, with null, identity, and shallow-handle copy. A struct is a value (LRM 7.2.2,
  Table 7-1). Hosting it on `ObjectType` would either silently give it reference semantics -- so
  `x = s` would alias instead of copy -- or force a "value object" special case onto a type whose
  entire purpose is to model references.

## Consequences

- The tuple remains the structural product: what a lowering composes for itself, such as a call's
  completion payload or an associative entry.
- **Union is out of scope here.** A tagged union (LRM 7.3.2) is a sum type and an untagged unpacked
  union (LRM 7.3) is overlapping storage; neither shares the struct representation.

## Cross-references

- `../architecture/mir.md` -- the tuple and the struct as the two products; the value-type /
  object-type split.
- [a-structures-operations-are-stated-in-mir](a-structures-operations-are-stated-in-mir.md) -- the
  struct's declaration states its whole-value operations as methods.
- [a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md) -- how the execution backend
  lays a product out.
- [object-model](object-model.md) -- the managed-reference object model a struct is deliberately not
  part of.
- LRM anchors: 6.22.1 (matching types), 6.22.2 (equivalent types), 7.2 (structures), 7.2.2
  (assigning to structures, member initialization), Table 7-1 (unpacked struct default), 8.3 (class
  handles), 11.4.5 (equality on any data type).
