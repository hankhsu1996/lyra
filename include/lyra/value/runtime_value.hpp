#pragma once

#include <cstddef>
#include <variant>

#include "lyra/value/chandle.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tagged_union.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/runtime_union.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

// A type-erased runtime value: the payload one opaque JIT handle refers to. The
// active alternative is the value's current runtime domain. This is a runtime
// representation, not a compiler-IR value -- it is neither an MIR nor a LIR
// value type. It closes over the aggregate realizations, so a struct component
// or a container element may itself be an aggregate.
struct RuntimeValue {
  std::variant<
      PackedArray, String, Real, ShortReal, Chandle, Empty, RuntimeTuple,
      RuntimeUnion, RuntimeTaggedUnion, RuntimeDynamicArray,
      RuntimeUnpackedArray, RuntimeQueue, RuntimeAssociativeArray>
      value;
};

// The shared element-wise relations over two runtime values of the same domain.
// The aggregate realizations reduce their whole-value equality / change
// predicates over these, so a component or element that is itself an aggregate
// recurses through the same operations.

// LRM 11.4.5 `==`: each domain's own equality, propagating X / Z.
[[nodiscard]] auto RuntimeValueEqual(
    const RuntimeValue& a, const RuntimeValue& b) -> PackedArray;

// LRM 11.4.5 `===`: each domain's own case equality, deterministic in X / Z. A
// real / shortreal element makes the source-level `===` an error, rejected
// before lowering, so it never reaches this function.
[[nodiscard]] auto RuntimeValueCaseEqual(
    const RuntimeValue& a, const RuntimeValue& b) -> PackedArray;

// LRM 9.4.2 update-event predicate (engine change-detection hook).
[[nodiscard]] auto RuntimeValueBitIdentical(
    const RuntimeValue& a, const RuntimeValue& b) -> bool;

// Two drivers' contributions folded into one under the truth table `fold`
// names -- tri-state, wired-and, or wired-or (LRM 6.6.1 Table 6-2, LRM 6.6.3
// Tables 6-3 and 6-4); high-impedance defers under every fold. LRM 6.7.1 admits
// only some domains as a net's data type, and a value of any other reaches this
// only if something above it admitted a net it should not have.
[[nodiscard]] auto RuntimeValueResolveNet(
    const RuntimeValue& a, const RuntimeValue& b, NetResolution fold)
    -> RuntimeValue;

// What a stronger contribution leaves a weaker one: `a` determines every
// position it drives and `b` the rest (LRM 28.12.1).
[[nodiscard]] auto RuntimeValueDominating(
    const RuntimeValue& a, const RuntimeValue& b) -> RuntimeValue;

// `prototype`'s shape with every bit set to `fill`: the identity the fold above
// starts from when the fill is high-impedance, and what a net type contributes
// to its own resolution otherwise (LRM 6.7.1). Only the prototype's shape is
// read.
[[nodiscard]] auto RuntimeValueFilledLike(
    const RuntimeValue& prototype, const PackedArray& fill) -> RuntimeValue;

// The order two values of one domain sit in: lexicographic for a string,
// numerical for an integral, and for a chandle the pointer it carries, an order
// LRM 6.14 leaves free to vary between runs. It is the domain's own ordering,
// read off the values rather than supplied beside them, which is what lets a
// keyed container carry no index prototype (LRM 7.8.2, 7.8.4) and lets an
// LRM 7.12.1 locator compare keys of whatever shape a `with` clause produced.
[[nodiscard]] auto RuntimeValueOrderBefore(
    const RuntimeValue& a, const RuntimeValue& b) -> bool;

// LRM 20.9: whether the value carries any unknown bit.
[[nodiscard]] auto RuntimeValueHasUnknown(const RuntimeValue& value) -> bool;

// LRM 20.6.2 `$bits`: how many bits the value currently holds. An aggregate
// reduces over this, its bit stream being its parts' laid end to end.
[[nodiscard]] auto RuntimeValueBitstreamWidth(const RuntimeValue& value)
    -> PackedArray;

// LRM 20.9 `$countbits`: how many of the value's bits match the control set.
// An aggregate reduces over this, its bit stream being its parts' laid end to
// end.
[[nodiscard]] auto RuntimeValueCountBits(
    const RuntimeValue& value, const PackedArray& control_bits) -> PackedArray;

// LRM 6.24.3: the value's own bits, and a value of the prototype's shape read
// back from a stream of exactly that width. An aggregate reduces over these
// the way it reduces over the width above, so a part states its own bits and
// nothing above it knows the part's shape.
[[nodiscard]] auto RuntimeValueToBitstream(const RuntimeValue& value)
    -> PackedArray;
[[nodiscard]] auto RuntimeValueFromBitstream(
    const PackedArray& bits, const RuntimeValue& prototype) -> RuntimeValue;

// The element count of a container value, and the element at a position, over
// any element-container domain (queue, dynamic array, unpacked array) -- the
// erased form of the raw element access a monomorphized container exposes. A
// spread concatenation part (LRM 10.10) crosses erased, so the entry that
// appends its elements reads them without naming its domain. A value of any
// other domain reaching these is a caller that spread a non-container.
[[nodiscard]] auto RuntimeValueContainerSize(const RuntimeValue& value)
    -> std::size_t;
[[nodiscard]] auto RuntimeValueContainerElementAt(
    const RuntimeValue& value, std::size_t position) -> const RuntimeValue&;

// A keyed container's contract is over the pair of the container and the type
// its indices are, and the erased container's index type is the erased value
// itself -- which closes over that container, so the pair is only nameable
// here. Claiming it beside the monomorphized realization's claim is what makes
// one contract cover both, so the two cannot answer the same question under
// different names.
static_assert(IndexTraversal<RuntimeAssociativeArray, RuntimeValue>);

}  // namespace lyra::value
