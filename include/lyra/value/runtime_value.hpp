#pragma once

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

// LRM 6.6.1 Table 6-2: two drivers' contributions folded into one, where
// agreement passes through, a 0/1 conflict yields `x`, and high-impedance
// defers. LRM 6.7.1 admits only some domains as a net's data type, and a value
// of any other reaches this only if something above it admitted a net it should
// not have.
[[nodiscard]] auto RuntimeValueResolveTriState(
    const RuntimeValue& a, const RuntimeValue& b) -> RuntimeValue;

// The all-high-impedance value at `prototype`'s shape: what a driver
// contributes where it is not driving, and so the identity the fold above
// starts from. Only the prototype's shape is read.
[[nodiscard]] auto RuntimeValueHighImpedanceLike(const RuntimeValue& prototype)
    -> RuntimeValue;

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

// A keyed container's contract is over the pair of the container and the type
// its indices are, and the erased container's index type is the erased value
// itself -- which closes over that container, so the pair is only nameable
// here. Claiming it beside the monomorphized realization's claim is what makes
// one contract cover both, so the two cannot answer the same question under
// different names.
static_assert(IndexTraversal<RuntimeAssociativeArray, RuntimeValue>);

}  // namespace lyra::value
