#pragma once

#include <variant>

#include "lyra/value/chandle.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

// A type-erased runtime value: the payload one opaque JIT handle refers to. The
// active alternative is the value's current runtime domain. This is a runtime
// representation, not a compiler-IR value -- it is neither an MIR nor a LIR
// value type. It closes over the aggregate realizations (`RuntimeTuple`,
// `RuntimeDynamicArray`), so a struct component or an array element may itself
// be an aggregate.
struct RuntimeValue {
  std::variant<
      PackedArray, String, Real, ShortReal, Chandle, RuntimeTuple,
      RuntimeDynamicArray>
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

// LRM 20.9: whether the value carries any unknown bit.
[[nodiscard]] auto RuntimeValueHasUnknown(const RuntimeValue& value) -> bool;

}  // namespace lyra::value
