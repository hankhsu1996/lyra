#pragma once

#include <type_traits>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// LRM 11.4.11 conditional operator. A condition that settles selects one arm
// and the other is never evaluated, which is why each arm arrives as a callable
// rather than a value. An ambiguous condition selects neither: it evaluates
// both and combines them, which a type made of parts that can agree defines for
// itself, and a type with no such parts answers with the Table 7-1 default of
// its own shape.
template <typename Then, typename Else>
[[nodiscard]] auto SelectByCondition(
    const PackedArray& condition, Then then_arm, Else else_arm) {
  const Truthiness truth = condition.Truth();
  if (truth != Truthiness::kUnknown) {
    return truth == Truthiness::kKnownNonzero ? then_arm() : else_arm();
  }
  using Value = std::invoke_result_t<Then>;
  Value then_value = then_arm();
  Value else_value = else_arm();
  if constexpr (ConditionallyMergeable<Value>) {
    return then_value.MergeConditional(else_value);
  } else if constexpr (Defaultable<Value>) {
    // A value whose default depends on its own shape -- a width, an element
    // count -- resets in place so that shape survives into the result.
    then_value.ResetToDefault();
    return then_value;
  } else {
    // A handle carries no shape: its default is null whatever it referred to.
    return Value{};
  }
}

}  // namespace lyra::value
