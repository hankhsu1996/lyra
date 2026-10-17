#pragma once

#include <span>

#include "lyra/value/any_value.hpp"
#include "lyra/value/fixed_array_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/laid_out_value.hpp"

namespace lyra::value {

class String;

// An array of `type` whose element at each position `build(position,
// element_out)` builds.
template <typename Build>
[[nodiscard]] auto BuiltArray(const FixedArrayType& type, Build build)
    -> LaidOutValue {
  return LaidOutValue(AnyValue::Built(type.Type(), [&](void* out) {
    type.BuildEach(out, build);
  }));
}

// LRM 7.6: an array of `type` holding copies of what `items` names, in order.
// How many elements a fixed-size array has is a declared property of the
// variable being written, so a source of another size is a run-time error and
// the assignment does not happen.
[[nodiscard]] auto ArrayFromElements(
    const FixedArrayType& type, std::span<const void* const> items)
    -> LaidOutValue;

// LRM 5.9 / 21.3.3: a string value assigned to an unpacked array of bytes is
// left-justified -- the first character lands at the array's left bound and
// runs toward the right bound, an element past the end of the text keeps the
// element type's default, and text beyond the array's last element is
// dropped. The clause admits this form for an array of bytes alone, so
// `type`'s element is integral.
[[nodiscard]] auto ArrayFromString(
    const FixedArrayType& type, const String& text) -> LaidOutValue;

// LRM 5.9: a string literal assigned to an unpacked array of bytes, under the
// same left justification. A literal is a packed bit-vector constant, not a
// string value, so its bytes arrive whole -- a NUL among them is a byte like
// any other, where building a string value would have removed it (LRM 6.16).
[[nodiscard]] auto ArrayFromIntegral(
    const FixedArrayType& type, const ConstIntegralView& bits) -> LaidOutValue;

}  // namespace lyra::value
