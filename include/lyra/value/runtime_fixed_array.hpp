#pragma once

#include <span>

#include "lyra/value/any_value.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/fixed_array_type.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/laid_out_value.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::value {

class String;

// A fixed-size unpacked array (LRM 7.4.2) as the runtime keeps one of its own.
//
// An array that holds elements keeps them where they are: an assignment gives
// each element the value of the one in the same place, so whatever names an
// element goes on naming it (LRM 13.5.2).
class RuntimeFixedArray : public LaidOutValue<RuntimeFixedArray> {
 public:
  // Holds no array: a variable's storage before its declaration installs one.
  RuntimeFixedArray() = default;

  // The array `value` holds, which is a value of an array type.
  explicit RuntimeFixedArray(AnyValue value);

  // A copy of the array of `type` laid out at `laid_out`.
  [[nodiscard]] static auto CopyOf(
      const FixedArrayType& type, const void* laid_out) -> RuntimeFixedArray;

  // An array of `type` whose element at each position `build(position,
  // element_out)` builds.
  template <typename Build>
  [[nodiscard]] static auto Built(const FixedArrayType& type, Build build)
      -> RuntimeFixedArray {
    return RuntimeFixedArray(
        AnyValue::Built(type, [&](void* out) { type.BuildEach(out, build); }));
  }

  // LRM 7.6: an array of `type` holding copies of what `items` names, in
  // order. How many elements a fixed-size array has is a declared property of
  // the variable being written, so a source of another size is a run-time
  // error and the assignment does not happen.
  [[nodiscard]] static auto FromElements(
      const FixedArrayType& type, std::span<const void* const> items)
      -> RuntimeFixedArray;

  // LRM 5.9 / 21.3.3: a string value assigned to an unpacked array of bytes is
  // left-justified -- the first character lands at the array's left bound and
  // runs toward the right bound, an element past the end of the text keeps the
  // element type's default, and text beyond the array's last element is
  // dropped. The clause admits this form for an array of bytes alone, so
  // `type`'s element is integral.
  [[nodiscard]] static auto FromString(
      const FixedArrayType& type, const String& text) -> RuntimeFixedArray;

  // LRM 5.9: a string literal assigned to an unpacked array of bytes, under the
  // same left justification. A literal is a packed bit-vector constant, not a
  // string value, so its bytes arrive whole -- a NUL among them is a byte like
  // any other, where building a string value would have removed it (LRM 6.16).
  [[nodiscard]] static auto FromIntegral(
      const FixedArrayType& type, const ConstIntegralView& bits)
      -> RuntimeFixedArray;

  [[nodiscard]] auto Type() const -> const FixedArrayType&;
};

static_assert(LyraValue<RuntimeFixedArray>);
static_assert(NetResolvable<RuntimeFixedArray>);
static_assert(CaseEqualComparable<RuntimeFixedArray>);
static_assert(BitstreamSizable<RuntimeFixedArray, ConstIntegralView>);
static_assert(TakesBytesInPlace<RuntimeFixedArray>);

}  // namespace lyra::value
