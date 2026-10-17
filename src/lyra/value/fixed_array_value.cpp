#include "lyra/value/fixed_array_value.hpp"

#include <cstddef>
#include <format>
#include <span>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/fixed_array_type.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/laid_out_value.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

namespace {

// An array of byte elements, the first of them the bytes `bytes` holds and the
// rest the element default (LRM 5.9).
auto FromBytes(std::string_view bytes, const FixedArrayType& type)
    -> LaidOutValue {
  const IntegralValueType* element = type.ElementType().Type().AsIntegral();
  if (element == nullptr) {
    throw InternalError(
        "fixed array: an array built from bytes holds integral "
        "elements -- please report this as a bug");
  }
  return BuiltArray(type, [&](std::size_t i, void* out) {
    WrittenWords written(element->Shape());
    if (i < bytes.size()) {
      FromInt(written.MutableView(), static_cast<unsigned char>(bytes[i]));
    } else {
      FillDefault(written.MutableView());
    }
    written.StoreTo(out);
  });
}

}  // namespace

auto ArrayFromElements(
    const FixedArrayType& type, std::span<const void* const> items)
    -> LaidOutValue {
  if (items.size() != type.Count()) {
    throw SimulationError(
        std::format(
            "a fixed-size unpacked array of {} elements cannot be assigned an "
            "array of {} (LRM 7.6)",
            type.Count(), items.size()));
  }
  return BuiltArray(type, [&](std::size_t i, void* out) {
    type.ElementType().Type().Copy(items[i], out);
  });
}

auto ArrayFromString(const FixedArrayType& type, const String& text)
    -> LaidOutValue {
  return FromBytes(text.View(), type);
}

auto ArrayFromIntegral(
    const FixedArrayType& type, const ConstIntegralView& bits) -> LaidOutValue {
  return FromBytes(BytesOf(bits), type);
}

}  // namespace lyra::value
