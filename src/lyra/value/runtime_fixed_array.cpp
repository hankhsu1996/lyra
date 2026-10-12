#include "lyra/value/runtime_fixed_array.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/fixed_array_type.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

namespace {

// An array of byte elements, the first of them the bytes `bytes` holds and the
// rest the element default (LRM 5.9).
auto FromBytes(std::string_view bytes, const FixedArrayType& type)
    -> RuntimeFixedArray {
  const IntegralValueType* element = type.ElementType().Type().AsIntegral();
  if (element == nullptr) {
    throw InternalError(
        "RuntimeFixedArray: an array built from bytes holds integral "
        "elements -- please report this as a bug");
  }
  return RuntimeFixedArray::Built(type, [&](std::size_t i, void* out) {
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

RuntimeFixedArray::RuntimeFixedArray(AnyValue value)
    : value_(std::move(value)) {
}

auto RuntimeFixedArray::CopyOf(const FixedArrayType& type, const void* laid_out)
    -> RuntimeFixedArray {
  return RuntimeFixedArray(AnyValue::CopyOf(type, laid_out));
}

auto RuntimeFixedArray::FromElements(
    const FixedArrayType& type, std::span<const void* const> items)
    -> RuntimeFixedArray {
  if (items.size() != type.Count()) {
    throw SimulationError(
        std::format(
            "a fixed-size unpacked array of {} elements cannot be assigned an "
            "array of {} (LRM 7.6)",
            type.Count(), items.size()));
  }
  return Built(type, [&](std::size_t i, void* out) {
    type.ElementType().Type().Copy(items[i], out);
  });
}

auto RuntimeFixedArray::FromString(
    const FixedArrayType& type, const String& text) -> RuntimeFixedArray {
  return FromBytes(text.View(), type);
}

auto RuntimeFixedArray::FromIntegral(
    const FixedArrayType& type, const ConstIntegralView& bits)
    -> RuntimeFixedArray {
  return FromBytes(BytesOf(bits), type);
}

auto RuntimeFixedArray::Bytes() const -> const void* {
  return value_.Bytes();
}

auto RuntimeFixedArray::Bytes() -> void* {
  return value_.Bytes();
}

auto RuntimeFixedArray::CopyInto(void* out) const -> void* {
  Type().Copy(Bytes(), out);
  return out;
}

auto RuntimeFixedArray::MoveInto(void* out) && -> void* {
  Type().Move(Bytes(), out);
  value_ = AnyValue();
  return out;
}

auto RuntimeFixedArray::IntoAnyValue() && -> AnyValue {
  return std::exchange(value_, AnyValue());
}

auto RuntimeFixedArray::Type() const -> const FixedArrayType& {
  const FixedArrayType* type = value_.Type().AsFixedArray();
  if (type == nullptr) {
    throw InternalError(
        "RuntimeFixedArray: an array holds a value of another kind -- please "
        "report this as a bug");
  }
  return *type;
}

auto RuntimeFixedArray::HoldsBytes(const void* bytes) const -> bool {
  return Type().BitIdentical(Bytes(), bytes);
}

void RuntimeFixedArray::TakeBytes(const void* bytes) {
  Type().Assign(Bytes(), bytes);
}

auto RuntimeFixedArray::Holding(const void* bytes) const -> RuntimeFixedArray {
  return CopyOf(Type(), bytes);
}

auto RuntimeFixedArray::operator==(const RuntimeFixedArray& other) const
    -> FourStateBit {
  return value_ == other.value_;
}

auto RuntimeFixedArray::operator!=(const RuntimeFixedArray& other) const
    -> FourStateBit {
  return value_ != other.value_;
}

auto RuntimeFixedArray::CaseEqual(const RuntimeFixedArray& other) const -> Bit {
  return value_.CaseEqual(other.value_);
}

auto RuntimeFixedArray::ResolveTriState(const RuntimeFixedArray& other) const
    -> RuntimeFixedArray {
  return RuntimeFixedArray(value_.ResolveTriState(other.value_));
}

auto RuntimeFixedArray::ResolveWiredAnd(const RuntimeFixedArray& other) const
    -> RuntimeFixedArray {
  return RuntimeFixedArray(value_.ResolveWiredAnd(other.value_));
}

auto RuntimeFixedArray::ResolveWiredOr(const RuntimeFixedArray& other) const
    -> RuntimeFixedArray {
  return RuntimeFixedArray(value_.ResolveWiredOr(other.value_));
}

auto RuntimeFixedArray::Dominating(const RuntimeFixedArray& weaker) const
    -> RuntimeFixedArray {
  return RuntimeFixedArray(value_.Dominating(weaker.value_));
}

auto RuntimeFixedArray::FilledLike(
    const RuntimeFixedArray& prototype, const Logic& fill)
    -> RuntimeFixedArray {
  return RuntimeFixedArray(AnyValue::FilledLike(prototype.value_, fill));
}

auto RuntimeFixedArray::IsBitIdentical(const RuntimeFixedArray& other) const
    -> bool {
  return value_.IsBitIdentical(other.value_);
}

auto RuntimeFixedArray::HasUnknown() const -> bool {
  return value_.HasUnknown();
}

auto RuntimeFixedArray::IsUnknown() const -> Bit {
  return value_.IsUnknown();
}

auto RuntimeFixedArray::BitstreamWidth() const -> Int {
  return value_.BitstreamWidth();
}

auto RuntimeFixedArray::CountBits(const ConstIntegralView& control_bits) const
    -> Int {
  return value_.CountBits(control_bits);
}

auto RuntimeFixedArray::WriteToStream(
    const IntegralView& stream, std::uint64_t filled) const -> std::uint64_t {
  return value_.WriteToStream(stream, filled);
}

}  // namespace lyra::value
