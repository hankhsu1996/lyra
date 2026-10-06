#include "lyra/value/any_value.hpp"

#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

auto AnyValue::CopyOf(const ValueType& type, const void* value) -> AnyValue {
  return Built(type, [&](void* out) { type.Copy(value, out); });
}

AnyValue::AnyValue(const AnyValue& other) {
  if (other.bytes_ != nullptr) {
    *this = CopyOf(other.Type(), other.Bytes());
  }
}

auto AnyValue::operator=(const AnyValue& other) -> AnyValue& {
  if (this == &other) {
    return *this;
  }
  // Two values of one type assign part by part, into the storage this already
  // owns.
  if (bytes_ != nullptr && other.bytes_ != nullptr &&
      &Type() == &other.Type()) {
    Type().Assign(Bytes(), other.Bytes());
    return *this;
  }
  AnyValue copy(other);
  return *this = std::move(copy);
}

auto AnyValue::Type() const -> const ValueType& {
  if (bytes_ == nullptr) {
    throw InternalError(
        "AnyValue: a value is read from storage that holds none -- please "
        "report this as a bug");
  }
  return *bytes_.get_deleter().type;
}

auto AnyValue::operator==(const AnyValue& other) const -> PackedArray {
  return TakeBuilt<PackedArray>(
      [&](void* out) { Type().Equal(Bytes(), other.Bytes(), out); });
}

auto AnyValue::operator!=(const AnyValue& other) const -> PackedArray {
  return !(*this == other);
}

auto AnyValue::CaseEqual(const AnyValue& other) const -> PackedArray {
  return TakeBuilt<PackedArray>(
      [&](void* out) { Type().CaseEqual(Bytes(), other.Bytes(), out); });
}

auto AnyValue::FoldedBy(Fold fold, const AnyValue& other) const -> AnyValue {
  return Built(
      Type(), [&](void* out) { (Type().*fold)(Bytes(), other.Bytes(), out); });
}

auto AnyValue::ResolveTriState(const AnyValue& other) const -> AnyValue {
  return FoldedBy(&ValueType::ResolveTriState, other);
}

auto AnyValue::ResolveWiredAnd(const AnyValue& other) const -> AnyValue {
  return FoldedBy(&ValueType::ResolveWiredAnd, other);
}

auto AnyValue::ResolveWiredOr(const AnyValue& other) const -> AnyValue {
  return FoldedBy(&ValueType::ResolveWiredOr, other);
}

auto AnyValue::Dominating(const AnyValue& weaker) const -> AnyValue {
  return FoldedBy(&ValueType::Dominating, weaker);
}

auto AnyValue::FilledLike(const AnyValue& prototype, const PackedArray& fill)
    -> AnyValue {
  const ValueType& type = prototype.Type();
  return Built(
      type, [&](void* out) { type.FilledLike(prototype.Bytes(), &fill, out); });
}

auto AnyValue::IsBitIdentical(const AnyValue& other) const -> bool {
  if (bytes_ == nullptr || other.bytes_ == nullptr) {
    return (bytes_ == nullptr) == (other.bytes_ == nullptr);
  }
  return &Type() == &other.Type() &&
         Type().BitIdentical(Bytes(), other.Bytes());
}

// A holder with no value has no bits, so none of them is unknown.
auto AnyValue::HasUnknown() const -> bool {
  return bytes_ != nullptr && Type().HasUnknown(Bytes());
}

auto AnyValue::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

auto AnyValue::BitstreamWidth() const -> PackedArray {
  return TakeBuilt<PackedArray>(
      [&](void* out) { Type().BitstreamWidth(Bytes(), out); });
}

auto AnyValue::CountBits(const PackedArray& control_bits) const -> PackedArray {
  return TakeBuilt<PackedArray>(
      [&](void* out) { Type().CountBits(Bytes(), &control_bits, out); });
}

auto AnyValue::ToBitstream() const -> PackedArray {
  return TakeBuilt<PackedArray>(
      [&](void* out) { Type().ToBitstream(Bytes(), out); });
}

auto AnyValue::FromBitstream(const PackedArray& bits, const AnyValue& prototype)
    -> AnyValue {
  const ValueType& type = prototype.Type();
  return Built(type, [&](void* out) {
    type.FromBitstream(&bits, prototype.Bytes(), out);
  });
}

}  // namespace lyra::value
