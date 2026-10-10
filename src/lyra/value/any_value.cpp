#include "lyra/value/any_value.hpp"

#include <cstdint>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/take_built.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

auto SameType(const ValueType& a, const ValueType& b) -> bool {
  if (&a == &b) {
    return true;
  }
  const IntegralValueType* x = a.AsIntegral();
  const IntegralValueType* y = b.AsIntegral();
  return x != nullptr && y != nullptr && x->Shape() == y->Shape();
}

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
      SameType(Type(), other.Type())) {
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

auto AnyValue::operator==(const AnyValue& other) const -> FourStateBit {
  return Type().Equal(Bytes(), other.Bytes());
}

auto AnyValue::operator!=(const AnyValue& other) const -> FourStateBit {
  return Inverted(*this == other);
}

auto AnyValue::CaseEqual(const AnyValue& other) const -> Bit {
  return Bit::FromBool(Type().CaseEqual(Bytes(), other.Bytes()));
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

auto AnyValue::FilledLike(const AnyValue& prototype, const Logic& fill)
    -> AnyValue {
  const ValueType& type = prototype.Type();
  return Built(
      type, [&](void* out) { type.FilledLike(prototype.Bytes(), &fill, out); });
}

auto AnyValue::IsBitIdentical(const AnyValue& other) const -> bool {
  if (bytes_ == nullptr || other.bytes_ == nullptr) {
    return (bytes_ == nullptr) == (other.bytes_ == nullptr);
  }
  return SameType(Type(), other.Type()) &&
         Type().BitIdentical(Bytes(), other.Bytes());
}

// A holder with no value has no bits, so none of them is unknown.
auto AnyValue::HasUnknown() const -> bool {
  return bytes_ != nullptr && Type().HasUnknown(Bytes());
}

auto AnyValue::IsUnknown() const -> Bit {
  return Bit::FromBool(HasUnknown());
}

auto AnyValue::BitstreamWidth() const -> Int {
  return TakeBuilt<Int>(
      [&](void* out) { Type().BitstreamWidth(Bytes(), out); });
}

auto AnyValue::CountBits(const ConstIntegralView& control_bits) const -> Int {
  return TakeBuilt<Int>([&](void* out) {
    Type().CountBits(Bytes(), control_bits.planes, control_bits.width, out);
  });
}

auto AnyValue::WriteToStream(
    Planes stream, std::uint64_t stream_width, std::uint64_t filled) const
    -> std::uint64_t {
  return Type().WriteToStream(Bytes(), stream, stream_width, filled);
}

auto AnyValue::ReadFromStream(
    ConstPlanes stream, std::uint64_t stream_width, std::uint64_t taken) const
    -> std::pair<AnyValue, std::uint64_t> {
  const ValueType& type = Type();
  std::uint64_t after = taken;
  AnyValue read = Built(type, [&](void* out) {
    after = type.ReadFromStream(stream, stream_width, taken, Bytes(), out);
  });
  return {std::move(read), after};
}

}  // namespace lyra::value
