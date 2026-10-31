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

auto AnyValue::IsBitIdentical(const AnyValue& other) const -> bool {
  if (bytes_ == nullptr || other.bytes_ == nullptr) {
    return (bytes_ == nullptr) == (other.bytes_ == nullptr);
  }
  return SameType(Type(), other.Type()) &&
         Type().BitIdentical(Bytes(), other.Bytes());
}

}  // namespace lyra::value
