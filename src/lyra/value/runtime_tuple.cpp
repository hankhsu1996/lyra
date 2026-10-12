#include "lyra/value/runtime_tuple.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <span>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::value {

namespace {

// Where a component lies, `offset` bytes into its tuple.
auto At(void* tuple, std::uint32_t offset) -> void* {
  return std::bit_cast<void*>(std::bit_cast<std::uintptr_t>(tuple) + offset);
}
auto At(const void* tuple, std::uint32_t offset) -> const void* {
  return std::bit_cast<const void*>(
      std::bit_cast<std::uintptr_t>(tuple) + offset);
}

// The component `index` a tuple of `type` lists.
auto Stated(const TupleType& type, std::size_t index) -> const TupleComponent& {
  const std::span<const TupleComponent> stated = type.Components();
  if (index >= stated.size()) {
    throw InternalError("TupleType: a component index is out of range");
  }
  return stated[index];
}

}  // namespace

auto TupleType::ComponentAt(void* laid_out, std::size_t index) const -> void* {
  return At(laid_out, Stated(*this, index).offset);
}

auto TupleType::ComponentAt(const void* laid_out, std::size_t index) const
    -> const void* {
  return At(laid_out, Stated(*this, index).offset);
}

void TupleType::BuildEachComponentDefault(void* laid_out) const {
  const std::span<const TupleComponent> stated = Components();
  std::size_t built = 0;
  const auto end_built = [&](const std::size_t* count) {
    for (std::size_t i = 0; i < *count; ++i) {
      stated[i].type->Destroy(ComponentAt(laid_out, i));
    }
  };
  std::unique_ptr<const std::size_t, decltype(end_built)> unwinding(
      &built, end_built);
  for (; built < stated.size(); ++built) {
    stated[built].type->BuildDefault(ComponentAt(laid_out, built));
  }
  unwinding.release();
}

RuntimeTuple::RuntimeTuple(AnyValue value) : value_(std::move(value)) {
}

auto RuntimeTuple::CopyOf(const TupleType& type, const void* laid_out)
    -> RuntimeTuple {
  return RuntimeTuple(AnyValue::CopyOf(type, laid_out));
}

auto RuntimeTuple::Bytes() const -> const void* {
  return value_.Bytes();
}

auto RuntimeTuple::Bytes() -> void* {
  return value_.Bytes();
}

auto RuntimeTuple::Type() const -> const ValueType& {
  return value_.Type();
}

auto RuntimeTuple::IsBitIdenticalTo(const void* bytes) const -> bool {
  return Type().BitIdentical(Bytes(), bytes);
}

void RuntimeTuple::AssignBytes(const void* bytes) {
  Type().Assign(Bytes(), bytes);
}

auto RuntimeTuple::CopyInto(void* out) const -> void* {
  Type().Copy(Bytes(), out);
  return out;
}

auto RuntimeTuple::MoveInto(void* out) && -> void* {
  Type().Move(Bytes(), out);
  value_ = AnyValue();
  return out;
}

auto RuntimeTuple::operator==(const RuntimeTuple& other) const -> FourStateBit {
  return value_ == other.value_;
}

auto RuntimeTuple::operator!=(const RuntimeTuple& other) const -> FourStateBit {
  return value_ != other.value_;
}

auto RuntimeTuple::CaseEqual(const RuntimeTuple& other) const -> Bit {
  return value_.CaseEqual(other.value_);
}

auto RuntimeTuple::ResolveTriState(const RuntimeTuple& other) const
    -> RuntimeTuple {
  return RuntimeTuple(value_.ResolveTriState(other.value_));
}

auto RuntimeTuple::ResolveWiredAnd(const RuntimeTuple& other) const
    -> RuntimeTuple {
  return RuntimeTuple(value_.ResolveWiredAnd(other.value_));
}

auto RuntimeTuple::ResolveWiredOr(const RuntimeTuple& other) const
    -> RuntimeTuple {
  return RuntimeTuple(value_.ResolveWiredOr(other.value_));
}

auto RuntimeTuple::Dominating(const RuntimeTuple& weaker) const
    -> RuntimeTuple {
  return RuntimeTuple(value_.Dominating(weaker.value_));
}

auto RuntimeTuple::FilledLike(const RuntimeTuple& prototype, const Logic& fill)
    -> RuntimeTuple {
  return RuntimeTuple(AnyValue::FilledLike(prototype.value_, fill));
}

auto RuntimeTuple::IsBitIdentical(const RuntimeTuple& other) const -> bool {
  return value_.IsBitIdentical(other.value_);
}

auto RuntimeTuple::HasUnknown() const -> bool {
  return value_.HasUnknown();
}

auto RuntimeTuple::IsUnknown() const -> Bit {
  return value_.IsUnknown();
}

auto RuntimeTuple::BitstreamWidth() const -> Int {
  return value_.BitstreamWidth();
}

auto RuntimeTuple::CountBits(const ConstIntegralView& control_bits) const
    -> Int {
  return value_.CountBits(control_bits);
}

auto RuntimeTuple::WriteToStream(
    const IntegralView& stream, std::uint64_t filled) const -> std::uint64_t {
  return value_.WriteToStream(stream, filled);
}

}  // namespace lyra::value
