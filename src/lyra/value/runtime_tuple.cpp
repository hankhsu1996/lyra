#include "lyra/value/runtime_tuple.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
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
    throw InternalError("RuntimeTuple: a component index is out of range");
  }
  return stated[index];
}

}  // namespace

RuntimeTuple::RuntimeTuple(AnyValue value) : value_(std::move(value)) {
}

auto RuntimeTuple::CopyOf(const void* laid_out) -> RuntimeTuple {
  return RuntimeTuple(AnyValue::CopyOf(TypeAt(laid_out), laid_out));
}

auto RuntimeTuple::Bytes() const -> const void* {
  return value_.Bytes();
}

auto RuntimeTuple::Bytes() -> void* {
  return value_.Bytes();
}

auto RuntimeTuple::TypeAt(const void* laid_out) -> const TupleType& {
  return **static_cast<const TupleType* const*>(laid_out);
}

auto RuntimeTuple::ComponentAt(void* laid_out, std::size_t index) -> void* {
  return At(laid_out, Stated(TypeAt(laid_out), index).offset);
}

auto RuntimeTuple::ComponentAt(const void* laid_out, std::size_t index) -> const
    void* {
  return At(laid_out, Stated(TypeAt(laid_out), index).offset);
}

auto RuntimeTuple::BitIdentical(const void* lhs, const void* rhs) -> bool {
  return TypeAt(lhs).BitIdentical(lhs, rhs);
}

void RuntimeTuple::AssignAt(void* storage, const void* value) {
  TypeAt(storage).Assign(storage, value);
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

auto RuntimeTuple::Type() const -> const TupleType& {
  if (Bytes() == nullptr) {
    throw InternalError(
        "RuntimeTuple: a tuple is read from storage that holds none -- please "
        "report this as a bug");
  }
  return TypeAt(Bytes());
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
    Planes stream, std::uint64_t stream_width, std::uint64_t filled) const
    -> std::uint64_t {
  return value_.WriteToStream(stream, stream_width, filled);
}

auto RuntimeTuple::ReadFromStream(
    ConstPlanes stream, std::uint64_t stream_width, std::uint64_t taken) const
    -> std::pair<RuntimeTuple, std::uint64_t> {
  auto [read, after] = value_.ReadFromStream(stream, stream_width, taken);
  return {RuntimeTuple(std::move(read)), after};
}

}  // namespace lyra::value
