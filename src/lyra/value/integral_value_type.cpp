#include "lyra/value/integral_value_type.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <memory>
#include <utility>

#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

template <typename Fold>
void IntegralValueType::Folded(
    const void* lhs, const void* rhs, void* out, Fold fold) const {
  const LoadedWords a = Load(lhs);
  const LoadedWords b = Load(rhs);
  LoadedWords result(shape_);
  fold(result.Write(), a.Read(), b.Read());
  result.StoreTo(out);
}

auto IntegralValueType::ShapeOffset() const -> std::size_t {
  return std::size_t{
      std::bit_cast<std::uintptr_t>(&shape_) -
      std::bit_cast<std::uintptr_t>(this)};
}

void IntegralValueType::Copy(const void* value, void* out) const {
  std::memcpy(out, value, Size());
}

void IntegralValueType::Move(void* value, void* out) const noexcept {
  std::memcpy(out, value, Size());
}

// A value is its bytes, so ending one is ending them, which leaves nothing to
// run.
void IntegralValueType::Destroy(void* value) const noexcept {
  std::destroy_n(static_cast<std::byte*>(value), Size());
}

void IntegralValueType::Assign(void* storage, const void* value) const {
  std::memmove(storage, value, Size());
}

auto IntegralValueType::Equal(const void* lhs, const void* rhs) const
    -> FourStateBit {
  return lyra::value::Equal(Load(lhs).Read(), Load(rhs).Read());
}

auto IntegralValueType::CaseEqual(const void* lhs, const void* rhs) const
    -> bool {
  return BitIdentical(lhs, rhs);
}

// Every position above the width is clear in both planes, so two values are
// the same bits exactly where their bytes are.
auto IntegralValueType::BitIdentical(const void* lhs, const void* rhs) const
    -> bool {
  return std::memcmp(lhs, rhs, Size()) == 0;
}

auto IntegralValueType::HasUnknown(const void* value) const -> bool {
  return lyra::value::HasUnknown(Load(value).Read());
}

// LRM 20.6.2, 20.9, 6.24.3: an integral value's stream of bits is its own bits,
// read unsigned.
void IntegralValueType::BitstreamWidth(const void*, void* out) const {
  std::construct_at(
      static_cast<Int*>(out),
      Int::FromInt(static_cast<std::int64_t>(shape_.width)));
}

void IntegralValueType::CountBits(
    const void* value, const ConstPlanes& control_bits,
    std::uint64_t control_width, void* out) const {
  std::construct_at(
      static_cast<Int*>(out),
      Int::FromInt(
          lyra::value::CountBits(
              Load(value).Read(), shape_.width, control_bits, control_width)));
}

auto IntegralValueType::WriteToStream(
    const void* value, const Planes& stream, std::uint64_t stream_width,
    std::uint64_t filled) const -> std::uint64_t {
  Insert(
      stream, stream_width, Load(value).Read(), shape_.width,
      static_cast<std::int64_t>(stream_width - filled - shape_.width));
  return filled + shape_.width;
}

// A two-state type reads each x or z of the stream as 0 (LRM 11.4.14.3).
auto IntegralValueType::ReadFromStream(
    const ConstPlanes& stream, std::uint64_t stream_width, std::uint64_t taken,
    const void*, void* out) const -> std::uint64_t {
  LoadedWords bits(shape_);
  Extract(
      bits.Write(), shape_.width, stream, stream_width,
      static_cast<std::int64_t>(stream_width - taken - shape_.width));
  bits.StoreTo(out);
  return taken + shape_.width;
}

void IntegralValueType::ResolveTriState(
    const void* lhs, const void* rhs, void* out) const {
  Folded(lhs, rhs, out, [](Planes o, ConstPlanes a, ConstPlanes b) {
    Resolve(o, a, b, NetResolution::kTriState);
  });
}

void IntegralValueType::ResolveWiredAnd(
    const void* lhs, const void* rhs, void* out) const {
  Folded(lhs, rhs, out, [](Planes o, ConstPlanes a, ConstPlanes b) {
    Resolve(o, a, b, NetResolution::kWiredAnd);
  });
}

void IntegralValueType::ResolveWiredOr(
    const void* lhs, const void* rhs, void* out) const {
  Folded(lhs, rhs, out, [](Planes o, ConstPlanes a, ConstPlanes b) {
    Resolve(o, a, b, NetResolution::kWiredOr);
  });
}

void IntegralValueType::Dominating(
    const void* stronger, const void* weaker, void* out) const {
  Folded(stronger, weaker, out, [](Planes o, ConstPlanes a, ConstPlanes b) {
    Dominate(o, a, b);
  });
}

void IntegralValueType::FilledLike(
    const void*, const void* fill, void* out) const {
  LoadedWords filled(shape_);
  FillScalar(
      filled.Write(), shape_.width, static_cast<const Logic*>(fill)->Lsb());
  filled.StoreTo(out);
}

// LRM 7.8.4: integral keys order by their numerical value at the type's
// signedness.
auto IntegralValueType::OrderBefore(const void* lhs, const void* rhs) const
    -> bool {
  return Less(
             Load(lhs).Read(), Load(rhs).Read(), shape_.width,
             shape_.signedness) == FourStateBit::kOne;
}

auto IntegralValueType::IsTrue(const void* value) const -> bool {
  return Truth(Load(value).Read()) == Truthiness::kKnownNonzero;
}

void IntegralValueType::Reduce(
    Reduction reduction, const void* lhs, const void* rhs, void* out) const {
  const std::uint64_t width = shape_.width;
  switch (reduction) {
    case Reduction::kSum:
      Folded(lhs, rhs, out, [width](Planes o, ConstPlanes a, ConstPlanes b) {
        Add(o, a, b, width);
      });
      return;
    case Reduction::kProduct:
      Folded(lhs, rhs, out, [width](Planes o, ConstPlanes a, ConstPlanes b) {
        Multiply(o, a, b, width);
      });
      return;
    case Reduction::kAnd:
      Folded(lhs, rhs, out, [](Planes o, ConstPlanes a, ConstPlanes b) {
        BitwiseAnd(o, a, b);
      });
      return;
    case Reduction::kOr:
      Folded(lhs, rhs, out, [](Planes o, ConstPlanes a, ConstPlanes b) {
        BitwiseOr(o, a, b);
      });
      return;
    case Reduction::kXor:
      Folded(lhs, rhs, out, [](Planes o, ConstPlanes a, ConstPlanes b) {
        BitwiseXor(o, a, b);
      });
      return;
  }
  std::unreachable();
}

// A packed value's bits are no storage of their own (LRM 7.4.1), so it holds
// no parts a walk reaches where they lie.
auto IntegralValueType::Parts() const -> const PartsByPosition* {
  return nullptr;
}

auto IntegralValueType::AsIntegral() const -> const IntegralValueType* {
  return this;
}

}  // namespace lyra::value
