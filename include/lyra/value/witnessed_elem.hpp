#pragma once

#include <cstddef>
#include <cstdint>
#include <utility>

#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/take_built.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The element of a container the library holds, of a type the library was
// compiled without: each operation a container's algorithms ask of one is
// answered by the type's table, and an index naming no element (LRM 7.4.5) by
// the element type the container was handed.
class WitnessedElem {
 public:
  explicit WitnessedElem(const value::ElementType& element)
      : element_(&element) {
  }

  [[nodiscard]] auto ElementType() const -> const value::ElementType& {
    return *element_;
  }

  [[nodiscard]] auto Default() const -> const void* {
    return element_->Default();
  }
  [[nodiscard]] auto DiscardedWrite() const -> void* {
    return element_->DiscardedWrite();
  }

  [[nodiscard]] auto Size() const -> std::size_t {
    return Type().Size();
  }
  [[nodiscard]] auto Align() const -> std::size_t {
    return Type().Align();
  }

  void Copy(const void* value, void* out) const {
    Type().Copy(value, out);
  }
  void Move(void* value, void* out) const {
    Type().Move(value, out);
  }
  void Destroy(void* value) const {
    Type().Destroy(value);
  }
  void Assign(void* storage, const void* value) const {
    Type().Assign(storage, value);
  }

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit {
    return Type().Equal(lhs, rhs);
  }
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const -> bool {
    return Type().CaseEqual(lhs, rhs);
  }
  [[nodiscard]] static auto CaseAnswer(bool holds) -> Bit {
    return Bit::FromBool(holds);
  }
  [[nodiscard]] auto Agree(const void* lhs, const void* rhs) const -> bool {
    return Holds(Equal(lhs, rhs));
  }
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool {
    return Type().BitIdentical(lhs, rhs);
  }
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool {
    return Type().HasUnknown(value);
  }
  [[nodiscard]] auto BitstreamWidth(const void* value) const -> std::int64_t {
    return TakeBuilt<Int>([&](void* out) { Type().BitstreamWidth(value, out); })
        .ToInt64();
  }
  [[nodiscard]] auto CountBits(
      const void* value, const ConstIntegralView& control_bits) const
      -> std::int64_t {
    return TakeBuilt<Int>(
               [&](void* out) { Type().CountBits(value, control_bits, out); })
        .ToInt64();
  }

  void Resolve(
      NetResolution fold, const void* lhs, const void* rhs, void* out) const {
    switch (fold) {
      case NetResolution::kTriState:
        Type().ResolveTriState(lhs, rhs, out);
        return;
      case NetResolution::kWiredAnd:
        Type().ResolveWiredAnd(lhs, rhs, out);
        return;
      case NetResolution::kWiredOr:
        Type().ResolveWiredOr(lhs, rhs, out);
        return;
    }
    std::unreachable();
  }
  void Dominating(const void* stronger, const void* weaker, void* out) const {
    Type().Dominating(stronger, weaker, out);
  }
  void FilledLike(const void* prototype, const Logic& fill, void* out) const {
    Type().FilledLike(prototype, &fill, out);
  }
  auto WriteToStream(
      const void* value, const IntegralView& stream, std::uint64_t filled) const
      -> std::uint64_t {
    return Type().WriteToStream(value, stream, filled);
  }
  auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, std::uint64_t until,
      void* out) const -> std::uint64_t {
    return Type().ReadFromStream(stream, taken, until, out);
  }

 private:
  [[nodiscard]] auto Type() const -> const ValueType& {
    return element_->Type();
  }

  const value::ElementType* element_;
};

static_assert(ElementPolicy<WitnessedElem>);

}  // namespace lyra::value
