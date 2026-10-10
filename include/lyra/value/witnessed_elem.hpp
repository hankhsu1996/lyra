#pragma once

#include <cstddef>
#include <cstdint>
#include <utility>

#include "lyra/value/any_value.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/take_built.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The element is of a type the library was compiled without, answered by its
// type's table. As for a C++ element type, the default arrives as a value of
// the type, at `element_default`.
class WitnessedElem {
 public:
  WitnessedElem(const ValueType& type, const void* element_default)
      : type_(&type), default_(AnyValue::CopyOf(type, element_default)) {
  }

  [[nodiscard]] auto Type() const -> const ValueType& {
    return *type_;
  }

  [[nodiscard]] auto Size() const -> std::size_t {
    return type_->Size();
  }
  [[nodiscard]] auto Align() const -> std::size_t {
    return type_->Align();
  }

  [[nodiscard]] auto Default() const -> const void* {
    return default_.Bytes();
  }

  void Copy(const void* value, void* out) const {
    type_->Copy(value, out);
  }
  void Move(void* value, void* out) const {
    type_->Move(value, out);
  }
  void Destroy(void* value) const {
    type_->Destroy(value);
  }
  void Assign(void* storage, const void* value) const {
    type_->Assign(storage, value);
  }

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit {
    return type_->Equal(lhs, rhs);
  }
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const -> bool {
    return type_->CaseEqual(lhs, rhs);
  }
  [[nodiscard]] static auto CaseAnswer(bool holds) -> Bit {
    return Bit::FromBool(holds);
  }
  [[nodiscard]] auto Agree(const void* lhs, const void* rhs) const -> bool {
    return Holds(Equal(lhs, rhs));
  }
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool {
    return type_->BitIdentical(lhs, rhs);
  }
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool {
    return type_->HasUnknown(value);
  }
  [[nodiscard]] auto BitstreamWidth(const void* value) const -> std::int64_t {
    return TakeBuilt<Int>([&](void* out) { type_->BitstreamWidth(value, out); })
        .ToInt64();
  }
  [[nodiscard]] auto CountBits(
      const void* value, const ConstIntegralView& control_bits) const
      -> std::int64_t {
    return TakeBuilt<Int>([&](void* out) {
             type_->CountBits(
                 value, control_bits.planes, control_bits.width, out);
           })
        .ToInt64();
  }

  void Resolve(
      NetResolution fold, const void* lhs, const void* rhs, void* out) const {
    switch (fold) {
      case NetResolution::kTriState:
        type_->ResolveTriState(lhs, rhs, out);
        return;
      case NetResolution::kWiredAnd:
        type_->ResolveWiredAnd(lhs, rhs, out);
        return;
      case NetResolution::kWiredOr:
        type_->ResolveWiredOr(lhs, rhs, out);
        return;
    }
    std::unreachable();
  }
  void Dominating(const void* stronger, const void* weaker, void* out) const {
    type_->Dominating(stronger, weaker, out);
  }
  void FilledLike(const void* prototype, const Logic& fill, void* out) const {
    type_->FilledLike(prototype, &fill, out);
  }
  auto WriteToStream(
      const void* value, Planes stream, std::uint64_t stream_width,
      std::uint64_t filled) const -> std::uint64_t {
    return type_->WriteToStream(value, stream, stream_width, filled);
  }
  auto ReadFromStream(
      ConstPlanes stream, std::uint64_t stream_width, std::uint64_t taken,
      const void* prototype, void* out) const -> std::uint64_t {
    return type_->ReadFromStream(stream, stream_width, taken, prototype, out);
  }

 private:
  const ValueType* type_;
  AnyValue default_;
};

static_assert(ElementPolicy<WitnessedElem>);

}  // namespace lyra::value
