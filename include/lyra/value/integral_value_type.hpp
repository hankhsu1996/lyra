#pragma once

#include <cstddef>
#include <cstdint>

#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The domain a value of an integral type `width` bits wide is held in. Up to a
// word that is the layout the value's bytes have -- the storage unit one plane
// takes and whether an unknown plane follows it -- because what holds such a
// value reads nothing else of its type. A wider one is its planes' words, as
// many as its width asks for, so all its domain says is which planes it has.
[[nodiscard]] constexpr auto IntegralDomainFor(
    std::uint64_t width, StateDomain domain) -> support::ValueDomain {
  using support::ValueDomain;
  const bool four_state = domain == StateDomain::kFourState;
  if (width > 64) {
    return four_state ? ValueDomain::kLogicWide : ValueDomain::kBitWide;
  }
  const std::size_t plane = PlaneBytesFor(width);
  if (plane == 1) {
    return four_state ? ValueDomain::kLogic8 : ValueDomain::kBit8;
  }
  if (plane == 2) {
    return four_state ? ValueDomain::kLogic16 : ValueDomain::kBit16;
  }
  if (plane == 4) {
    return four_state ? ValueDomain::kLogic32 : ValueDomain::kBit32;
  }
  return four_state ? ValueDomain::kLogic64 : ValueDomain::kBit64;
}

// The type of an integral value (LRM 6.11) as code compiled before any design
// existed is handed it: the value's shape, and every operation on a whole
// value carried out at that shape over the bytes the value is laid out in.
//
// Generated code states one of these for each integral type it hands a value
// of to such code, as constant data of this class, so its layout is part of
// what the two sides agree on. The library states one for each type its own
// code names.
class IntegralValueType final : public ValueType {
 public:
  explicit constexpr IntegralValueType(IntegralShape shape)
      : ValueType(
            static_cast<std::uint32_t>(
                IntegralBytesFor(shape.width, shape.domain)),
            static_cast<std::uint32_t>(IntegralAlignFor(shape.width))),
        shape_(shape) {
  }

  // The library's type of the integral type `T`.
  template <IntegralValue T>
  [[nodiscard]] static auto Of() -> const IntegralValueType& {
    static constinit const IntegralValueType kType{kShapeOf<T>};
    return kType;
  }

  [[nodiscard]] auto Shape() const -> IntegralShape {
    return shape_;
  }

  // How far into the type its shape lies, which code stating a type as
  // constant data places the shape by.
  [[nodiscard]] auto ShapeOffset() const -> std::size_t;

  // The planes of the value laid out at `value`.
  [[nodiscard]] auto Load(const void* value) const -> LoadedWords {
    return LoadedWords::Load(value, shape_);
  }

  void Copy(const void* value, void* out) const override;
  void Move(void* value, void* out) const noexcept override;
  void Destroy(void* value) const noexcept override;
  void Assign(void* storage, const void* value) const override;

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit override;
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool override;

  void BitstreamWidth(const void* value, void* out) const override;
  void CountBits(
      const void* value, const ConstPlanes& control_bits,
      std::uint64_t control_width, void* out) const override;
  auto WriteToStream(
      const void* value, const Planes& stream, std::uint64_t stream_width,
      std::uint64_t filled) const -> std::uint64_t override;
  auto ReadFromStream(
      const ConstPlanes& stream, std::uint64_t stream_width,
      std::uint64_t taken, const void* prototype, void* out) const
      -> std::uint64_t override;

  void ResolveTriState(
      const void* lhs, const void* rhs, void* out) const override;
  void ResolveWiredAnd(
      const void* lhs, const void* rhs, void* out) const override;
  void ResolveWiredOr(
      const void* lhs, const void* rhs, void* out) const override;
  void Dominating(
      const void* stronger, const void* weaker, void* out) const override;
  void FilledLike(
      const void* prototype, const void* fill, void* out) const override;

  [[nodiscard]] auto OrderBefore(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto IsTrue(const void* value) const -> bool override;
  void Reduce(Reduction reduction, const void* lhs, const void* rhs, void* out)
      const override;

  [[nodiscard]] auto Parts() const -> const PartsByPosition* override;
  [[nodiscard]] auto AsIntegral() const -> const IntegralValueType* override;

 private:
  // A fold of two values of this type into a third, which `fold` carries out
  // over their planes.
  template <typename Fold>
  void Folded(const void* lhs, const void* rhs, void* out, Fold fold) const;

  IntegralShape shape_;
};

}  // namespace lyra::value
