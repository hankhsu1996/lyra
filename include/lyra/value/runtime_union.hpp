#pragma once

#include <cstddef>
#include <utility>

#include "lyra/value/any_value.hpp"
#include "lyra/value/basic_union.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

// The live member of a union as the library holds one: its declaration-order
// index and its value, with the value's type.
class HeldMember {
 public:
  HeldMember() = default;
  HeldMember(std::size_t index, AnyValue value)
      : index_(index), value_(std::move(value)) {
  }

  [[nodiscard]] auto Index() const -> std::size_t {
    return index_;
  }
  [[nodiscard]] auto Value() const -> const AnyValue& {
    return value_;
  }

  [[nodiscard]] static auto Equal(const HeldMember& a, const HeldMember& b)
      -> FourStateBit {
    if (a.index_ != b.index_) {
      return FourStateBit::kZero;
    }
    return a.value_ == b.value_;
  }

  [[nodiscard]] static auto HighImpedance() -> Logic {
    return Logic::Filled(FourStateBit::kHighImpedance);
  }

  template <typename F>
  [[nodiscard]] auto Visit(F f) const {
    return f(value_);
  }
  template <typename F>
  [[nodiscard]] static auto Paired(
      const HeldMember& a, const HeldMember& b, F f) {
    return f(a.value_, b.value_);
  }
  template <typename F>
  [[nodiscard]] static auto Rebuilt(
      const HeldMember& a, const HeldMember& b, F f) -> HeldMember {
    return {a.index_, f(a.value_, b.value_)};
  }
  template <typename F>
  [[nodiscard]] auto Mapped(F f) const -> HeldMember {
    return {index_, f(value_)};
  }

 private:
  std::size_t index_ = 0;
  AnyValue value_;
};

// An untagged unpacked union (LRM 7.3) as the library holds one.
class RuntimeUnion : public BasicUnion<RuntimeUnion, HeldMember> {
 public:
  // Holds no member: storage before its declaration installs the first
  // member's default (LRM Table 7-1).
  RuntimeUnion() = default;
  RuntimeUnion(std::size_t index, AnyValue value);

  // Reads member `index`, which must be the live one.
  [[nodiscard]] auto Component(std::size_t index) const -> const AnyValue&;

  // Makes `index` the live member, carrying `value` (the activating write of a
  // member, and the whole-value rebuild a build primitive produces).
  void SetComponent(std::size_t index, AnyValue value);

 private:
  friend BasicUnion<RuntimeUnion, HeldMember>;

  explicit RuntimeUnion(HeldMember live);
};

static_assert(LyraValue<RuntimeUnion>);
static_assert(NetResolvable<RuntimeUnion>);
static_assert(CaseEqualComparable<RuntimeUnion>);

}  // namespace lyra::value
