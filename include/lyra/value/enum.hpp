#pragma once

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

struct EnumMember {
  std::string_view name;
  std::int64_t value;
};

// CRTP base implementing every SystemVerilog enum's intrinsic methods (LRM
// 6.19.5) once. Each emitted enum class supplies only what truly varies per
// type: its base PackedType (`kBase`) and its member table (`kMembers`).
//
// Public inheritance models LRM 6.19.3: enum values auto-cast to their base
// integral type; integral-to-enum requires an explicit cast (the explicit
// PackedArray ctor below). The enum's storage is the inherited PackedArray
// subobject -- the same representation Lyra uses for all integrals.
template <typename Derived>
class Enum : public PackedArray {
 public:
  Enum() : PackedArray(Derived::kBase) {
    AssertWellFormed();
  }

  explicit Enum(PackedArray v) : PackedArray(std::move(v)) {
    AssertWellFormed();
    if (BitWidth() != Derived::kBase.bit_width ||
        IsSigned() != Derived::kBase.is_signed ||
        IsFourState() != Derived::kBase.is_four_state) {
      throw InternalError(
          "Enum<Derived>: PackedArray value's shape does not match kBase "
          "(emit is expected to convert before constructing the enum)");
    }
  }

  [[nodiscard]] auto Name() const -> String {
    if (HasUnknown()) return {};
    const auto v = ToInt64();
    for (const auto& m : Derived::kMembers) {
      if (m.value == v) return String{std::string{m.name}};
    }
    return {};
  }

  // LRM 6.19.5.3 / 6.19.5.4: the step is an SV int (default 1).
  [[nodiscard]] auto Next(const PackedArray& step = PackedArray::Int(1)) const
      -> Derived {
    return StepBy(step.ToInt64());
  }

  [[nodiscard]] auto Prev(const PackedArray& step = PackedArray::Int(1)) const
      -> Derived {
    return StepBy(-step.ToInt64());
  }

  static auto First() -> Derived {
    return Derived{MakeFromInt64(Derived::kMembers[0].value)};
  }

  static auto Last() -> Derived {
    constexpr std::size_t kLastIdx = std::size(Derived::kMembers) - 1;
    return Derived{MakeFromInt64(Derived::kMembers[kLastIdx].value)};
  }

  // LRM 6.19.5.1: num() yields an SV int.
  [[nodiscard]] static auto Num() -> PackedArray {
    return PackedArray::Int(
        static_cast<std::int32_t>(std::size(Derived::kMembers)));
  }

 private:
  // Single source of truth for compile-time invariants on Derived. Called
  // from every ctor so the assertion fires no matter which path is used.
  static constexpr void AssertWellFormed() {
    static_assert(
        std::size(Derived::kMembers) > 0,
        "Enum<Derived>: kMembers must be non-empty (SV does not allow "
        "empty enumerations)");
  }

  static auto MakeFromInt64(std::int64_t v) -> PackedArray {
    return PackedArray::FromInt(v, Derived::kBase);
  }

  // LRM 6.19.5.3/4: if the current value is not a member of the enumeration
  // (including any X/Z bits in a 4-state base), next/prev return the enum's
  // default initial value. The default ctor of PackedArray already produces
  // that: all-X for 4-state, 0 for 2-state (Table 6-7).
  [[nodiscard]] auto StepBy(std::int64_t delta) const -> Derived {
    if (HasUnknown()) return Derived{};

    const std::int64_t cur = ToInt64();
    const auto n = static_cast<std::int64_t>(std::size(Derived::kMembers));
    std::int64_t idx = -1;
    for (std::int64_t i = 0; i < n; ++i) {
      if (Derived::kMembers[i].value == cur) {
        idx = i;
        break;
      }
    }
    if (idx < 0) return Derived{};

    const std::int64_t stepped = ((idx + delta) % n + n) % n;
    return Derived{MakeFromInt64(Derived::kMembers[stepped].value)};
  }
};

}  // namespace lyra::value
