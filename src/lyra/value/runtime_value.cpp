#include "lyra/value/runtime_value.hpp"

#include <functional>
#include <type_traits>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

namespace {

auto SameDomain(const RuntimeValue& a, const RuntimeValue& b) -> void {
  if (a.value.index() != b.value.index()) {
    throw InternalError(
        "RuntimeValue: comparing values of different runtime domains");
  }
}

}  // namespace

auto RuntimeValueEqual(const RuntimeValue& a, const RuntimeValue& b)
    -> PackedArray {
  SameDomain(a, b);
  return std::visit(
      [&](const auto& lhs) -> PackedArray {
        using T = std::decay_t<decltype(lhs)>;
        return lhs == std::get<T>(b.value);
      },
      a.value);
}

auto RuntimeValueCaseEqual(const RuntimeValue& a, const RuntimeValue& b)
    -> PackedArray {
  SameDomain(a, b);
  return std::visit(
      [&](const auto& lhs) -> PackedArray {
        using T = std::decay_t<decltype(lhs)>;
        if constexpr (std::is_same_v<T, Real> || std::is_same_v<T, ShortReal>) {
          throw InternalError(
              "RuntimeValue::CaseEqual: === is not defined on a real value "
              "(LRM Table 11-1)");
        } else {
          return detail::ArrayCaseEqElement(lhs, std::get<T>(b.value));
        }
      },
      a.value);
}

auto RuntimeValueBitIdentical(const RuntimeValue& a, const RuntimeValue& b)
    -> bool {
  SameDomain(a, b);
  return std::visit(
      [&](const auto& lhs) -> bool {
        using T = std::decay_t<decltype(lhs)>;
        return lhs.IsBitIdentical(std::get<T>(b.value));
      },
      a.value);
}

auto RuntimeValueIndexBefore(const RuntimeValue& a, const RuntimeValue& b)
    -> bool {
  SameDomain(a, b);
  return std::visit(
      [&](const auto& lhs) -> bool {
        using T = std::decay_t<decltype(lhs)>;
        if constexpr (std::is_same_v<T, Chandle>) {
          return std::less<>{}(lhs.Ptr(), std::get<T>(b.value).Ptr());
        } else if constexpr (requires { lhs < std::get<T>(b.value); }) {
          return static_cast<bool>(lhs < std::get<T>(b.value));
        } else {
          throw SimulationError(
              "associative array: this index type has no order on this "
              "backend; please open an issue asking for support");
        }
      },
      a.value);
}

auto RuntimeValueHasUnknown(const RuntimeValue& value) -> bool {
  return std::visit(
      [](const auto& v) -> bool { return v.HasUnknown(); }, value.value);
}

auto RuntimeValueBitstreamWidth(const RuntimeValue& value) -> PackedArray {
  return std::visit(
      [](const auto& v) -> PackedArray {
        using T = std::decay_t<decltype(v)>;
        if constexpr (
            std::is_same_v<T, Real> || std::is_same_v<T, ShortReal> ||
            std::is_same_v<T, Chandle>) {
          throw InternalError(
              "RuntimeValue::BitstreamWidth: a real and a chandle are not "
              "bit-stream types (LRM 6.24.3)");
        } else {
          return v.BitstreamWidth();
        }
      },
      value.value);
}

auto RuntimeValueCountBits(
    const RuntimeValue& value, const PackedArray& control_bits) -> PackedArray {
  return std::visit(
      [&](const auto& v) -> PackedArray {
        using T = std::decay_t<decltype(v)>;
        if constexpr (
            std::is_same_v<T, Real> || std::is_same_v<T, ShortReal> ||
            std::is_same_v<T, Chandle>) {
          throw InternalError(
              "RuntimeValue::CountBits: $countbits takes a bit-stream operand, "
              "which a real or a chandle is not (LRM 20.9)");
        } else {
          return v.CountBits(control_bits);
        }
      },
      value.value);
}

}  // namespace lyra::value
