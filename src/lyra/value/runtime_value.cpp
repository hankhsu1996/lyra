#include "lyra/value/runtime_value.hpp"

#include <type_traits>
#include <variant>

#include "lyra/base/internal_error.hpp"
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
          return lhs.CaseEqual(std::get<T>(b.value));
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

auto RuntimeValueHasUnknown(const RuntimeValue& value) -> bool {
  return std::visit(
      [](const auto& v) -> bool { return v.HasUnknown(); }, value.value);
}

}  // namespace lyra::value
