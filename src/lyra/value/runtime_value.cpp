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

auto RuntimeValueResolveTriState(const RuntimeValue& a, const RuntimeValue& b)
    -> RuntimeValue {
  SameDomain(a, b);
  return std::visit(
      [&](const auto& lhs) -> RuntimeValue {
        using T = std::decay_t<decltype(lhs)>;
        if constexpr (NetResolvable<T>) {
          return RuntimeValue{
              .value = lhs.ResolveTriState(std::get<T>(b.value))};
        } else {
          throw InternalError(
              "RuntimeValue::ResolveTriState: this domain is not valid for a "
              "net (LRM 6.7.1), so nothing should have attached a driver to "
              "it");
        }
      },
      a.value);
}

auto RuntimeValueHighImpedanceLike(const RuntimeValue& prototype)
    -> RuntimeValue {
  return std::visit(
      [](const auto& shape) -> RuntimeValue {
        using T = std::decay_t<decltype(shape)>;
        if constexpr (NetResolvable<T>) {
          return RuntimeValue{.value = T::HighImpedanceLike(shape)};
        } else {
          throw InternalError(
              "RuntimeValue::HighImpedanceLike: this domain is not valid for a "
              "net (LRM 6.7.1), so it has no non-driving contribution");
        }
      },
      prototype.value);
}

auto RuntimeValueOrderBefore(const RuntimeValue& a, const RuntimeValue& b)
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
              "this value's domain has no order on this backend; please open "
              "an issue asking for support");
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
        } else if constexpr (
            std::is_same_v<T, RuntimeUnion> ||
            std::is_same_v<T, RuntimeTaggedUnion>) {
          throw SimulationError(
              "$bits of a union is not yet supported on this backend; please "
              "open an issue asking for support");
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
        } else if constexpr (
            std::is_same_v<T, RuntimeUnion> ||
            std::is_same_v<T, RuntimeTaggedUnion>) {
          throw SimulationError(
              "$countbits of a union is not yet supported on this backend; "
              "please open an issue asking for support");
        } else {
          return v.CountBits(control_bits);
        }
      },
      value.value);
}

namespace {

template <typename T>
constexpr bool kIsElementContainer =
    std::is_same_v<T, RuntimeQueue> || std::is_same_v<T, RuntimeDynamicArray> ||
    std::is_same_v<T, RuntimeUnpackedArray>;

}  // namespace

auto RuntimeValueContainerSize(const RuntimeValue& value) -> std::size_t {
  return std::visit(
      [](const auto& v) -> std::size_t {
        using T = std::decay_t<decltype(v)>;
        if constexpr (kIsElementContainer<T>) {
          return static_cast<std::size_t>(v.Size().ToInt64());
        } else {
          throw InternalError(
              "RuntimeValue: a spread concatenation part is not an element "
              "container (LRM 10.10)");
        }
      },
      value.value);
}

auto RuntimeValueContainerElementAt(
    const RuntimeValue& value, std::size_t position) -> const RuntimeValue& {
  return std::visit(
      [position](const auto& v) -> const RuntimeValue& {
        using T = std::decay_t<decltype(v)>;
        if constexpr (kIsElementContainer<T>) {
          return v.ElementAt(position);
        } else {
          throw InternalError(
              "RuntimeValue: a spread concatenation part is not an element "
              "container (LRM 10.10)");
        }
      },
      value.value);
}

}  // namespace lyra::value
