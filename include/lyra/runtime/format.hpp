#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/runtime/packed.hpp"

namespace lyra::runtime {

enum class PrintKind : std::uint8_t {
  kDisplay,
  kWrite,
  kFDisplay,
  kFWrite,
};

enum class FormatKind : std::uint8_t {
  kDecimal,
  kHex,
  kBinary,
  kOctal,
  kString,
};

// Two-state vs four-state lives on the integral axis, not as a top-level
// value kind: it's a representation/semantics dimension of integral, in the
// same way `string` is a value category. The two enums are orthogonal.
enum class IntegralStateKind : std::uint8_t {
  kTwoState,
  kFourState,
};

struct FormatSpec {
  FormatKind kind = FormatKind::kDecimal;
  std::int32_t width = -1;
  std::int32_t precision = -1;
  bool zero_pad = false;
  bool left_align = false;
  std::int32_t timeunit_power = 0;
};

// Non-owning view payloads passed into the runtime print API.
//
// Narrow integral (bit_width <= 64): the value lives inline in `inline_word`,
// and (for four-state) the unknown plane lives inline in
// `inline_unknown_word`. The whole view is built inline at a `LyraPrint` call
// site with no externally owned storage. Wide integrals (>64 bits) are not
// yet supported.
struct IntegralValueView {
  IntegralStateKind state = IntegralStateKind::kTwoState;
  std::uint64_t inline_word = 0;
  std::uint64_t inline_unknown_word = 0;
  std::uint32_t bit_width = 0;
  bool is_signed = false;
};

// String values reference external character storage (e.g. a `std::string`
// kept alive for the duration of the LyraPrint call) via `data` / `size`.
// The `String(string_view)` factory captures the view directly.
struct StringValueView {
  const char* data = nullptr;
  std::uint32_t size = 0;
};

// Wrapper over the variant payload so factories live on the value type
// itself rather than scattered in a sub-namespace. Adding a new category
// requires a new alternative + a new visitor arm; std::visit on `data`
// gives compile-time exhaustiveness via Overloaded.
struct RuntimeValueView {
  std::variant<IntegralValueView, StringValueView> data;

  [[nodiscard]] static auto NarrowIntegral(
      std::uint64_t word, std::uint32_t bit_width, bool is_signed)
      -> RuntimeValueView;

  [[nodiscard]] static auto String(std::string_view sv) -> RuntimeValueView;

  [[nodiscard]] static auto FromBitView(ConstBitView view, bool is_signed)
      -> RuntimeValueView;

  [[nodiscard]] static auto FromBitView(BitView view, bool is_signed)
      -> RuntimeValueView {
    return FromBitView(view.AsConst(), is_signed);
  }

  [[nodiscard]] static auto FromLogicView(ConstLogicView view, bool is_signed)
      -> RuntimeValueView;

  [[nodiscard]] static auto FromLogicView(LogicView view, bool is_signed)
      -> RuntimeValueView {
    return FromLogicView(view.AsConst(), is_signed);
  }
};

[[nodiscard]] auto FormatValue(
    const FormatSpec& spec, const RuntimeValueView& value) -> std::string;

// Print items: each element of a `LyraPrint` argument list is one of these.
// `LyraPrint` dispatches via `std::visit`, so each arm only sees the payload
// it actually needs.
struct PrintLiteralItem {
  const char* data;
  std::uint32_t size;
};

struct PrintValueItem {
  FormatSpec spec;
  RuntimeValueView value;
};

using PrintItem = std::variant<PrintLiteralItem, PrintValueItem>;

}  // namespace lyra::runtime
