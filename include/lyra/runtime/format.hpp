#pragma once

#include <cstdint>
#include <span>
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

struct NarrowIntegralView {
  std::uint64_t value_word = 0;
  std::uint64_t unknown_word = 0;
  std::uint64_t bit_width = 0;
  IntegralStateKind state_kind = IntegralStateKind::kTwoState;
  bool is_signed = false;
};

struct WideIntegralView {
  std::span<const std::uint64_t> value_words;
  std::span<const std::uint64_t> unknown_words;
  std::uint64_t bit_width = 0;
  IntegralStateKind state_kind = IntegralStateKind::kTwoState;
  bool is_signed = false;
};

struct IntegralValueView {
  std::variant<NarrowIntegralView, WideIntegralView> data;

  [[nodiscard]] static auto Narrow(
      std::uint64_t value_word, std::uint64_t unknown_word,
      std::uint64_t bit_width, IntegralStateKind state_kind, bool is_signed)
      -> IntegralValueView;

  [[nodiscard]] static auto Wide(
      std::span<const std::uint64_t> value_words,
      std::span<const std::uint64_t> unknown_words, std::uint64_t bit_width,
      IntegralStateKind state_kind, bool is_signed) -> IntegralValueView;

  [[nodiscard]] auto BitWidth() const -> std::uint64_t;
  [[nodiscard]] auto StateKind() const -> IntegralStateKind;
  [[nodiscard]] auto IsSigned() const -> bool;
  [[nodiscard]] auto IsWide() const -> bool;
};

struct StringValueView {
  const char* data = nullptr;
  std::uint32_t size = 0;
};

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
