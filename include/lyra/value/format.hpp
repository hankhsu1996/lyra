#pragma once

#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/value/packed.hpp"

namespace lyra::value {

class PackedArray;
template <typename T>
class UnpackedArray;

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
  kChar,
  kRealDecimal,
  kRealExponential,
  kRealGeneral,
  kAssignmentPattern,
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

struct Real64ValueView {
  double value = 0.0;
};

struct Real32ValueView {
  float value = 0.0F;
};

struct RuntimeValueView;

// LRM 21.2.1.6 aggregate operand view. Owns the recursive vector of element
// views; each element view borrows into the operand's per-element storage
// (matching the singular-view lifetime contract). For nested unpacked arrays
// the element views are themselves UnpackedArrayValueView, so a multi-dim
// operand is one tree rooted at the outermost view.
struct UnpackedArrayValueView {
  std::vector<RuntimeValueView> elements;
};

struct RuntimeValueView {
  std::variant<
      IntegralValueView, StringValueView, Real64ValueView, Real32ValueView,
      UnpackedArrayValueView>
      data;

  [[nodiscard]] static auto NarrowIntegral(
      std::uint64_t word, std::uint32_t bit_width, bool is_signed)
      -> RuntimeValueView;

  [[nodiscard]] static auto String(std::string_view sv) -> RuntimeValueView;

  [[nodiscard]] static auto Real64(double v) -> RuntimeValueView;
  [[nodiscard]] static auto Real32(float v) -> RuntimeValueView;

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

  // The packed-array overload supersedes the view overloads at every cpp-emit
  // call site (signedness is read from the PackedArray itself). The view
  // overloads remain for callers that already hold a view.
  [[nodiscard]] static auto FromPackedArray(const PackedArray& pa)
      -> RuntimeValueView;

  // LRM 21.2.1.6: aggregate operand. The returned view owns a recursive
  // vector of per-element views that borrow into `arr`'s storage. Defined
  // out-of-line as a template because `T` may be `PackedArray` (terminal) or
  // `UnpackedArray<U>` (recursive).
  template <typename T>
  [[nodiscard]] static auto FromUnpackedArray(const UnpackedArray<T>& arr)
      -> RuntimeValueView;
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

// Concise constructors for a formatted print value, so emitted code reads as
// `PrintValue(x, spec)` instead of spelling out the value-view wrapper. The
// overload set picks the right RuntimeValueView from the operand's type.
[[nodiscard]] inline auto PrintValue(const PackedArray& value, FormatSpec spec)
    -> PrintItem {
  return PrintValueItem{
      .spec = spec, .value = RuntimeValueView::FromPackedArray(value)};
}
[[nodiscard]] inline auto PrintValue(std::string_view value, FormatSpec spec)
    -> PrintItem {
  return PrintValueItem{.spec = spec, .value = RuntimeValueView::String(value)};
}
[[nodiscard]] inline auto PrintValue(double value, FormatSpec spec)
    -> PrintItem {
  return PrintValueItem{.spec = spec, .value = RuntimeValueView::Real64(value)};
}
[[nodiscard]] inline auto PrintValue(float value, FormatSpec spec)
    -> PrintItem {
  return PrintValueItem{.spec = spec, .value = RuntimeValueView::Real32(value)};
}
template <typename T>
[[nodiscard]] auto PrintValue(const UnpackedArray<T>& value, FormatSpec spec)
    -> PrintItem {
  return PrintValueItem{
      .spec = spec, .value = RuntimeValueView::FromUnpackedArray(value)};
}

}  // namespace lyra::value
