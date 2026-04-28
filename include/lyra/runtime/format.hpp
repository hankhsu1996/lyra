#pragma once

#include <cstdint>
#include <limits>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"

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

enum class RuntimeValueKind : std::uint8_t {
  kIntegral,
  kString,
};

struct FormatSpec {
  FormatKind kind = FormatKind::kDecimal;
  std::int32_t width = -1;
  std::int32_t precision = -1;
  bool zero_pad = false;
  bool left_align = false;
  std::int32_t timeunit_power = 0;
};

// Non-owning view of a value passed into the runtime print API.
//
// Two integral storage planes coexist:
//   - Narrow integral (bit_width <= 64): the word lives inline in
//   `inline_word`.
//     `value_words` is null. Construction needs no externally-owned storage,
//     so the whole view can be built inline at a `LyraPrint` call site.
//   - Wide integral (bit_width > 64): caller owns a `uint64_t[word_count]`
//     array and passes a pointer via `value_words`. Not used by this cut.
//
// String values reference external character storage (e.g. a `std::string`
// kept alive for the duration of the LyraPrint call) via `string_data` /
// `string_size`. The `String(string_view)` factory captures the view directly.
struct RuntimeValueView {
  RuntimeValueKind kind = RuntimeValueKind::kIntegral;
  std::uint64_t inline_word = 0;
  const std::uint64_t* value_words = nullptr;
  const std::uint64_t* unknown_words = nullptr;
  std::uint32_t word_count = 0;
  std::uint32_t bit_width = 0;
  bool is_signed = false;

  const char* string_data = nullptr;
  std::uint32_t string_size = 0;

  static auto NarrowIntegral(
      std::uint64_t word, std::uint32_t bit_width, bool is_signed)
      -> RuntimeValueView {
    return RuntimeValueView{
        .kind = RuntimeValueKind::kIntegral,
        .inline_word = word,
        .word_count = 1,
        .bit_width = bit_width,
        .is_signed = is_signed};
  }

  static auto String(std::string_view sv) -> RuntimeValueView {
    if (sv.size() > std::numeric_limits<std::uint32_t>::max()) {
      throw InternalError("RuntimeValueView::String: string too large");
    }
    return RuntimeValueView{
        .kind = RuntimeValueKind::kString,
        .string_data = sv.data(),
        .string_size = static_cast<std::uint32_t>(sv.size())};
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
