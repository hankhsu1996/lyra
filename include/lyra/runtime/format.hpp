#pragma once

#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>

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

struct RuntimeValueView {
  RuntimeValueKind kind = RuntimeValueKind::kIntegral;
  const std::uint64_t* value_words = nullptr;
  const std::uint64_t* unknown_words = nullptr;
  std::uint32_t word_count = 0;
  std::uint32_t bit_width = 0;
  bool is_signed = false;

  const char* string_data = nullptr;
  std::uint32_t string_size = 0;

  static auto Integral(
      const std::uint64_t* value_words, const std::uint64_t* unknown_words,
      std::uint32_t word_count, std::uint32_t bit_width, bool is_signed)
      -> RuntimeValueView {
    return RuntimeValueView{
        .kind = RuntimeValueKind::kIntegral,
        .value_words = value_words,
        .unknown_words = unknown_words,
        .word_count = word_count,
        .bit_width = bit_width,
        .is_signed = is_signed};
  }

  static auto String(const char* data, std::size_t size) -> RuntimeValueView {
    if (size > std::numeric_limits<std::uint32_t>::max()) {
      throw InternalError("RuntimeValueView::String: string too large");
    }
    return RuntimeValueView{
        .kind = RuntimeValueKind::kString,
        .string_data = data,
        .string_size = static_cast<std::uint32_t>(size)};
  }
};

[[nodiscard]] auto FormatValue(
    const FormatSpec& spec, const RuntimeValueView& value) -> std::string;

}  // namespace lyra::runtime
