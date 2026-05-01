#pragma once

#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/packed.hpp"

namespace lyra::runtime::detail {

struct PackedAccess {
  static auto BitOffset(const ConstBitView& v) -> std::uint64_t {
    return v.bit_offset_;
  }
  static auto BitOffset(const BitView& v) -> std::uint64_t {
    return v.bit_offset_;
  }
  static auto BitOffset(const ConstLogicView& v) -> std::uint64_t {
    return v.bit_offset_;
  }
  static auto BitOffset(const LogicView& v) -> std::uint64_t {
    return v.bit_offset_;
  }

  static auto ValueWords(const ConstBitView& v)
      -> std::span<const std::uint64_t> {
    return v.words_;
  }
  static auto ValueWords(const BitView& v) -> std::span<std::uint64_t> {
    return v.words_;
  }
  static auto ValueWords(const ConstLogicView& v)
      -> std::span<const std::uint64_t> {
    return v.value_words_;
  }
  static auto ValueWords(const LogicView& v) -> std::span<std::uint64_t> {
    return v.value_words_;
  }

  static auto UnknownWords(const ConstLogicView& v)
      -> std::span<const std::uint64_t> {
    return v.unknown_words_;
  }
  static auto UnknownWords(const LogicView& v) -> std::span<std::uint64_t> {
    return v.unknown_words_;
  }
};

inline auto RequireSameWidth(
    std::string_view where, std::uint64_t a, std::uint64_t b) -> void {
  if (a != b) {
    throw InternalError(
        std::format("{}: width mismatch ({} vs {})", where, a, b));
  }
}

inline auto RequireSameWidth(
    std::string_view where, std::uint64_t a, std::uint64_t b, std::uint64_t c)
    -> void {
  if (a != b || a != c) {
    throw InternalError(
        std::format("{}: width mismatch ({} vs {} vs {})", where, a, b, c));
  }
}

inline auto RequireAligned(std::string_view where, std::uint64_t bit_offset)
    -> void {
  if (bit_offset != 0U) {
    throw InternalError(
        std::format(
            "{}: requires bit_offset == 0 (got {})", where, bit_offset));
  }
}

inline auto RequireWordCount(
    std::string_view where, std::span<const std::uint64_t> words,
    std::uint64_t bit_width) -> void {
  const std::size_t expected = WordCountForBits(bit_width);
  if (words.size() != expected) {
    throw InternalError(
        std::format(
            "{}: word count mismatch ({} vs expected {})", where, words.size(),
            expected));
  }
}

inline auto RequireWordCount(
    std::string_view where, std::span<std::uint64_t> words,
    std::uint64_t bit_width) -> void {
  RequireWordCount(
      where, std::span<const std::uint64_t>{words.data(), words.size()},
      bit_width);
}

}  // namespace lyra::runtime::detail
