#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>

#include "lyra/value/packed.hpp"

namespace lyra::value::detail {

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

// What a failed check reports. Defined out of line so `std::format` does not
// leak into every translation unit that includes this header, and
// `[[noreturn]]` so a caller's failing path has no continuation to keep live.
[[noreturn]] void RaiseWidthMismatch(
    std::string_view where, std::uint64_t a, std::uint64_t b);

[[noreturn]] void RaiseWidthMismatch(
    std::string_view where, std::uint64_t a, std::uint64_t b, std::uint64_t c);

[[noreturn]] void RaiseMisalignedView(
    std::string_view where, std::uint64_t bit_offset);

[[noreturn]] void RaiseWordCountMismatch(
    std::string_view where, std::size_t words, std::size_t expected);

// A packed operation runs several of these over its operands before it touches
// a word, so the test itself is defined here where the optimizer can see it;
// only the report it branches to is worth the cost of being a call.
inline auto RequireSameWidth(
    std::string_view where, std::uint64_t a, std::uint64_t b) -> void {
  if (a != b) [[unlikely]] {
    RaiseWidthMismatch(where, a, b);
  }
}

inline auto RequireSameWidth(
    std::string_view where, std::uint64_t a, std::uint64_t b, std::uint64_t c)
    -> void {
  if (a != b || a != c) [[unlikely]] {
    RaiseWidthMismatch(where, a, b, c);
  }
}

inline auto RequireAligned(std::string_view where, std::uint64_t bit_offset)
    -> void {
  if (bit_offset != 0U) [[unlikely]] {
    RaiseMisalignedView(where, bit_offset);
  }
}

inline auto RequireWordCount(
    std::string_view where, std::span<const std::uint64_t> words,
    std::uint64_t bit_width) -> void {
  const std::size_t expected = WordCountForBits(bit_width);
  if (words.size() != expected) [[unlikely]] {
    RaiseWordCountMismatch(where, words.size(), expected);
  }
}

inline auto RequireWordCount(
    std::string_view where, std::span<std::uint64_t> words,
    std::uint64_t bit_width) -> void {
  RequireWordCount(
      where, std::span<const std::uint64_t>{words.data(), words.size()},
      bit_width);
}

}  // namespace lyra::value::detail
