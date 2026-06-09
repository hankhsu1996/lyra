#pragma once

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

// Out-of-line so std::format does not leak into every translation unit
// that includes this header.
auto RequireSameWidth(std::string_view where, std::uint64_t a, std::uint64_t b)
    -> void;

auto RequireSameWidth(
    std::string_view where, std::uint64_t a, std::uint64_t b, std::uint64_t c)
    -> void;

auto RequireAligned(std::string_view where, std::uint64_t bit_offset) -> void;

auto RequireWordCount(
    std::string_view where, std::span<const std::uint64_t> words,
    std::uint64_t bit_width) -> void;

inline auto RequireWordCount(
    std::string_view where, std::span<std::uint64_t> words,
    std::uint64_t bit_width) -> void {
  RequireWordCount(
      where, std::span<const std::uint64_t>{words.data(), words.size()},
      bit_width);
}

}  // namespace lyra::value::detail
