#include "lyra/value/packed_internal.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed.hpp"

namespace lyra::value::detail {

auto RequireSameWidth(std::string_view where, std::uint64_t a, std::uint64_t b)
    -> void {
  if (a != b) {
    throw InternalError(
        std::format("{}: width mismatch ({} vs {})", where, a, b));
  }
}

auto RequireSameWidth(
    std::string_view where, std::uint64_t a, std::uint64_t b, std::uint64_t c)
    -> void {
  if (a != b || a != c) {
    throw InternalError(
        std::format("{}: width mismatch ({} vs {} vs {})", where, a, b, c));
  }
}

auto RequireAligned(std::string_view where, std::uint64_t bit_offset) -> void {
  if (bit_offset != 0U) {
    throw InternalError(
        std::format(
            "{}: requires bit_offset == 0 (got {})", where, bit_offset));
  }
}

auto RequireWordCount(
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

}  // namespace lyra::value::detail
