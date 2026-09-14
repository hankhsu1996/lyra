#include "lyra/value/packed_internal.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::value::detail {

void RaiseWidthMismatch(
    std::string_view where, std::uint64_t a, std::uint64_t b) {
  throw InternalError(
      std::format("{}: width mismatch ({} vs {})", where, a, b));
}

void RaiseWidthMismatch(
    std::string_view where, std::uint64_t a, std::uint64_t b, std::uint64_t c) {
  throw InternalError(
      std::format("{}: width mismatch ({} vs {} vs {})", where, a, b, c));
}

void RaiseMisalignedView(std::string_view where, std::uint64_t bit_offset) {
  throw InternalError(
      std::format("{}: requires bit_offset == 0 (got {})", where, bit_offset));
}

void RaiseWordCountMismatch(
    std::string_view where, std::size_t words, std::size_t expected) {
  throw InternalError(
      std::format(
          "{}: word count mismatch ({} vs expected {})", where, words,
          expected));
}

}  // namespace lyra::value::detail
