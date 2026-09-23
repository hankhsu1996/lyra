#include "lyra/value/packed_internal.hpp"

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

void RaiseStateDomainMismatch(std::string_view where) {
  throw InternalError(std::format("{}: state domain mismatch", where));
}

void RaiseZeroWidth(std::string_view where) {
  throw InternalError(std::format("{}: zero bit_width", where));
}

}  // namespace lyra::value::detail
