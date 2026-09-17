#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::value::detail {

// What a failed check reports. Defined out of line so `std::format` does not
// leak into every translation unit that includes this header, and
// `[[noreturn]]` so a caller's failing path has no continuation to keep live.
[[noreturn]] void RaiseWidthMismatch(
    std::string_view where, std::uint64_t a, std::uint64_t b);

[[noreturn]] void RaiseWidthMismatch(
    std::string_view where, std::uint64_t a, std::uint64_t b, std::uint64_t c);

// Operand widths agreeing is what the layer that typed the expression owes the
// operation, so the test is defined here where the optimizer can see it; only
// the report it branches to is worth the cost of being a call.
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

}  // namespace lyra::value::detail
