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

[[noreturn]] void RaiseStateDomainMismatch(std::string_view where);

// A width of zero is the state of a value nothing has installed yet, which no
// operation is ever handed.
[[noreturn]] void RaiseZeroWidth(std::string_view where);

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

// The state domain fixes how many planes a value has, so two operands a
// word-parallel operation reads together agree on it for the same reason they
// agree on width.
inline auto RequireSameStateDomain(
    std::string_view where, bool a_is_four_state, bool b_is_four_state)
    -> void {
  if (a_is_four_state != b_is_four_state) [[unlikely]] {
    RaiseStateDomainMismatch(where);
  }
}

}  // namespace lyra::value::detail
