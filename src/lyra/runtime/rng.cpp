#include "lyra/runtime/rng.hpp"

#include <array>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <system_error>

namespace lyra::runtime {

auto DrawRng::StateText() const -> std::string {
  return std::format(
      "{} {} {} {}", addend_, congruential_, xor_low_, xor_high_);
}

auto DrawRng::FromStateText(std::string_view text) -> std::optional<DrawRng> {
  std::array<std::uint64_t, 4> words{};
  std::size_t count = 0;
  for (const auto piece : std::views::split(text, ' ')) {
    if (count == words.size()) return std::nullopt;
    const std::string_view digits(piece.begin(), piece.end());
    const char* const last = std::to_address(digits.end());
    const auto [past, error] =
        std::from_chars(std::to_address(digits.begin()), last, words.at(count));
    if (error != std::errc{} || past != last) return std::nullopt;
    ++count;
  }
  if (count != words.size()) return std::nullopt;
  const auto [addend, congruential, xor_low, xor_high] = words;
  // Every generator made here has an odd addend, which is what gives the
  // congruential half its full period, and a xoroshiro half that is not all
  // zero, which it would never leave.
  if ((addend & 1U) == 0 || (xor_low | xor_high) == 0) return std::nullopt;
  return DrawRng{addend, congruential, xor_low, xor_high};
}

auto DrawBelow(DrawRng& rng, std::uint64_t span) -> std::uint64_t {
  // Rejection rather than a modulo of the raw draw: the low values would
  // otherwise come up more often whenever the span does not divide the
  // generator's range, which is every span that is not a power of two. A span
  // covering the whole range divides it exactly, so nothing is ever rejected.
  const std::uint64_t limit = (std::uint64_t{1} << 32U) / span * span;
  std::uint64_t draw = rng.NextValue();
  while (draw >= limit) {
    draw = rng.NextValue();
  }
  return draw % span;
}

}  // namespace lyra::runtime
