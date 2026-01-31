#include <charconv>
#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>

#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

namespace {

// Parse format string: "PREFIX%<spec>" -> (prefix, spec_char)
// Handles %0d -> %d by skipping leading 0.
auto ParsePlusargsFormat(std::string_view format)
    -> std::pair<std::string_view, char> {
  auto percent_pos = format.find('%');
  if (percent_pos == std::string_view::npos) {
    return {format, '\0'};
  }

  std::string_view prefix = format.substr(0, percent_pos);
  char spec = '\0';
  size_t spec_pos = percent_pos + 1;
  if (spec_pos < format.size()) {
    spec = format[spec_pos];
    if (spec == '0' && spec_pos + 1 < format.size()) {
      spec = format[spec_pos + 1];
    }
  }
  return {prefix, spec};
}

}  // namespace

auto Engine::TestPlusargs(std::string_view query) const -> int32_t {
  for (const auto& arg : plusargs_) {
    std::string_view content = arg;
    if (!content.empty() && content.front() == '+') {
      content = content.substr(1);
    }
    if (content.starts_with(query)) {
      return 1;
    }
  }
  return 0;
}

auto Engine::ValuePlusargsInt(std::string_view format, int32_t* output) const
    -> int32_t {
  auto [prefix, spec] = ParsePlusargsFormat(format);

  if (spec != 'd' && spec != 'D') {
    return 0;
  }

  for (const auto& arg : plusargs_) {
    std::string_view content = arg;
    if (!content.empty() && content.front() == '+') {
      content = content.substr(1);
    }
    if (!content.starts_with(prefix)) {
      continue;
    }
    std::string_view remainder = content.substr(prefix.size());

    int32_t parsed_value = 0;
    auto [ptr, ec] = std::from_chars(
        remainder.data(), remainder.data() + remainder.size(), parsed_value);
    if (ec != std::errc{}) {
      parsed_value = 0;
    }
    if (output != nullptr) {
      *output = parsed_value;
    }
    return 1;
  }
  return 0;
}

auto Engine::ValuePlusargsString(
    std::string_view format, std::string* output) const -> int32_t {
  auto [prefix, spec] = ParsePlusargsFormat(format);

  if (spec != 's' && spec != 'S') {
    return 0;
  }

  for (const auto& arg : plusargs_) {
    std::string_view content = arg;
    if (!content.empty() && content.front() == '+') {
      content = content.substr(1);
    }
    if (!content.starts_with(prefix)) {
      continue;
    }
    std::string_view remainder = content.substr(prefix.size());

    if (output != nullptr) {
      *output = std::string(remainder);
    }
    return 1;
  }
  return 0;
}

// LCG with glibc constants: a=1103515245, c=12345, m=2^31
// Returns unsigned 32-bit value (advances state).
auto Engine::Urandom() -> uint32_t {
  constexpr uint32_t kMultiplier = 1103515245;
  constexpr uint32_t kIncrement = 12345;
  prng_state_ = prng_state_ * kMultiplier + kIncrement;
  return prng_state_;
}

// $random returns signed 32-bit value.
auto Engine::Random() -> int32_t {
  return static_cast<int32_t>(Urandom());
}

}  // namespace lyra::runtime
