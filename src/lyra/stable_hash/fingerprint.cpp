#include "lyra/stable_hash/fingerprint.hpp"

#include <charconv>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>

namespace lyra::stable_hash {

auto HexOf(const Fingerprint& fingerprint) -> std::string {
  std::string text;
  text.reserve(2 * fingerprint.bytes.size());
  for (const std::uint8_t byte : fingerprint.bytes) {
    text += std::format("{:02x}", byte);
  }
  return text;
}

auto FingerprintOfHex(std::string_view hex) -> std::optional<Fingerprint> {
  Fingerprint fingerprint;
  if (hex.size() != 2 * fingerprint.bytes.size()) {
    return std::nullopt;
  }
  for (std::size_t i = 0; i < fingerprint.bytes.size(); ++i) {
    const std::string_view digits = hex.substr(2 * i, 2);
    std::uint8_t byte = 0;
    const auto [end, error] =
        std::from_chars(digits.data(), digits.data() + digits.size(), byte, 16);
    if (error != std::errc{} || end != digits.data() + digits.size()) {
      return std::nullopt;
    }
    fingerprint.bytes.at(i) = byte;
  }
  return fingerprint;
}

}  // namespace lyra::stable_hash
