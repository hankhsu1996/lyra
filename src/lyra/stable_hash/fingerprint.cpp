#include "lyra/stable_hash/fingerprint.hpp"

#include <cstdint>
#include <format>
#include <string>

namespace lyra::stable_hash {

auto HexOf(const Fingerprint& fingerprint) -> std::string {
  std::string text;
  text.reserve(2 * fingerprint.bytes.size());
  for (const std::uint8_t byte : fingerprint.bytes) {
    text += std::format("{:02x}", byte);
  }
  return text;
}

}  // namespace lyra::stable_hash
