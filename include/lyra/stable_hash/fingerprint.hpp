#pragma once

#include <array>
#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace lyra::stable_hash {

// What something built is a function of, as 128 bits of a cryptographic hash
// of everything it was built from. Two things with one fingerprint are taken
// to be one thing, so a collision would hand one of them over as the other
// without a word; the width and the hash are what make that never happen in
// practice rather than rarely.
struct Fingerprint {
  std::array<std::uint8_t, 16> bytes{};

  auto operator<=>(const Fingerprint&) const -> std::strong_ordering = default;
};

// Two lowercase hex digits a byte, as a person, a file name or a report reads
// it.
[[nodiscard]] auto HexOf(const Fingerprint& fingerprint) -> std::string;

// The fingerprint `HexOf` wrote as `hex`, or nothing where `hex` is not one.
[[nodiscard]] auto FingerprintOfHex(std::string_view hex)
    -> std::optional<Fingerprint>;

// The fingerprint of one text alone, as a hasher given only that text answers
// it.
[[nodiscard]] auto FingerprintOfText(std::string_view text) -> Fingerprint;

}  // namespace lyra::stable_hash
