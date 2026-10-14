#include "lyra/stable_hash/stable_hasher.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <string_view>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/BLAKE3.h>

#include "lyra/stable_hash/fingerprint.hpp"

namespace lyra::stable_hash {

void StableHasher::AddNumber(std::uint64_t value) {
  std::array<std::uint8_t, sizeof(value)> bytes{};
  for (std::size_t i = 0; i < bytes.size(); ++i) {
    bytes.at(i) = static_cast<std::uint8_t>(value >> (8 * i));
  }
  hasher_.update(llvm::ArrayRef<std::uint8_t>(bytes));
}

void StableHasher::AddText(std::string_view text) {
  AddNumber(text.size());
  hasher_.update(llvm::StringRef(text.data(), text.size()));
}

void StableHasher::AddFingerprint(const Fingerprint& fingerprint) {
  hasher_.update(llvm::ArrayRef<std::uint8_t>(fingerprint.bytes));
}

// Finishing reads the state without ending it, so a hasher can be finished
// and still be added to.
auto StableHasher::Finish() const -> Fingerprint {
  llvm::BLAKE3 finished = hasher_;
  return Fingerprint{.bytes = finished.final<sizeof(Fingerprint::bytes)>()};
}

}  // namespace lyra::stable_hash
