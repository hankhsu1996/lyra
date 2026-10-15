#pragma once

#include <cstdint>
#include <string_view>

#include <llvm/Support/BLAKE3.h>

#include "lyra/stable_hash/fingerprint.hpp"

namespace lyra::stable_hash {

// Takes what something was built from and answers its fingerprint. What is
// hashed is the same on every host and every run: a number is written at one
// width and in one byte order, and a text is preceded by its length, so two
// pieces written one after the other cannot be read as two others.
class StableHasher {
 public:
  void AddNumber(std::uint64_t value);
  void AddText(std::string_view text);
  void AddFingerprint(const Fingerprint& fingerprint);

  [[nodiscard]] auto Finish() const -> Fingerprint;

 private:
  llvm::BLAKE3 hasher_;
};

}  // namespace lyra::stable_hash
