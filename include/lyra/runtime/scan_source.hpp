#pragma once

#include <cstddef>
#include <optional>
#include <string_view>

namespace lyra::runtime {

// Input source for the $sscanf / $fscanf scanner. The scanner only ever
// peeks one byte ahead, so a single-byte pushback is enough. The interface
// is deliberately small so the file-source adapter (a follow-up PR for
// $fscanf) can satisfy the same contract by wrapping LyraFGetc / LyraFUngetc.
//
// `Peek` returns the next byte without advancing the cursor, or -1 on EOF.
// `Consume` returns the next byte and advances. `Unget` pushes one byte back
// so the next `Peek` / `Consume` returns it again; pushing while a byte is
// already pending throws InternalError to surface scanner-side misuse.
class StringScanSource {
 public:
  explicit StringScanSource(std::string_view buf);

  auto Peek() -> int;
  auto Consume() -> int;
  void Unget(int byte);

 private:
  std::string_view buf_;
  std::size_t cursor_ = 0;
  std::optional<int> pushback_ = std::nullopt;
};

}  // namespace lyra::runtime
