#pragma once

#include <cstddef>
#include <fstream>
#include <optional>
#include <string_view>

namespace lyra::runtime {

// Abstract byte-source for the $sscanf / $fscanf scanner. The scanner only
// ever looks one byte ahead, so a single-byte pushback is enough; each
// concrete source manages its own pushback internally.
//
// `Peek` returns the next byte without advancing the cursor, or -1 on EOF.
// `Consume` returns the next byte and advances. `Unget` pushes one byte back
// so the next `Peek` / `Consume` returns it again; pushing while a byte is
// already pending throws InternalError to surface scanner-side misuse.
class ScanSource {
 public:
  ScanSource() = default;
  virtual ~ScanSource() = default;
  ScanSource(const ScanSource&) = delete;
  auto operator=(const ScanSource&) -> ScanSource& = delete;
  ScanSource(ScanSource&&) = delete;
  auto operator=(ScanSource&&) -> ScanSource& = delete;

  virtual auto Peek() -> int = 0;
  virtual auto Consume() -> int = 0;
  virtual void Unget(int byte) = 0;
};

// The pushback slot holds a byte the scanner Peek'd but decided not to
// Consume (e.g. a literal-mismatch byte per LRM 21.3.4.3 "the offending
// input character is left unread in the input stream"); the cursor itself
// never moves past such a byte.
class StringScanSource : public ScanSource {
 public:
  explicit StringScanSource(std::string_view buf);

  auto Peek() -> int override;
  auto Consume() -> int override;
  void Unget(int byte) override;

 private:
  std::string_view buf_;
  std::size_t cursor_ = 0;
  std::optional<int> pushback_ = std::nullopt;
};

// The scanner-local pushback (`peeked_`) holds a byte that has been read
// from the underlying fstream into the scanner but not yet logically
// consumed. `FlushPushback` must be called when the scanner finishes with
// the source so any unconsumed byte (typically the "offending character"
// per LRM 21.3.4.3 "the offending input character is left unread in the
// input stream") is pushed back into the fstream's putback buffer and
// becomes observable to the next read on the same FD.
class FileScanSource : public ScanSource {
 public:
  explicit FileScanSource(std::fstream& stream);

  auto Peek() -> int override;
  auto Consume() -> int override;
  void Unget(int byte) override;

  void FlushPushback();

 private:
  std::fstream* stream_;
  std::optional<int> peeked_ = std::nullopt;
};

}  // namespace lyra::runtime
