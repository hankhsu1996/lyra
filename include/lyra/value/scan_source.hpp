#pragma once

#include <cstddef>
#include <optional>
#include <string_view>

namespace lyra::value::detail {

// Byte-source contract for the scan parser. The parser looks one byte
// ahead at most, so a single-byte pushback (`Unget`) is enough. Each
// concrete source manages its own pushback. `IsWhitespace` lets a source
// extend the default ASCII set -- `StringScanSource` adds NUL per LRM
// 21.3.4.3(a).
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
  virtual auto IsWhitespace(int ch) -> bool;
};

class StringScanSource : public ScanSource {
 public:
  explicit StringScanSource(std::string_view buf);

  auto Peek() -> int override;
  auto Consume() -> int override;
  void Unget(int byte) override;
  auto IsWhitespace(int ch) -> bool override;

  // Offset of the next unread byte in `buf`, ignoring any pending pushback.
  [[nodiscard]] auto Position() const -> std::size_t {
    return cursor_;
  }

 private:
  std::string_view buf_;
  std::size_t cursor_ = 0;
  std::optional<int> pushback_ = std::nullopt;
};

}  // namespace lyra::value::detail
