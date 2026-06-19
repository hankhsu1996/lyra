#pragma once

#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>

#include "lyra/value/format.hpp"

namespace lyra::runtime {

class RuntimeServices;

enum class Severity : std::uint8_t {
  kInfo,
  kWarning,
  kError,
};

struct SourceLocation {
  std::string file;
  std::uint32_t line;
  std::uint32_t col;

  auto operator==(const SourceLocation&) const -> bool = default;
};

struct DiagnosticRecord {
  Severity severity;
  std::optional<SourceLocation> origin;
  std::string body;
};

class DiagnosticDispatcher {
 public:
  using DiagnosticSink = std::function<void(std::string_view)>;

  // Per-(file:line, severity) suppression after this many emits in one run.
  // Zero disables rate limiting.
  static constexpr std::uint32_t kDefaultRateLimit = 10;

  explicit DiagnosticDispatcher(
      DiagnosticSink sink, std::uint32_t rate_limit = kDefaultRateLimit);

  void Emit(DiagnosticRecord record);

 private:
  struct CountKey {
    std::string origin;
    Severity severity;

    auto operator==(const CountKey&) const -> bool = default;
  };

  struct CountKeyHash {
    auto operator()(const CountKey& k) const noexcept -> std::size_t;
  };

  DiagnosticSink sink_;
  std::uint32_t rate_limit_;
  std::unordered_map<CountKey, std::uint32_t, CountKeyHash> emit_counts_;
};

// Formats `items` via value::Format, builds a DiagnosticRecord, and emits
// it through services.Diagnostic(). The body is fully formatted before being
// handed to the dispatcher so the dispatcher can prepend its own envelope
// (origin, severity prefix) without re-parsing user content.
void LyraDiagnostic(
    RuntimeServices& services, Severity severity,
    std::optional<SourceLocation> origin,
    std::span<const value::PrintItem> items);

// The severity-fixed diagnostic entries for $info / $warning / $error. The
// severity is encoded by the entry, so the call carries none -- the lowering
// selects the entry. Source location is not yet resolved at lowering, so these
// emit with no origin (LRM 20.10).
void LyraInfo(
    RuntimeServices& services, std::span<const value::PrintItem> items);
void LyraWarning(
    RuntimeServices& services, std::span<const value::PrintItem> items);
void LyraError(
    RuntimeServices& services, std::span<const value::PrintItem> items);

}  // namespace lyra::runtime
