#pragma once

#include <cstdint>
#include <functional>
#include <string>
#include <string_view>
#include <unordered_map>

#include "lyra/value/string.hpp"

namespace lyra::runtime {

enum class Severity : std::uint8_t {
  kInfo,
  kWarning,
  kError,
};

struct DiagnosticRecord {
  Severity severity;
  std::string origin;
  std::string body;
};

// LRM 20.10 severity-fixed emit surface. The three EmitX methods are the
// generated-code entry points: each takes the pre-formatted origin string
// ("file:line:col", produced at lowering from the call's source span) and a
// pre-formatted body string (from `services.Format(items)`), and routes a
// record at its fixed severity through Emit. The dispatcher uses the origin
// directly for both the message prefix and the rate-limit dedup key, so
// distinct call sites get distinct counters.
class DiagnosticDispatcher {
 public:
  using DiagnosticSink = std::function<void(std::string_view)>;

  // Per-(origin, severity) suppression after this many emits in one run.
  // Zero disables rate limiting.
  static constexpr std::uint32_t kDefaultRateLimit = 10;

  explicit DiagnosticDispatcher(
      DiagnosticSink sink, std::uint32_t rate_limit = kDefaultRateLimit);

  void EmitInfo(
      const lyra::value::String& origin, const lyra::value::String& text);
  void EmitWarning(
      const lyra::value::String& origin, const lyra::value::String& text);
  void EmitError(
      const lyra::value::String& origin, const lyra::value::String& text);

 private:
  void Emit(DiagnosticRecord record);

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

}  // namespace lyra::runtime
