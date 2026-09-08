#pragma once

#include <cstddef>
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
  kFatal,
};

// LRM 20.10 severity emit surface. The four EmitX methods are the
// generated-code entry points: each takes the pre-formatted origin string
// ("file:line:col", produced at lowering from the call's source span) and a
// pre-formatted body string (from `runtime.Format(items)`), and writes at its
// fixed severity. The origin serves both as the message prefix and as the
// rate-limit dedup key, so distinct call sites get distinct counters.
//
// LRM 20.10 also requires every report to say where in the design and when it
// was made. Those are facts of the moment rather than of the call, so they are
// asked of the run as each report is written rather than carried in an operand
// -- which is also what lets a report the tool writes for the design carry
// them, having no call site of its own to name.
class DiagnosticDispatcher {
 public:
  using DiagnosticSink = std::function<void(std::string_view)>;
  // The scope a report is being made in and the simulation time it is made at,
  // rendered as one phrase; empty where no execution is under way.
  using ContextSource = std::function<std::string()>;

  // Per-(origin, severity) suppression after this many emits in one run.
  // Zero disables rate limiting.
  static constexpr std::uint32_t kDefaultRateLimit = 10;

  explicit DiagnosticDispatcher(
      DiagnosticSink sink, std::uint32_t rate_limit = kDefaultRateLimit);

  // The run has to exist before it can be asked where it is, so the source is
  // installed rather than taken at construction.
  void SetContextSource(ContextSource context);

  void EmitInfo(
      const lyra::value::String& origin, const lyra::value::String& text);
  void EmitWarning(
      const lyra::value::String& origin, const lyra::value::String& text);
  void EmitError(
      const lyra::value::String& origin, const lyra::value::String& text);
  void EmitFatal(
      const lyra::value::String& origin, const lyra::value::String& text);

  // A report the tool writes for the design, which reaches no severity task and
  // so names no source location of its own.
  void Report(Severity severity, std::string_view body);

  // A line about the run rather than about the design: what a simulation
  // control task did (LRM 20.2, Table 20-1), and why the tool could not carry
  // on. It carries no severity and is written every time, having no call site
  // to attribute or to count.
  void Note(std::string_view text);

  // LRM 20.10: a fatal report terminates the simulation with an error code, so
  // whether one was made is what the run's exit status answers to.
  [[nodiscard]] auto ReportedFatal() const -> bool;

 private:
  void Emit(Severity severity, std::string_view origin, std::string_view body);

  struct CountKey {
    std::string origin;
    Severity severity;

    auto operator==(const CountKey&) const -> bool = default;
  };

  struct CountKeyHash {
    auto operator()(const CountKey& k) const noexcept -> std::size_t;
  };

  DiagnosticSink sink_;
  ContextSource context_;
  std::uint32_t rate_limit_;
  bool reported_fatal_ = false;
  std::unordered_map<CountKey, std::uint32_t, CountKeyHash> emit_counts_;
};

}  // namespace lyra::runtime
