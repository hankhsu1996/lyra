#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>

#include "lyra/base/diag_message.hpp"
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
  // Where in the design and when a report is being made, as the run answers it.
  struct Context {
    // The instance the report is made in and the simulation time, as the
    // phrase a note says them in.
    std::string where;
    // Where the procedure the report is made under is written; empty where
    // none is running or no source text states it.
    std::string_view procedure;
  };
  using ContextSource = std::function<Context()>;

  // Per-(place, severity) suppression after this many emits in one run.
  // Zero disables rate limiting.
  static constexpr std::uint32_t kDefaultRateLimit = 10;

  explicit DiagnosticDispatcher(std::uint32_t rate_limit = kDefaultRateLimit);

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
  // so names no source location of its own. What it can name is the procedure
  // it is made under, and it does. `notes` follow it, each a line of its own,
  // after the one saying where and when.
  void Report(
      Severity severity, const base::DiagMessage& message,
      std::span<const base::DiagMessage> notes = {});

  // A whole line about the run rather than about the design, opening with its
  // own word: what a simulation control task did (LRM 20.2, Table 20-1), what
  // kept a time slot from ending, and why the tool could not carry on. It is
  // written every time, having no call site to attribute or to count.
  static void Line(std::string_view line);

  // LRM 20.10: a fatal report terminates the simulation with an error code, so
  // whether one was made is what the run's exit status answers to.
  [[nodiscard]] auto ReportedFatal() const -> bool;

 private:
  void Emit(
      Severity severity, std::string_view word, std::string_view origin,
      std::string_view body, std::span<const base::DiagMessage> notes = {});
  // Every report goes to the process's error stream, which C++ ties to its
  // standard output, so a report follows what the design printed before it.
  static void Write(std::string_view text);

  struct CountKey {
    std::string place;
    Severity severity;

    auto operator==(const CountKey&) const -> bool = default;
  };

  struct CountKeyHash {
    auto operator()(const CountKey& k) const noexcept -> std::size_t;
  };

  ContextSource context_;
  std::uint32_t rate_limit_;
  bool reported_fatal_ = false;
  std::unordered_map<CountKey, std::uint32_t, CountKeyHash> emit_counts_;
};

}  // namespace lyra::runtime
