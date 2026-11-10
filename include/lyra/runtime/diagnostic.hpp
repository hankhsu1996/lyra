#pragma once

#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/base/time.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class FileTable;

enum class Severity : std::uint8_t {
  kInfo,
  kWarning,
  kError,
  kFatal,
};

// LRM 20.10 severity emit surface. The four EmitX methods, and the four
// EmitXAtCaller methods below them, are the generated-code entry points: each
// EmitX takes a pre-formatted body string (from
// `runtime.Format(items)`), the hierarchical name of the scope the call is
// written in, and where the call is written, which the program hands over last
// as every operation that can fail is handed it, and writes at its fixed
// severity. Each call generates its report (LRM 20.10), however often one place
// is reached.
//
// LRM 20.10 also requires every report to say when it was made. That is a fact
// of the moment rather than of the call, so it is asked of the run as each
// report is written rather than carried in an operand.
class DiagnosticDispatcher {
 public:
  // When a report is made, and the procedure it is made under, as the run
  // answers them.
  struct Context {
    // The scope the procedure runs in; none for a namespace's initializer and
    // for an action it started.
    std::optional<std::string> scope;
    SimTime time = 0;
    // Where the procedure is written; no place where none is running or no
    // source text states it.
    base::SourceLocation written_at{};
  };
  using ContextSource = std::function<Context()>;

  // Every report goes to the error stream of `files`, which hands what the
  // design wrote to standard output over first, so a report follows what was
  // printed before it wherever the two streams meet.
  explicit DiagnosticDispatcher(FileTable& files);

  // The run has to exist before it can be asked where it is, so the source is
  // installed rather than taken at construction.
  void SetContextSource(ContextSource context);

  void EmitInfo(
      const lyra::value::String& text, const lyra::value::String& scope,
      base::SourceLocation at);
  void EmitWarning(
      const lyra::value::String& text, const lyra::value::String& scope,
      base::SourceLocation at);
  void EmitError(
      const lyra::value::String& text, const lyra::value::String& scope,
      base::SourceLocation at);
  void EmitFatal(
      const lyra::value::String& text, const lyra::value::String& scope,
      base::SourceLocation at);

  // The same reports made by a body that reports at its caller: its scopes
  // mean nothing to the design's author, so each names the procedure that was
  // running, as a failure the tool raises does.
  void EmitInfoAtCaller(
      const lyra::value::String& text, base::SourceLocation at);
  void EmitWarningAtCaller(
      const lyra::value::String& text, base::SourceLocation at);
  void EmitErrorAtCaller(
      const lyra::value::String& text, base::SourceLocation at);
  void EmitFatalAtCaller(
      const lyra::value::String& text, base::SourceLocation at);

  // A report the tool writes for the design: placed where the operation being
  // carried out was written, which is what is bound, and naming the scope of
  // the procedure that was running, as a panic names its thread. `notes`
  // follow it, each a line of its own, after the one saying where and when.
  void Report(
      Severity severity, const base::DiagMessage& message,
      std::span<const base::DiagMessage> notes = {});

  // A report the run makes for no operation of the design, placed at the
  // procedure it is made under.
  void Report(
      base::NoSourceLocation no_location, Severity severity,
      const base::DiagMessage& message,
      std::span<const base::DiagMessage> notes = {});

  // A report of a failure already raised, placed where the raise kept: past the
  // raise nothing is bound, and no place there is a raise of the run's own.
  void ReportAt(
      base::SourceLocation at, Severity severity,
      const base::DiagMessage& message);

  // A whole line about the run rather than about the design, opening with its
  // own word: what a simulation control task did (LRM 20.2, Table 20-1), what
  // kept a time slot from ending, and why the tool could not carry on. It is
  // written every time, having no call site to attribute or to count.
  void Line(std::string_view line);

  // LRM 20.10: a fatal report terminates the simulation with an error code, so
  // whether one was made is what the run's exit status answers to.
  [[nodiscard]] auto ReportedFatal() const -> bool;

 private:
  // A report the design's code makes, naming the scope that code is written
  // in.
  void EmitInScope(
      Severity severity, std::string_view body, std::string_view scope,
      base::SourceLocation at);
  // A report of the tool's own, opening with `word` and naming the procedure it
  // is made under.
  void EmitUnderProcedure(
      Severity severity, std::string_view word, base::SourceLocation at,
      std::string_view body, std::span<const base::DiagMessage> notes);
  // `scope` is what the note names, none where the report is made in no scope.
  void Emit(
      Severity severity, std::string_view word, base::SourceLocation at,
      std::string_view body, std::optional<std::string_view> scope,
      SimTime time, std::span<const base::DiagMessage> notes);
  void Write(std::string_view text);

  FileTable* files_;
  ContextSource context_;
  bool reported_fatal_ = false;
};

// The place a report names for `at`, as "file:line:col" (LRM 20.10); empty
// where `at` is no place.
[[nodiscard]] auto SourceLocationText(base::SourceLocation at) -> std::string;

}  // namespace lyra::runtime
