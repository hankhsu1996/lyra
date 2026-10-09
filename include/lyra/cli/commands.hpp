#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>
#include <slang/driver/Driver.h>

#include "lyra/cli/command_line.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/render.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/driver/claimed_file.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/status/status.hpp"

namespace lyra::cli {

// Turns a diagnostic into terminal output. Constructed once, after the
// terminal has been inspected, and handed to every command so none of them
// re-decides how rendering works.
class Reporter {
 public:
  explicit Reporter(diag::RenderOptions opts) : opts_(opts) {
  }

  void operator()(
      diag::Diagnostic diag, const diag::SourceManager* mgr = nullptr) const {
    status::Clear();
    fmt::print(stderr, "{}", diag::RenderDiagnostic(diag, mgr, opts_));
  }

  void operator()(
      const diag::DiagnosticSink& sink, const diag::SourceManager* mgr) const {
    status::Clear();
    fmt::print(stderr, "{}", diag::RenderDiagnostics(sink, mgr, opts_));
  }

  // The same rendering with warnings left out.
  [[nodiscard]] auto WithoutWarnings() const -> Reporter {
    diag::RenderOptions opts = opts_;
    opts.show_warnings = false;
    return Reporter{opts};
  }

 private:
  diag::RenderOptions opts_;
};

// A command line whose command is known and whose options that command acts
// on: everything read before deciding what the command needs. A command that
// reads a design finds and elaborates it from here; one that reads none never
// looks for one, so a declaration elsewhere on the machine cannot stop it.
struct Invocation {
  slang::driver::Driver* driver;
  const CliOptions* options;
  CommandKind command;
  std::vector<std::string> simulation_args;
  std::string_view program_path;
  const Reporter* report;
};

// The exit status of a run in which the compiler failed through a fault of its
// own, which a caller tells apart from a design it refused.
inline constexpr int kCompilerFailureExit = 2;

// Carries out the command and answers with the process exit code.
auto RunCommand(const Invocation& invocation) -> int;

// The files a run was asked to write about itself, each claimed before the
// command starts.
struct SelfReport {
  std::optional<driver::ClaimedFile> time_trace;
  std::optional<driver::ClaimedFile> statistics;
};

// Whether the run records where its cost went is settled before the command
// starts, so the record covers all of it and a file that cannot be written is
// refused before the work it would have described. What was recorded is written
// once the command is done.
auto StartSelfReport(const CliOptions& options) -> diag::Result<SelfReport>;
auto WriteSelfReport(SelfReport& report) -> diag::Result<void>;

// What a command that reads a design receives: the request, what the front end
// elaborated from it, and the channel for anything it has to report. A command
// reads this and returns the process exit code; nothing else about the
// invocation is visible to it.
//
// The elaboration is not const: a command that reads past it lowers it to HIR
// and takes the AST as it does, because that is where the front end's own
// account of the design stops being read. How far past it a command goes is
// the command's own business.
//
// A command writes what it has to report into the sink and renders nothing.
// Every stage above it already writes there, so one account covers the whole
// run and what reaches the terminal is decided in one place -- which is also
// why a command answering with nothing always means the same thing. Members are
// non-owning pointers rather than references: this outlives nothing, and a
// reference member would make the type unassignable for no gain.
struct CommandContext {
  const ParsedArgs* args;
  frontend::ParseResult* elaborated;
  diag::DiagnosticSink* sink;
  std::span<const driver::DpiLinkInput> dpi_inputs;
  std::string_view program_path;
};

}  // namespace lyra::cli
