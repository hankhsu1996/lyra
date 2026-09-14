#pragma once

#include <span>
#include <string_view>

#include <fmt/core.h>

#include "lyra/cli/command_line.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/render.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/driver/cpp_build.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/frontend/load.hpp"

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
    fmt::print(stderr, "{}", diag::RenderDiagnostic(diag, mgr, opts_));
  }

  void operator()(
      const diag::DiagnosticSink& sink, const diag::SourceManager* mgr) const {
    fmt::print(stderr, "{}", diag::RenderDiagnostics(sink, mgr, opts_));
  }

 private:
  diag::RenderOptions opts_;
};

// What a command receives: the request, what the front end elaborated from it,
// and the channel for anything that goes wrong. A command reads this and
// returns the process exit code; nothing else about the invocation is visible
// to it.
//
// The elaboration is not const: a command that reads past it lowers it to HIR
// and takes the AST as it does, because that is where the front end's own
// account of the design stops being read. How far past it a command goes is
// the command's own business.
//
// A command writes what went wrong into the sink and renders nothing. Every
// stage above it already writes there, so one account covers the whole run and
// what reaches the terminal is decided in one place -- which is also why a
// command answering with nothing always means the same thing.
// Members are non-owning pointers rather than references: this outlives
// nothing, and a reference member would make the type unassignable for no gain.
struct CommandContext {
  const ParsedArgs* args;
  frontend::ParseResult* elaborated;
  diag::DiagnosticSink* sink;
  std::span<const driver::DpiLinkInput> dpi_inputs;
  driver::SourceFormatting formatting;
  std::string_view program_path;
};

auto RunCommand(const CommandContext& ctx) -> int;

}  // namespace lyra::cli
