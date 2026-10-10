#include <cstddef>
#include <cstdio>
#include <exception>
#include <span>
#include <string>
#include <utility>

#include <fmt/core.h>
#include <slang/driver/Driver.h>
#include <slang/text/SourceManager.h>

#include "lyra/cli/command_line.hpp"
#include "lyra/cli/commands.hpp"
#include "lyra/compiler/stack.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/diag/render.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/driver/signals.hpp"
#include "lyra/frontend/slang_source_manager.hpp"
#include "lyra/status/status.hpp"

auto main(int argc, char** argv) -> int {
  try {
    lyra::driver::EndOnSignal();
    lyra::compiler::GiveStartingThreadCompileStack();
    const std::span<char* const> raw_args(argv, static_cast<std::size_t>(argc));
    const std::string program_path =
        raw_args.empty() ? std::string{} : std::string(raw_args.front());
    auto argv_split = lyra::cli::SplitAtSeparator(raw_args);

    slang::driver::Driver driver;
    driver.addStandardArgs();
    lyra::cli::CliOptions cli_options;
    lyra::cli::RegisterCliOptions(driver.cmdLine, cli_options);

    if (lyra::cli::WantsHelp(argv_split.lyra)) {
      lyra::cli::PrintHelp(driver);
      return 0;
    }

    auto command = lyra::cli::ParseCommandWords(driver, argv_split.lyra);

    const bool use_color = lyra::cli::UseColor(cli_options);
    const lyra::frontend::SlangSourceManager sources(driver.sourceManager);
    const lyra::cli::Reporter report{
        lyra::diag::RenderOptions{
            .use_color = use_color,
            .show_remarks = cli_options.remarks.value_or(false)},
        sources};

    if (!command) {
      // An empty message means the parser already printed its own account of
      // what was wrong with the command line.
      if (!command.error().empty()) {
        report(
            lyra::diag::Make(
                lyra::diag::DiagCode::kHostInvalidCliArgs, command.error()));
      }
      return 1;
    }
    driver.setTerminalColorsEnabled(use_color);
    lyra::cli::PredefineToolIdentity(driver);

    if (auto taken = lyra::cli::RefuseOptionsNotTaken(
            cli_options, *command, !argv_split.child.empty());
        !taken) {
      report(
          lyra::diag::Make(
              lyra::diag::DiagCode::kHostInvalidCliArgs, taken.error()));
      return 1;
    }

    auto self_report = lyra::cli::StartSelfReport(cli_options);
    if (!self_report) {
      report(std::move(self_report.error()));
      return 1;
    }
    const auto look = lyra::cli::StatusLookOf(*command, cli_options, use_color);
    if (!look) {
      report(
          lyra::diag::Make(
              lyra::diag::DiagCode::kHostInvalidCliArgs, look.error()));
      return 1;
    }
    const lyra::status::Shown shown(*look);
    const int exit_code = lyra::cli::RunCommand(
        lyra::cli::Invocation{
            .driver = &driver,
            .options = &cli_options,
            .command = *command,
            .simulation_args = std::move(argv_split.child),
            .program_path = program_path,
            .report = &report});
    lyra::status::Clear();
    if (auto written = lyra::cli::WriteSelfReport(*self_report); !written) {
      report(std::move(written.error()));
      return exit_code == 0 ? 1 : exit_code;
    }
    return exit_code;
  } catch (const std::exception& failure) {
    // Nothing a command read is left here, so the failure is shown against no
    // source at all: plain, and about no place, since a place it still names
    // is one of a source that is gone.
    const slang::SourceManager nothing_read;
    const lyra::frontend::SlangSourceManager sources(nothing_read);
    lyra::diag::Diagnostic report = lyra::diag::InternalFailure(failure);
    report.primary.span = lyra::diag::UnknownSpan{};
    lyra::diag::DiagnosticSink sink;
    sink.Report(std::move(report));
    fmt::print(
        stderr, "{}",
        lyra::diag::RenderDiagnostics(
            sink, sources, lyra::diag::RenderOptions{.use_color = false}));
    return lyra::cli::kCompilerFailureExit;
  }
}
