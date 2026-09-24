#include <cstddef>
#include <cstdio>
#include <exception>
#include <span>
#include <string>
#include <utility>

#include <fmt/core.h>
#include <slang/driver/Driver.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/cli/command_line.hpp"
#include "lyra/cli/commands.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/render.hpp"

auto main(int argc, char** argv) -> int {
  try {
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
    const lyra::cli::Reporter report{lyra::diag::RenderOptions{
        .use_color = use_color, .show_source_snippet = true}};

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

    if (auto taken = lyra::cli::RefuseOptionsNotTaken(
            cli_options, *command, !argv_split.child.empty());
        !taken) {
      report(
          lyra::diag::Make(
              lyra::diag::DiagCode::kHostInvalidCliArgs, taken.error()));
      return 1;
    }

    return lyra::cli::RunCommand(
        lyra::cli::Invocation{
            .driver = &driver,
            .options = &cli_options,
            .command = *command,
            .simulation_args = std::move(argv_split.child),
            .program_path = program_path,
            .report = &report});
  } catch (const lyra::InternalError& e) {
    fmt::print(stderr, "{}", lyra::diag::RenderInternalError(e.what()));
    return 2;
  } catch (const std::exception& e) {
    fmt::print(stderr, "lyra: error: {}\n", e.what());
    return 2;
  }
}
