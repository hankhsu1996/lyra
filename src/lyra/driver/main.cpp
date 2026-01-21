#include <string>
#include <vector>

#include <fmt/color.h>
#include <fmt/core.h>

#include "config.hpp"
#include "dump.hpp"
#include "run.hpp"

namespace {

constexpr auto kToolColor = fmt::terminal_color::white;
constexpr auto kToolStyle = fmt::fg(kToolColor) | fmt::emphasis::bold;

void PrintUsage() {
  fmt::print(stderr, "Usage: lyra <command> [args]\n\n");
  fmt::print(stderr, "Commands:\n");
  fmt::print(
      stderr,
      "  run [files...]      Run simulation (uses lyra.toml if no files)\n");
  fmt::print(stderr, "  dump hir <file.sv>  Dump HIR representation\n");
  fmt::print(stderr, "  dump mir <file.sv>  Dump MIR representation\n");
  fmt::print(stderr, "  dump llvm <file.sv> Dump LLVM IR\n");
}

void PrintError(const std::string& message) {
  fmt::print(
      stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
      fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
      fmt::styled(message, fmt::emphasis::bold));
}

}  // namespace

auto main(int argc, char* argv[]) -> int {
  std::vector<std::string> args(argv + 1, argv + argc);

  if (args.empty()) {
    PrintUsage();
    return 1;
  }

  if (args[0] == "run") {
    // Collect files from remaining arguments
    std::vector<std::string> cli_files(args.begin() + 1, args.end());

    std::vector<std::string> files;

    // Try to load lyra.toml
    auto config_path = lyra::driver::FindConfig();
    if (config_path) {
      try {
        auto config = lyra::driver::LoadConfig(*config_path);
        // Start with config files
        files = config.files;
        // Add CLI files (additive)
        files.insert(files.end(), cli_files.begin(), cli_files.end());
      } catch (const std::exception& e) {
        PrintError(e.what());
        return 1;
      }
    } else if (!cli_files.empty()) {
      // No config, use CLI files
      files = cli_files;
    } else {
      PrintError("no lyra.toml found and no files specified");
      return 1;
    }

    return lyra::driver::RunMir(files);
  }

  if (args[0] == "dump" && args.size() >= 3) {
    if (args[1] == "hir") {
      return lyra::driver::DumpHir(args[2]);
    }
    if (args[1] == "mir") {
      return lyra::driver::DumpMir(args[2]);
    }
    if (args[1] == "llvm") {
      return lyra::driver::DumpLlvm(args[2]);
    }
  }

  PrintUsage();
  return 1;
}
