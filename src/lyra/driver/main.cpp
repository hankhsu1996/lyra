#include <string>
#include <vector>

#include <fmt/core.h>

#include "dump.hpp"
#include "run.hpp"

namespace {

void PrintUsage() {
  fmt::print(stderr, "Usage: lyra <command> [args]\n\n");
  fmt::print(stderr, "Commands:\n");
  fmt::print(stderr, "  dump hir <file.sv>  Dump HIR representation\n");
  fmt::print(stderr, "  dump mir <file.sv>  Dump MIR representation\n");
  fmt::print(stderr, "  run mir <file.sv>   Interpret MIR\n");
}

}  // namespace

auto main(int argc, char* argv[]) -> int {
  std::vector<std::string> args(argv + 1, argv + argc);

  if (args.size() < 3) {
    PrintUsage();
    return 1;
  }

  if (args[0] == "dump" && args[1] == "hir") {
    return lyra::driver::DumpHir(args[2]);
  }

  if (args[0] == "dump" && args[1] == "mir") {
    return lyra::driver::DumpMir(args[2]);
  }

  if (args[0] == "run" && args[1] == "mir") {
    return lyra::driver::RunMir(args[2]);
  }

  PrintUsage();
  return 1;
}
