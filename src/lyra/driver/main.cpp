#include <string>
#include <vector>

#include <fmt/core.h>

#include "dump.hpp"

namespace {

void PrintUsage() {
  fmt::print(stderr, "Usage: lyra dump hir <file.sv>\n");
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

  PrintUsage();
  return 1;
}
