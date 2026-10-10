#include "lyra/runtime/simulation_entry.hpp"

#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <memory>
#include <new>
#include <span>
#include <string>
#include <string_view>
#include <unistd.h>
#include <utility>
#include <vector>

#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/interrupt_report.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"

namespace lyra::runtime {

namespace {

// What a refused request for memory runs, where the request would otherwise
// have answered. Anything carried on from here may ask again -- a message
// being composed, a frame being left, a `final` procedure -- so the line is one
// that needed nothing to write and the program ends where it stands. Standard
// output is the process's own and holds nothing a refused request can have
// left half made, so every line the design finished there is delivered first.
[[noreturn]] void EndForLackOfMemory() {
  std::cout.flush();
  std::fflush(stdout);
  std::string_view left = "lyra: out of memory\n";
  while (!left.empty()) {
    const ssize_t wrote = ::write(STDERR_FILENO, left.data(), left.size());
    if (wrote <= 0) {
      break;
    }
    left.remove_prefix(static_cast<std::size_t>(wrote));
  }
  std::_Exit(EXIT_FAILURE);
}

// Drives a bound Runtime to completion. The run accounts for a design's own
// run-time error and for a failure of the tool discovered while it is under
// way, so what is reported here is what happened outside that: an invariant
// the engine established for itself.
auto RunSimulation(Runtime& runtime) -> int {
  try {
    // Foreign code reaches the run with plain C arguments and nothing to find
    // it by, so what it resolves a scope against is anchored for as long as the
    // run lasts -- which starts at the first thing a run does, initializing
    // state, since a context import can already be reached from there (LRM
    // 10.5, 35.5.3).
    const AmbientRunContext run_context{&runtime.DesignRoot(), runtime};
    const InterruptReport interrupt_report{runtime};
    return runtime.Run();
  } catch (const std::exception&) {
    ReportRaisedError(runtime, std::current_exception());
    return EXIT_FAILURE;
  }
}

}  // namespace

auto RunDesignRoot(
    int argc, char** argv, std::string_view root_name, const RootFactory& make)
    -> int {
  std::set_new_handler(EndForLackOfMemory);

  // A built program's own argv leads with its name, which is not one of the
  // simulation's arguments.
  const std::span<char*> args{argv, static_cast<std::size_t>(argc)};
  std::vector<std::string> arguments;
  for (std::size_t i = 1; i < args.size(); ++i) {
    arguments.emplace_back(args[i]);
  }
  auto options = DefaultRuntimeOptions();
  options.plusargs = PlusargsFrom(arguments);
  Runtime runtime{std::move(options)};

  // Building the design and resolving its references precede the simulation
  // (LRM 3.12). An error here has no activation to leave and no final procedure
  // to reach, so it is reported and the run never starts.
  try {
    runtime.BindDesign(
        std::make_unique<Design>(
            make(nullptr, HierarchySegment{std::string{root_name}, {}})));
  } catch (const std::exception&) {
    ReportRaisedError(runtime, std::current_exception());
    return EXIT_FAILURE;
  }

  return RunSimulation(runtime);
}
}  // namespace lyra::runtime
