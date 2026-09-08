#include "lyra/runtime/simulation_entry.hpp"

#include <cstddef>
#include <cstdlib>
#include <exception>
#include <memory>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

auto RunDesignHost(int argc, char** argv, const RootBuilder& builder) -> int {
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

  // Building the design and resolving its references is elaboration, which
  // precedes the simulation (LRM 3.12). An error here has no activation to
  // leave and no final procedure to reach, so it is reported and the run never
  // starts.
  Scope* root_scope = nullptr;
  try {
    auto root = builder();
    root_scope = root.get();
    runtime.BindDesign(std::make_unique<Design>(std::move(root)));
  } catch (const std::exception&) {
    ReportRaisedError(runtime, std::current_exception());
    return EXIT_FAILURE;
  }

  const AmbientRunContext run_context{root_scope, runtime};
  return RunSimulation(runtime);
}

auto RunSimulation(Runtime& runtime) -> int {
  try {
    return runtime.Run();
  } catch (const std::exception&) {
    ReportRaisedError(runtime, std::current_exception());
    return EXIT_FAILURE;
  }
}

}  // namespace lyra::runtime
