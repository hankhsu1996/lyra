#include "lyra/runtime/simulation_entry.hpp"

#include <cstddef>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <memory>
#include <new>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/runtime.hpp"
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
  auto root = builder();
  Scope* root_scope = root.get();
  auto design = std::make_unique<Design>(std::move(root));
  runtime.BindDesign(std::move(design));
  AmbientRunContext run_context{root_scope, runtime};
  return RunSimulation(runtime);
}

auto RunSimulation(Runtime& runtime) -> int {
  try {
    return runtime.Run();
  } catch (const SimulationError& e) {
    std::cerr << e.what() << "\n";
    return EXIT_FAILURE;
  } catch (const InternalError& e) {
    std::cerr << "internal error: " << e.what() << "\n";
    return EXIT_FAILURE;
  } catch (const std::bad_alloc&) {
    std::cerr << "out of memory\n";
    return EXIT_FAILURE;
  } catch (const std::exception& e) {
    std::cerr << "unexpected error: " << e.what() << "\n";
    return EXIT_FAILURE;
  }
}

}  // namespace lyra::runtime
