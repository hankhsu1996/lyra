#include "lyra/runtime/simulation_entry.hpp"

#include <cstddef>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <new>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

auto RunDesignHost(int argc, char** argv, RootBuilder builder) -> int {
  // LRM 21.6: `+`-prefixed argv entries are plusargs; strip the `+` so a
  // stored token compares directly against a user-supplied prefix.
  const std::span<char*> args{argv, static_cast<std::size_t>(argc)};
  std::vector<std::string> plusargs;
  for (std::size_t i = 1; i < args.size(); ++i) {
    const std::string_view view{args[i]};
    if (view.starts_with("+")) {
      plusargs.emplace_back(view.substr(1));
    }
  }
  auto options = DefaultEngineOptions();
  options.plusargs = std::move(plusargs);
  Engine engine{std::move(options)};
  auto root = builder(engine.Services());
  engine.BindDesign(std::move(root));
  return RunSimulation(engine);
}

auto RunSimulation(Engine& engine) -> int {
  try {
    return engine.Run();
  } catch (const InternalError& e) {
    std::cerr << e.what() << "\n";
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
