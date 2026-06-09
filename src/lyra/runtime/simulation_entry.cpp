#include "lyra/runtime/simulation_entry.hpp"

#include <cstdlib>
#include <exception>
#include <new>
#include <print>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

auto RunSimulation(Engine& engine) -> int {
  try {
    return engine.Run();
  } catch (const InternalError& e) {
    std::print(stderr, "{}\n", e.what());
    return EXIT_FAILURE;
  } catch (const std::bad_alloc&) {
    std::print(stderr, "out of memory\n");
    return EXIT_FAILURE;
  } catch (const std::exception& e) {
    std::print(stderr, "unexpected error: {}\n", e.what());
    return EXIT_FAILURE;
  }
}

}  // namespace lyra::runtime
