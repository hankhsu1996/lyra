#include "lyra/runtime/simulation_entry.hpp"

#include <cstdlib>
#include <exception>
#include <iostream>
#include <new>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

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
