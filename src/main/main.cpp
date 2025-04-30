#include <iostream>
#include <string>
#include <vector>

#include "simulation/simulate.hpp"
#include "simulation/simulation_options.hpp"

auto main() -> int {
  const std::vector<std::string> test_file_paths = {"src/main/test.sv"};
  lyra::SimulationOptions options = {.dump_lir = true};
  auto result = lyra::RunFromFiles(test_file_paths, options);

  std::cout << "[ Simulation Result ]\n";
  for (const auto& name : result.context->signal_table.DeclaredSignalNames()) {
    const auto& val = result.ReadSignal(name);
    std::cout << name << " = " << val.AsInt() << "\n";
  }

  return 0;
}
