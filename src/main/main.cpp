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
  const auto& val = result.ReadVariable("a");
  std::cout << "a = " << val << "\n";
  const auto& val2 = result.ReadVariable("b");
  std::cout << "b = " << val2 << "\n";
  const auto& val3 = result.ReadVariable("c");
  std::cout << "c = " << val3 << "\n";

  return 0;
}
