#include <iostream>
#include <string>
#include <vector>

#include "driver/driver.hpp"
#include "driver/driver_options.hpp"

using Driver = lyra::driver::Driver;
using DriverOptions = lyra::driver::DriverOptions;

auto main() -> int {
  const std::vector<std::string> test_file_paths = {"src/main/test.sv"};
  DriverOptions options = {.dump_lir = true};
  auto result = Driver::RunFromFiles(test_file_paths, options);

  std::cout << "[ Simulation Result ]\n";
  std::cout << "pc      = " << result.ReadVariable("pc") << "\n";
  std::cout << "reg0    = " << result.ReadVariable("reg0") << "\n";
  std::cout << "reg1    = " << result.ReadVariable("reg1") << "\n";
  std::cout << "halted  = " << result.ReadVariable("halted") << "\n";

  return 0;
}
