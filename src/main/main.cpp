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
  const auto& val = result.ReadVariable("money");
  std::cout << "money = " << val << "\n";
  const auto& val2 = result.ReadVariable("vend");
  std::cout << "vend = " << val2 << "\n";

  return 0;
}
