#include <iostream>
#include <lyra/driver/driver.hpp>
#include <lyra/driver/driver_options.hpp>
#include <string>
#include <vector>

using Driver = lyra::driver::Driver;
using DriverOptions = lyra::driver::DriverOptions;

auto main() -> int {
  const std::vector<std::string> test_file_paths = {"src/main/test.sv"};
  DriverOptions options = {.dump_lir = true};
  auto result = Driver::RunFromFiles(test_file_paths, options);

  // std::cout << "[ Simulation Result ]\n";
  // std::cout << "pc      = " << result.ReadVariable("pc") << "\n";
  // std::cout << "reg0    = " << result.ReadVariable("reg0") << "\n";
  // std::cout << "reg1    = " << result.ReadVariable("reg1") << "\n";
  // std::cout << "halted  = " << result.ReadVariable("halted") << "\n";

  std::cout << "[ Simulation Result ]\n";
  std::cout << "r1      = " << result.ReadVariable("r1") << "\n";
  std::cout << "r2      = " << result.ReadVariable("r2") << "\n";
  std::cout << "r3      = " << result.ReadVariable("r3") << "\n";
  std::cout << "r4      = " << result.ReadVariable("r4") << "\n";

  return 0;
}
