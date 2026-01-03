#include <iostream>
#include <lyra/interpreter/interpreter.hpp>
#include <lyra/interpreter/interpreter_options.hpp>
#include <string>
#include <vector>

using Interpreter = lyra::interpreter::Interpreter;
using InterpreterOptions = lyra::interpreter::InterpreterOptions;

auto main() -> int {
  const std::vector<std::string> test_file_paths = {"src/main/test.sv"};
  InterpreterOptions options = {.dump_lir = true};
  auto result = Interpreter::RunFromFiles(test_file_paths, options);

  std::cout << "[ Simulation Result ]\n";
  std::cout << "pc      = " << result.ReadVariable("pc") << "\n";
  std::cout << "reg0    = " << result.ReadVariable("reg0") << "\n";
  std::cout << "reg1    = " << result.ReadVariable("reg1") << "\n";
  std::cout << "halted  = " << result.ReadVariable("halted") << "\n";

  return 0;
}
