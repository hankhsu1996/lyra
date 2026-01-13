#pragma once

#include <cstdint>
#include <filesystem>
#include <map>
#include <mutex>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "tests/framework/test_case.hpp"

namespace lyra::test {

struct BatchTestResult {
  bool success = false;
  std::string error_message;
  std::unordered_map<std::string, std::variant<int64_t, double>> variables;
  uint64_t final_time = 0;
  std::string captured_output;
};

class BatchCompiler {
 public:
  static auto Instance() -> BatchCompiler&;

  auto RunTest(
      const TestCase& test_case,
      const std::vector<std::string>& variables_to_read) -> BatchTestResult;

 private:
  BatchCompiler() = default;

  void EnsurePrepared();
  void CollectTestsForShard();
  void PrepareBatch();
  auto Execute(size_t index, const std::vector<std::string>& variables)
      -> BatchTestResult;

  struct PreparedTest {
    std::string name;
    std::string namespace_name;
    std::string top_module_name;
    std::string wrapped_code;
    std::filesystem::path work_dir;
    std::vector<std::string> plusargs;
    std::vector<std::string> variables;
    int8_t global_precision_power = -12;  // Default: 1ps
  };

  auto PrepareTest(const TestCase& test, size_t index) -> PreparedTest;
  auto GenerateBatchMain() -> std::string;
  void Compile();

  std::vector<TestCase> test_cases_;
  std::vector<PreparedTest> prepared_tests_;
  std::map<std::string, size_t> name_to_index_;
  std::filesystem::path batch_dir_;
  std::filesystem::path binary_path_;
  std::once_flag preparation_flag_;
  bool prepared_ = false;
  std::string preparation_error_;
};

}  // namespace lyra::test
