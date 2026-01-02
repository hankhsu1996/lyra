#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

#include "tests/framework/test_case.hpp"

namespace lyra::test {

class CppTestResult {
 public:
  [[nodiscard]] auto Success() const -> bool {
    return success_;
  }
  [[nodiscard]] auto ErrorMessage() const -> const std::string& {
    return error_message_;
  }
  [[nodiscard]] auto ReadVariable(const std::string& name) const -> int64_t;
  [[nodiscard]] auto FinalTime() const -> uint64_t {
    return final_time_;
  }

 private:
  friend class CppTestRunner;

  bool success_ = false;
  std::string error_message_;
  std::unordered_map<std::string, int64_t> variables_;
  uint64_t final_time_ = 0;
};

class CppTestRunner {
 public:
  // Single-file test
  static auto RunFromSource(
      const std::string& sv_code,
      const std::vector<std::string>& variables_to_read) -> CppTestResult;

  // Multi-file test
  static auto RunFromSources(
      const std::vector<SourceFile>& files,
      const std::vector<std::string>& variables_to_read) -> CppTestResult;
};

}  // namespace lyra::test
