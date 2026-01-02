#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace lyra::test {

class CppTestResult {
 public:
  [[nodiscard]] auto Success() const -> bool { return success_; }
  [[nodiscard]] auto ErrorMessage() const -> const std::string& {
    return error_message_;
  }
  [[nodiscard]] auto ReadVariable(const std::string& name) const -> int64_t;

 private:
  friend class CppTestRunner;

  bool success_ = false;
  std::string error_message_;
  std::unordered_map<std::string, int64_t> variables_;
};

class CppTestRunner {
 public:
  static auto RunFromSource(
      const std::string& sv_code,
      const std::vector<std::string>& variables_to_read) -> CppTestResult;
};

}  // namespace lyra::test
