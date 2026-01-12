#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>
#include <variant>

namespace lyra::compiler {

// Value type for test assertions: integers or floating-point
using TestValue = std::variant<int64_t, double>;

class CompilerResult {
 public:
  [[nodiscard]] auto Success() const -> bool {
    return success_;
  }
  [[nodiscard]] auto ErrorMessage() const -> const std::string& {
    return error_message_;
  }
  [[nodiscard]] auto ReadVariable(const std::string& name) const -> TestValue;
  [[nodiscard]] auto FinalTime() const -> uint64_t {
    return final_time_;
  }
  [[nodiscard]] auto CapturedOutput() const -> const std::string& {
    return captured_output_;
  }

 private:
  friend class Compiler;

  bool success_ = false;
  std::string error_message_;
  std::unordered_map<std::string, TestValue> variables_;
  uint64_t final_time_ = 0;
  std::string captured_output_;
};

}  // namespace lyra::compiler
