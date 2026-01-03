#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>

namespace lyra::compiler {

class CompilerResult {
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
  friend class Compiler;

  bool success_ = false;
  std::string error_message_;
  std::unordered_map<std::string, int64_t> variables_;
  uint64_t final_time_ = 0;
};

}  // namespace lyra::compiler
