#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

#include "lyra/interpreter/interpreter_options.hpp"
#include "lyra/interpreter/interpreter_result.hpp"

namespace slang {
class SourceManager;
}

namespace slang::ast {
class Compilation;
}

namespace lyra::interpreter {

class Interpreter {
 public:
  Interpreter() = default;

  static auto RunFromSource(
      const std::string& code, const std::string& top = "",
      const InterpreterOptions& options = {}) -> InterpreterResult;

  static auto RunFromFiles(
      const std::vector<std::string>& paths, const std::string& top = "",
      const InterpreterOptions& options = {}) -> InterpreterResult;

 private:
  static auto RunWithCompilation(
      std::unique_ptr<slang::ast::Compilation> compilation,
      const std::string& top, const InterpreterOptions& options,
      std::optional<std::filesystem::path> base_dir) -> InterpreterResult;
};

}  // namespace lyra::interpreter
