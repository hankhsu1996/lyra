#pragma once

#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "lyra/compiler/compiler_result.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::compiler {

class Compiler {
 public:
  static auto RunFromSource(
      const std::string& code,
      const std::vector<std::string>& variables_to_read) -> CompilerResult;

  static auto RunFromFiles(
      const std::vector<std::string>& paths,
      const std::vector<std::string>& variables_to_read) -> CompilerResult;

 private:
  static auto CompileAndRun(
      const std::vector<std::unique_ptr<mir::Module>>& modules,
      const std::vector<std::string>& variables_to_read,
      std::optional<std::filesystem::path> base_dir) -> CompilerResult;
};

}  // namespace lyra::compiler
