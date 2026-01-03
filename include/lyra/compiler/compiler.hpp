#pragma once

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
      const mir::Module& mir, const std::vector<std::string>& variables_to_read)
      -> CompilerResult;
};

}  // namespace lyra::compiler
