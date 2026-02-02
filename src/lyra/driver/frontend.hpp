#pragma once

#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/text/SourceManager.h>

#include "lyra/common/opt_level.hpp"

namespace lyra::driver {

struct CompilationInput {
  std::vector<std::string> files;
  std::string top;
  std::vector<std::string> incdir;
  std::vector<std::string> defines;
  std::vector<std::string> warnings;
  std::vector<std::string> param_overrides;  // Top-level param overrides (-G)
  std::filesystem::path fs_base_dir;   // Base directory for runtime file I/O
  std::vector<std::string> plusargs;   // Runtime plusargs for $plusargs
  OptLevel opt_level = OptLevel::kO2;  // Default O2 for CLI
};

struct ParseResult {
  std::shared_ptr<slang::SourceManager> source_manager;
  std::unique_ptr<slang::ast::Compilation> compilation;
};

auto LoadFiles(const CompilationInput& input) -> std::optional<ParseResult>;

}  // namespace lyra::driver
