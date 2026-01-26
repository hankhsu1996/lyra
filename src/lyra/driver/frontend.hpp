#pragma once

#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/text/SourceManager.h>

namespace lyra::driver {

struct CompilationInput {
  std::vector<std::string> files;
  std::string top;
  std::vector<std::string> incdir;
  std::vector<std::string> defines;
  std::vector<std::string> warnings;
  std::filesystem::path fs_base_dir;  // Base directory for runtime file I/O
};

struct ParseResult {
  std::shared_ptr<slang::SourceManager> source_manager;
  std::unique_ptr<slang::ast::Compilation> compilation;
};

auto LoadFiles(const CompilationInput& input) -> std::optional<ParseResult>;

}  // namespace lyra::driver
