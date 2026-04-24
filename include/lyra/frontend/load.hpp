#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/text/SourceManager.h>

namespace lyra::frontend {

struct CompilationInput {
  std::vector<std::string> files;
  std::string top;
  std::vector<std::string> incdirs;
  std::vector<std::string> defines;
  std::vector<std::string> param_overrides;
  bool single_unit = false;
};

struct ParseResult {
  std::shared_ptr<slang::SourceManager> source_manager;
  std::unique_ptr<slang::ast::Compilation> compilation;
};

auto ParseFiles(const CompilationInput& input) -> std::optional<ParseResult>;
auto Elaborate(ParseResult& result) -> bool;
auto LoadFiles(const CompilationInput& input) -> std::optional<ParseResult>;

}  // namespace lyra::frontend
