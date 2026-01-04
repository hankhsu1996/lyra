#include "lyra/frontend/slang_frontend.hpp"

#include <stdexcept>

#include <slang/ast/Compilation.h>
#include <slang/syntax/SyntaxTree.h>
#include <slang/text/SourceManager.h>

namespace lyra::frontend {

SlangFrontend::SlangFrontend()
    : source_manager_(std::make_shared<slang::SourceManager>()) {
}

void SlangFrontend::AddTree(
    const std::shared_ptr<slang::syntax::SyntaxTree>& tree,
    slang::ast::Compilation& compilation) {
  owned_trees_.push_back(tree);
  compilation.addSyntaxTree(tree);
}

auto SlangFrontend::LoadFromFiles(
    const std::vector<std::string>& paths, const FrontendOptions& options)
    -> std::unique_ptr<slang::ast::Compilation> {
  // Add include directories to source manager
  for (const auto& dir : options.include_dirs) {
    source_manager_->addUserDirectories(dir);
  }

  auto compilation = std::make_unique<slang::ast::Compilation>();

  for (const auto& path : paths) {
    auto result = slang::syntax::SyntaxTree::fromFile(path, *source_manager_);
    if (!result) {
      throw std::runtime_error("Failed to parse file: " + path);
    }
    AddTree(result.value(), *compilation);
  }

  return compilation;
}

auto SlangFrontend::LoadFromString(
    const std::string& code, const std::string& name)
    -> std::unique_ptr<slang::ast::Compilation> {
  auto compilation = std::make_unique<slang::ast::Compilation>();
  auto buffer = source_manager_->assignText(name, code);
  auto tree = slang::syntax::SyntaxTree::fromBuffer(buffer, *source_manager_);
  AddTree(tree, *compilation);
  return compilation;
}

}  // namespace lyra::frontend
