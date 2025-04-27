#include "frontend/slang_frontend.hpp"

#include <memory>

#include <slang/ast/Compilation.h>
#include <slang/syntax/SyntaxTree.h>
#include <slang/text/SourceManager.h>

namespace lyra::frontend {

auto LoadCompilation(std::vector<std::string> files)
    -> std::unique_ptr<slang::ast::Compilation> {
  static std::shared_ptr<slang::SourceManager> source_manager =
      std::make_shared<slang::SourceManager>();
  static std::vector<std::shared_ptr<slang::syntax::SyntaxTree>> owned_trees;

  auto compilation = std::make_unique<slang::ast::Compilation>();

  for (const auto& file : files) {
    auto result = slang::syntax::SyntaxTree::fromFile(file, *source_manager);
    if (!result) {
      throw std::runtime_error("Failed to parse file: " + file);
    }

    auto tree = result.value();
    owned_trees.push_back(tree);
    compilation->addSyntaxTree(tree);
  }

  return compilation;
}

}  // namespace lyra::frontend
