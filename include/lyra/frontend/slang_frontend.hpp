#pragma once

#include <memory>
#include <string>
#include <vector>

namespace slang {
class SourceManager;
}

namespace slang::syntax {
class SyntaxTree;
}

namespace slang::ast {
class Compilation;
}

namespace lyra::frontend {

struct FrontendOptions {
  std::vector<std::string> include_dirs;
};

class SlangFrontend {
 public:
  SlangFrontend();

  auto LoadFromFiles(
      const std::vector<std::string>& paths,
      const FrontendOptions& options = {})
      -> std::unique_ptr<slang::ast::Compilation>;

  auto LoadFromString(
      const std::string& code, const std::string& name = "input.sv")
      -> std::unique_ptr<slang::ast::Compilation>;

 private:
  std::shared_ptr<slang::SourceManager> source_manager_;
  std::vector<std::shared_ptr<slang::syntax::SyntaxTree>> owned_trees_;

  void AddTree(
      const std::shared_ptr<slang::syntax::SyntaxTree>& tree,
      slang::ast::Compilation& compilation);
};

}  // namespace lyra::frontend
