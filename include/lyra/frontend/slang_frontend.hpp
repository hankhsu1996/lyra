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
  std::vector<std::string> defines;
  std::vector<std::string> warning_options;
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

  [[nodiscard]] auto GetSourceManager() const -> const slang::SourceManager& {
    return *source_manager_;
  }

  [[nodiscard]] auto GetSourceManagerPtr() const
      -> std::shared_ptr<slang::SourceManager> {
    return source_manager_;
  }

 private:
  std::shared_ptr<slang::SourceManager> source_manager_;
  std::vector<std::shared_ptr<slang::syntax::SyntaxTree>> owned_trees_;

  void AddTree(
      const std::shared_ptr<slang::syntax::SyntaxTree>& tree,
      slang::ast::Compilation& compilation);
};

}  // namespace lyra::frontend
