#include "lyra/frontend/slang_frontend.hpp"

#include <memory>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <slang/ast/Compilation.h>
#include <slang/diagnostics/DiagnosticEngine.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
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
      // File not found or couldn't be opened
      auto [error_code, error_msg] = result.error();
      fmt::print(stderr, "error: {}: {}\n", path, error_msg);
      return nullptr;
    }
    AddTree(result.value(), *compilation);
  }

  // Check for parse/elaboration diagnostics
  auto diagnostics = compilation->getAllDiagnostics();
  if (!diagnostics.empty()) {
    // Set up diagnostic engine with our warning policies
    slang::DiagnosticEngine diag_engine(*source_manager_);
    auto diag_client = std::make_shared<slang::TextDiagnosticClient>();
    diag_client->showColors(true);
    diag_engine.addClient(diag_client);

    // Promote certain warnings to errors - these warnings result in invalid
    // AST nodes that we cannot lower, so they must be treated as errors
    std::vector<std::string> warning_options = {
        "error=finish-num",  // Invalid $fatal/$finish argument
    };
    diag_engine.setWarningOptions(warning_options);

    // Issue all diagnostics through the engine
    for (const auto& diag : diagnostics) {
      diag_engine.issue(diag);
    }

    // TextDiagnosticClient buffers output - print it
    fmt::print(stderr, "{}", diag_client->getString());

    // Check if there were any errors (including promoted warnings)
    if (diag_engine.getNumErrors() > 0) {
      return nullptr;
    }
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
