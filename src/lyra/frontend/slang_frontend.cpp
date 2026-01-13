#include "lyra/frontend/slang_frontend.hpp"

#include <format>
#include <memory>
#include <string>
#include <system_error>
#include <vector>

#include <fmt/core.h>
#include <slang/ast/Compilation.h>
#include <slang/diagnostics/DiagnosticEngine.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/numeric/Time.h>
#include <slang/syntax/SyntaxTree.h>
#include <slang/text/SourceManager.h>
#include <slang/util/LanguageVersion.h>

#include "lyra/common/diagnostic.hpp"

namespace lyra::frontend {

namespace {

// Default timescale: 1ps/1ps (no scaling, matches Verilator behavior)
// This is Lyra's implementation-specific default per LRM when no timescale
// is specified anywhere in the design.
auto MakeDefaultTimeScale() -> slang::TimeScale {
  slang::TimeScaleValue ps{
      slang::TimeUnit::Picoseconds, slang::TimeScaleMagnitude::One};
  return slang::TimeScale{ps, ps};
}

}  // namespace

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

  slang::ast::CompilationOptions comp_options;
  comp_options.defaultTimeScale = MakeDefaultTimeScale();
  comp_options.languageVersion = slang::LanguageVersion::v1800_2023;
  auto compilation = std::make_unique<slang::ast::Compilation>(comp_options);

  for (const auto& path : paths) {
    auto result = slang::syntax::SyntaxTree::fromFile(path, *source_manager_);
    if (!result) {
      auto error_code = result.error().first;
      if (error_code == std::errc::no_such_file_or_directory) {
        PrintDiagnostic(
            Diagnostic::Error(
                {}, std::format("source file not found: {}", path)));
      } else {
        PrintDiagnostic(
            Diagnostic::Error(
                {}, std::format(
                        "cannot read '{}': {}", path, error_code.message())));
      }
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

    // Start with user-provided warning options
    std::vector<std::string> warning_options = options.warning_options;

    // Append built-in defaults - these warnings result in invalid AST nodes
    // that we cannot lower, so they must be treated as errors
    warning_options.emplace_back("error=finish-num");

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
  slang::ast::CompilationOptions comp_options;
  comp_options.defaultTimeScale = MakeDefaultTimeScale();
  comp_options.languageVersion = slang::LanguageVersion::v1800_2023;
  auto compilation = std::make_unique<slang::ast::Compilation>(comp_options);
  auto buffer = source_manager_->assignText(name, code);
  auto tree = slang::syntax::SyntaxTree::fromBuffer(buffer, *source_manager_);
  AddTree(tree, *compilation);
  return compilation;
}

}  // namespace lyra::frontend
