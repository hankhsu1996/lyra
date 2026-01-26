#include "frontend.hpp"

#include <memory>
#include <optional>
#include <utility>

#include <fmt/color.h>
#include <fmt/core.h>
#include <slang/diagnostics/DiagnosticEngine.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/parsing/Preprocessor.h>
#include <slang/syntax/SyntaxTree.h>
#include <slang/util/Bag.h>
#include <slang/util/LanguageVersion.h>

namespace lyra::driver {

namespace {
constexpr auto kToolColor = fmt::terminal_color::white;
constexpr auto kToolStyle = fmt::fg(kToolColor) | fmt::emphasis::bold;
}  // namespace

auto LoadFiles(const CompilationInput& input) -> std::optional<ParseResult> {
  if (input.files.empty()) {
    fmt::print(
        stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
        fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
        fmt::styled("no input files", fmt::emphasis::bold));
    return std::nullopt;
  }

  auto source_manager = std::make_shared<slang::SourceManager>();

  for (const auto& dir : input.incdir) {
    source_manager->addUserDirectories(dir);
  }

  slang::Bag options;
  if (!input.defines.empty()) {
    slang::parsing::PreprocessorOptions pp_options;
    pp_options.predefines = input.defines;
    options.set(pp_options);
  }

  slang::ast::CompilationOptions comp_options;
  comp_options.languageVersion = slang::LanguageVersion::v1800_2023;
  if (!input.top.empty()) {
    comp_options.topModules.emplace(input.top);
  }
  auto compilation = std::make_unique<slang::ast::Compilation>(comp_options);

  for (const auto& path : input.files) {
    auto result =
        slang::syntax::SyntaxTree::fromFile(path, *source_manager, options);
    if (!result) {
      fmt::print(
          stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
          fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
          fmt::styled(
              fmt::format("cannot read '{}'", path), fmt::emphasis::bold));
      return std::nullopt;
    }
    compilation->addSyntaxTree(result.value());
  }

  auto diagnostics = compilation->getAllDiagnostics();
  if (!diagnostics.empty()) {
    slang::DiagnosticEngine diag_engine(*source_manager);
    auto diag_client = std::make_shared<slang::TextDiagnosticClient>();
    diag_client->showColors(true);
    diag_engine.addClient(diag_client);

    auto warnings = input.warnings;
    // Warnings that produce invalid AST nodes must be errors
    warnings.emplace_back("error=finish-num");
    diag_engine.setWarningOptions(warnings);

    for (const auto& diag : diagnostics) {
      diag_engine.issue(diag);
    }

    fmt::print(stderr, "{}", diag_client->getString());

    if (diag_engine.getNumErrors() > 0) {
      return std::nullopt;
    }
  }

  return ParseResult{
      .source_manager = std::move(source_manager),
      .compilation = std::move(compilation),
  };
}

}  // namespace lyra::driver
