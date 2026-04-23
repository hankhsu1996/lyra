#include "lyra/frontend/load.hpp"

#include <memory>
#include <optional>

#include <fmt/color.h>
#include <fmt/core.h>
#include <slang/ast/Compilation.h>
#include <slang/diagnostics/DiagnosticEngine.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/text/SourceManager.h>
#include <slang/util/Bag.h>
#include <slang/util/Util.h>

#include "compiler_env.hpp"
#include "parse_unit.hpp"

namespace lyra::frontend {

namespace {
constexpr auto kToolColor = fmt::terminal_color::white;
constexpr auto kToolStyle = fmt::fg(kToolColor) | fmt::emphasis::bold;

void ReportError(std::string_view message) {
  fmt::print(
      stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
      fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
      fmt::styled(message, fmt::emphasis::bold));
}
}  // namespace

auto ParseFiles(const CompilationInput& input) -> std::optional<ParseResult> {
  if (input.files.empty()) {
    ReportError("no input files");
    return std::nullopt;
  }

  auto source_manager = std::make_shared<slang::SourceManager>();

  slang::Bag options;
  options.set(BuildLyraPreprocessorOptions());

  slang::ast::CompilationOptions comp_options;
  comp_options.languageVersion = slang::LanguageVersion::v1800_2023;
  if (!input.top.empty()) {
    comp_options.topModules.emplace(input.top);
  }

  auto compilation = std::make_unique<slang::ast::Compilation>(comp_options);

  auto plan = BuildParsePlan(input.files, CompilationUnitMode::kPerFile);
  for (const auto& unit : plan.units) {
    if (!ExecuteParseUnit(unit, *source_manager, *compilation, options)) {
      return std::nullopt;
    }
  }

  return ParseResult{
      .source_manager = std::move(source_manager),
      .compilation = std::move(compilation),
  };
}

auto Elaborate(ParseResult& result) -> bool {
  auto diagnostics = result.compilation->getAllDiagnostics();
  if (diagnostics.empty()) {
    return true;
  }

  slang::DiagnosticEngine diag_engine(*result.source_manager);
  auto diag_client = std::make_shared<slang::TextDiagnosticClient>();
  diag_client->showColors(true);
  diag_engine.addClient(diag_client);

  for (const auto& diag : diagnostics) {
    diag_engine.issue(diag);
  }

  fmt::print(stderr, "{}", diag_client->getString());

  return diag_engine.getNumErrors() == 0;
}

auto LoadFiles(const CompilationInput& input) -> std::optional<ParseResult> {
  auto result = ParseFiles(input);
  if (!result) {
    return std::nullopt;
  }
  if (!Elaborate(*result)) {
    return std::nullopt;
  }
  return result;
}

}  // namespace lyra::frontend
