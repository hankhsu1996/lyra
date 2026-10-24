#include "lyra/frontend/load.hpp"

#include <array>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include <slang/analysis/AnalysisManager.h>
#include <slang/ast/Compilation.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/driver/Driver.h>
#include <slang/parsing/Token.h>
#include <slang/syntax/AllSyntax.h>
#include <slang/syntax/SyntaxTree.h>
#include <slang/syntax/SyntaxVisitor.h>

#include "lyra/frontend/coverage_library_text.hpp"
#include "lyra/frontend/library_texts.hpp"
#include "lyra/frontend/slang_source_manager.hpp"
#include "lyra/frontend/std_definitions_text.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::frontend {

namespace {

// Whether a text asks for coverage to be counted: it declares a covergroup
// (LRM 19.3), or calls a function answering over what covergroups counted
// (LRM 19.9). It is read off the parsed text, before the compilation exists,
// as a compiler driver reads off its arguments which runtime a program links.
struct AsksForCoverage : slang::syntax::SyntaxVisitor<AsksForCoverage> {
  bool asks = false;

  void visitToken(slang::parsing::Token token) {
    asks = asks || Asks(token);
  }

  static auto Asks(slang::parsing::Token token) -> bool {
    if (token.kind == slang::parsing::TokenKind::CoverGroupKeyword) {
      return true;
    }
    if (token.kind != slang::parsing::TokenKind::SystemIdentifier) {
      return false;
    }
    const support::SystemSubroutineDesc* called =
        support::FindSystemSubroutine(token.valueText());
    return called != nullptr &&
           std::holds_alternative<
               support::CoverageDatabaseSystemSubroutineInfo>(called->semantic);
  }
};

auto AnyAsksForCoverage(
    std::span<const std::shared_ptr<slang::syntax::SyntaxTree>> trees) -> bool {
  AsksForCoverage visitor;
  for (const auto& tree : trees) {
    tree->root().visit(visitor);
  }
  return visitor.asks;
}

// A text of Lyra's own, read as the buffer `name`.
auto ParseLibraryText(
    slang::driver::Driver& driver, std::string_view text, std::string_view name)
    -> std::shared_ptr<slang::syntax::SyntaxTree> {
  return slang::syntax::SyntaxTree::fromText(
      text, driver.sourceManager, "", name, driver.createParseOptionBag());
}

// Where Lyra reads SystemVerilog differently from the tool whose front end it
// borrows. Applied before the caller's own options, so the caller still
// overrides any of it.
void ApplyBaseline(slang::driver::Driver& driver) {
  if (!driver.options.languageVersion) {
    driver.options.languageVersion = "1800-2023";
  }

  // A select whose constant position lies outside its vector or array is given
  // a value by LRM 11.5.1 and 7.4.5, and 11.5.1 also lets a tool refuse it
  // while compiling. The front end refuses; Lyra runs it with the standard's
  // value and warns, because code written for a larger configuration selects
  // past the end in a smaller one.
  constexpr std::array kOutOfRangeSelectsAsWarnings = {
      "no-error=index-oob", "no-error=range-oob", "no-error=range-width-oob"};
  auto& warnings = driver.options.warningOptions;
  warnings.insert(
      warnings.begin(), kOutOfRangeSelectsAsWarnings.begin(),
      kOutOfRangeSelectsAsWarnings.end());
}

}  // namespace

auto Elaborate(slang::driver::Driver& driver) -> std::optional<ParseResult> {
  ApplyBaseline(driver);
  if (!driver.processOptions() || !driver.parseAllSources()) {
    return std::nullopt;
  }
  // Every design reads the definitions of `std`'s classes beside its own
  // sources, as a C++ program reads its standard library's headers, and the
  // coverage library only where it asks for coverage.
  const bool asks_for_coverage = AnyAsksForCoverage(driver.syntaxTrees);
  driver.syntaxTrees.push_back(
      ParseLibraryText(driver, kStdDefinitionsText, kStdDefinitions.buffer));
  if (asks_for_coverage) {
    driver.syntaxTrees.push_back(ParseLibraryText(
        driver, kCoverageLibraryText, kCoverageLibrary.buffer));
  }

  return ParseResult{
      .compilation = driver.createCompilation(),
      .diag_sources = SlangSourceManager(driver.sourceManager)};
}

auto ReportSlangDiagnostics(
    slang::driver::Driver& driver, slang::ast::Compilation& compilation,
    std::string& out_text) -> bool {
  for (const auto& diagnostic : compilation.getAllDiagnostics()) {
    driver.diagEngine.issue(diagnostic);
  }

  // What a program may connect is decided over the whole elaborated design
  // rather than over any one declaration -- which drivers reach a net, and
  // whether a net type admits that many (LRM 6.6.2, 6.6.7) -- and the front end
  // answers it in a pass of its own. Running that pass through the same engine
  // keeps one account of what the front end has to say and one error count.
  driver.runAnalysis(compilation);

  out_text = driver.textDiagClient->getString();
  return driver.diagEngine.getNumErrors() == 0;
}

}  // namespace lyra::frontend
