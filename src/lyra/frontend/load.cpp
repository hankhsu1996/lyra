#include "lyra/frontend/load.hpp"

#include <optional>
#include <string>

#include <slang/analysis/AnalysisManager.h>
#include <slang/ast/Compilation.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/driver/Driver.h>

#include "lyra/frontend/slang_source_manager.hpp"

namespace lyra::frontend {

namespace {

// Where Lyra reads SystemVerilog differently from the tool whose front end it
// borrows. Applied before the caller's own options, so the caller still
// overrides any of it.
void ApplyBaseline(slang::driver::Driver& driver) {
  if (!driver.options.languageVersion) {
    driver.options.languageVersion = "1800-2023";
  }
}

}  // namespace

auto Elaborate(slang::driver::Driver& driver) -> std::optional<ParseResult> {
  ApplyBaseline(driver);
  if (!driver.processOptions() || !driver.parseAllSources()) {
    return std::nullopt;
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
