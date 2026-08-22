#include "lyra/frontend/load.hpp"

#include <optional>
#include <string>

#include <slang/ast/Compilation.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/driver/CompatSettings.h>
#include <slang/driver/Driver.h>
#include <slang/text/SourceManager.h>

#include "lyra/diag/source_manager.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"

namespace lyra::frontend {

namespace {

// Copies every buffer slang loaded into Lyra's own source table, so a span
// raised during lowering renders against the same text the front end read.
// Driven from the loaded set rather than from what the caller named, because
// an include or a filelist entry reaches the compilation without ever being
// named on the command line.
void RegisterBuffers(
    const slang::SourceManager& sources, diag::SourceManager& diag_sources,
    SlangSourceMapper& mapper) {
  for (const auto buffer : sources.getAllBuffers()) {
    if (mapper.Contains(buffer)) {
      continue;
    }
    const auto file_id = diag_sources.AddFile(
        sources.getFullPath(buffer).string(),
        std::string(sources.getSourceText(buffer)));
    mapper.Register(buffer, file_id);
  }
}

// Where Lyra reads SystemVerilog differently from the tool whose front end it
// borrows. Applied before the caller's own options, so the caller still
// overrides any of it.
void ApplyBaseline(slang::driver::Driver& driver) {
  if (!driver.options.languageVersion) {
    driver.options.languageVersion = "1800-2023";
  }

  // slang answers to the standard; Lyra answers to the designs people already
  // simulate, and those were written against tools that depart from it in
  // well-known ways. Rejecting a design every other simulator runs helps
  // nobody, so the tolerant reading is the default and strictness is asked
  // for. This is the whole of that policy: no per-diagnostic list of Lyra's
  // own, which would drift from the front end that defines them.
  if (!driver.options.compat) {
    driver.options.compat = slang::driver::CompatMode::Vcs;
  }
}

}  // namespace

auto Elaborate(slang::driver::Driver& driver) -> std::optional<ParseResult> {
  ApplyBaseline(driver);
  if (!driver.processOptions() || !driver.parseAllSources()) {
    return std::nullopt;
  }

  ParseResult out;
  out.compilation = driver.createCompilation();
  RegisterBuffers(driver.sourceManager, out.diag_sources, out.source_mapper);
  return out;
}

auto ReportSlangDiagnostics(
    slang::driver::Driver& driver, slang::ast::Compilation& compilation,
    std::string& out_text) -> bool {
  for (const auto& diagnostic : compilation.getAllDiagnostics()) {
    driver.diagEngine.issue(diagnostic);
  }
  out_text = driver.textDiagClient->getString();
  return driver.diagEngine.getNumErrors() == 0;
}

}  // namespace lyra::frontend
