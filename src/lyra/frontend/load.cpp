#include "lyra/frontend/load.hpp"

#include <memory>
#include <optional>
#include <string>

#include <slang/ast/Compilation.h>
#include <slang/diagnostics/DiagnosticEngine.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/text/SourceManager.h>
#include <slang/util/Bag.h>

#include "lyra/diag/sink.hpp"
#include "lyra/frontend/compiler_env.hpp"
#include "lyra/frontend/parse_unit.hpp"

namespace lyra::frontend {

auto LoadFiles(const CompilationInput& input, diag::DiagnosticSink& sink)
    -> std::optional<ParseResult> {
  ParseResult out;
  out.source_manager = std::make_shared<slang::SourceManager>();
  for (const auto& dir : input.incdirs) {
    (void)out.source_manager->addUserDirectories(dir);
  }

  slang::Bag options;
  options.set(BuildLyraPreprocessorOptions(input.defines));

  slang::ast::CompilationOptions comp_options;
  comp_options.languageVersion = slang::LanguageVersion::v1800_2023;
  if (!input.top.empty()) {
    comp_options.topModules.emplace(input.top);
  }
  comp_options.paramOverrides = input.param_overrides;

  out.compilation = std::make_unique<slang::ast::Compilation>(comp_options);

  const auto mode = input.single_unit ? CompilationUnitMode::kSingleUnit
                                      : CompilationUnitMode::kPerFile;
  auto plan = BuildParsePlan(input.files, mode);
  for (const auto& unit : plan.units) {
    if (!ExecuteParseUnit(
            unit, *out.source_manager, *out.compilation, options,
            out.diag_sources, out.source_mapper, sink)) {
      return std::nullopt;
    }
  }
  return out;
}

auto RenderSlangDiagnostics(
    ParseResult& parse, bool use_color, std::string& out_text) -> bool {
  auto diagnostics = parse.compilation->getAllDiagnostics();
  if (diagnostics.empty()) {
    out_text.clear();
    return true;
  }
  slang::DiagnosticEngine diag_engine(*parse.source_manager);
  auto client = std::make_shared<slang::TextDiagnosticClient>();
  client->showColors(use_color);
  diag_engine.addClient(client);
  for (const auto& d : diagnostics) {
    diag_engine.issue(d);
  }
  out_text = client->getString();
  return diag_engine.getNumErrors() == 0;
}

auto HasSlangErrors(ParseResult& parse) -> bool {
  auto diagnostics = parse.compilation->getAllDiagnostics();
  if (diagnostics.empty()) {
    return false;
  }
  slang::DiagnosticEngine diag_engine(*parse.source_manager);
  for (const auto& d : diagnostics) {
    diag_engine.issue(d);
  }
  return diag_engine.getNumErrors() > 0;
}

}  // namespace lyra::frontend
