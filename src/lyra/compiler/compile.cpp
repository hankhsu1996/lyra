#include "lyra/compiler/compile.hpp"

#include <memory>
#include <optional>
#include <utility>

#include <slang/ast/Compilation.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/driver/Driver.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

namespace lyra::compiler {

auto RunFrontEnd(slang::driver::Driver& driver) -> FrontEndResult {
  FrontEndResult result;

  auto parse = frontend::Elaborate(driver);
  if (!parse) {
    result.diagnostics = driver.textDiagClient->getString();
    return result;
  }

  // Running AST->HIR over an AST slang rejected would bury the front end's own
  // account under follow-on errors, so an error here answers with no
  // elaboration at all.
  if (!frontend::ReportSlangDiagnostics(
          driver, *parse->compilation, result.diagnostics)) {
    return result;
  }

  result.elaborated = std::move(*parse);
  return result;
}

auto LowerToHir(
    std::unique_ptr<slang::ast::Compilation> elaborated,
    const frontend::SlangSourceMapper& source_mapper, LoweringPolicy policy,
    diag::DiagnosticSink& sink) -> std::optional<ElaboratedDesign> {
  // Taking the AST is what ends its life, so a run has exactly one of these to
  // make and nothing to fall back on if it is asked for twice.
  if (elaborated == nullptr) {
    throw InternalError(
        "LowerToHir: the elaborated design has already been "
        "taken, so there is none left to lower");
  }
  lowering::ast_to_hir::SensitivityAnalyzer sensitivity_analyzer;
  const lowering::ast_to_hir::LowerCompilationFacts facts(
      *elaborated, source_mapper, sensitivity_analyzer, policy.assertions);
  auto tops = lowering::ast_to_hir::TopLevelUnits(facts);
  if (!tops) {
    sink.Report(std::move(tops.error()));
    return std::nullopt;
  }
  ElaboratedDesign design{
      .tops = *std::move(tops),
      .hir = lowering::ast_to_hir::LowerCompilationToHir(facts, sink)};
  if (sink.HasErrors()) {
    return std::nullopt;
  }
  return design;
}

}  // namespace lyra::compiler
