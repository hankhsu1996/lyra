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

auto DeclareUnits(
    std::unique_ptr<slang::ast::Compilation> elaborated,
    const frontend::SlangSourceMapper& source_mapper, LoweringPolicy policy,
    diag::DiagnosticSink& sink) -> std::optional<ElaboratedDesign> {
  // Taking the AST is what hands it to the units, so a run has exactly one of
  // these to make and nothing to fall back on if it is asked for twice.
  if (elaborated == nullptr) {
    throw InternalError(
        "DeclareUnits: the elaborated design has already been taken, so there "
        "is none left to declare");
  }
  auto tops = lowering::ast_to_hir::TopLevelUnits(
      lowering::ast_to_hir::LowerCompilationFacts(
          *elaborated, source_mapper, policy.assertions));
  if (!tops) {
    sink.Report(std::move(tops.error()));
    return std::nullopt;
  }
  auto units = lowering::ast_to_hir::DeclaredDesign::Declare(
      std::move(elaborated), source_mapper, policy.assertions, sink);
  if (!units) {
    return std::nullopt;
  }
  return ElaboratedDesign{.tops = *std::move(tops), .units = *std::move(units)};
}

}  // namespace lyra::compiler
