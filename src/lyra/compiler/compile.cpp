#include "lyra/compiler/compile.hpp"

#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/driver/Driver.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

namespace lyra::compiler {

namespace {

// One place for the message, so a missing stage always reads the same whichever
// accessor found it.
auto NoStageProductMessage(std::string_view stage) -> std::string {
  return std::format(
      "CompileArtifacts: no {} to read; a stage's product is kept only for a "
      "request that reads it, and this request does not",
      stage);
}

template <typename T>
auto StageProduct(const std::optional<T>& product, std::string_view stage)
    -> const T& {
  if (!product.has_value()) {
    throw InternalError(NoStageProductMessage(stage));
  }
  return *product;
}

auto LowerElaboration(
    const frontend::ParseResult& parse, LoweringPolicy policy,
    diag::DiagnosticSink& sink) -> std::optional<ElaboratedDesign> {
  lowering::ast_to_hir::SensitivityAnalyzer sensitivity_analyzer;
  const lowering::ast_to_hir::LowerCompilationFacts facts(
      *parse.compilation, parse.source_mapper, sensitivity_analyzer,
      policy.assertions);
  auto tops = lowering::ast_to_hir::TopLevelUnits(facts);
  if (!tops) {
    sink.Report(std::move(tops.error()));
    return std::nullopt;
  }
  return ElaboratedDesign{
      .tops = *std::move(tops),
      .hir = lowering::ast_to_hir::LowerCompilationToHir(facts, sink)};
}

}  // namespace

auto Compile(
    slang::driver::Driver& driver, LoweringPolicy policy,
    diag::DiagnosticSink& sink, StopAfter stop_after) -> CompileResult {
  CompileResult result;

  auto parse = frontend::Elaborate(driver);
  if (!parse) {
    result.slang_ok = false;
    result.slang_diagnostics = driver.textDiagClient->getString();
    return result;
  }
  result.artifacts.parse = std::move(*parse);

  // Every slang diagnostic is issued here, once, so the engine's error count
  // is the whole account of the front end. Running AST->HIR over an AST slang
  // rejected would bury that account under follow-on errors, so stop instead.
  result.slang_ok = frontend::ReportSlangDiagnostics(
      driver, *result.artifacts.parse->compilation, result.slang_diagnostics);
  if (!result.slang_ok) {
    return result;
  }
  if (stop_after == StopAfter::kParse) {
    return result;
  }

  // Lower the whole compilation to a flat set of self-contained HIR units --
  // every package, then every module body -- each tagged with its kind. Every
  // unit is attempted, so what the sink holds afterwards is the whole account
  // of what this compilation cannot lower; what comes back when any of them
  // failed is not the design, and nothing below runs on it.
  auto lowered = LowerElaboration(*result.artifacts.parse, policy, sink);
  if (!lowered) {
    return result;
  }

  // Every reader of the elaborated AST is above this line: no IR carries a
  // front-end node, and a diagnostic resolves its spans through the source
  // manager beside it. A whole design's worth of it is among the largest
  // things a run ever holds, so its lifetime ends where its last reader does.
  result.artifacts.parse->compilation.reset();
  if (sink.HasErrors()) {
    return result;
  }
  result.artifacts.design = std::move(*lowered);
  return result;
}

auto CompileArtifacts::Elaboration() const -> slang::ast::Compilation& {
  const frontend::ParseResult& product = StageProduct(parse, "elaboration");
  if (!product.compilation) {
    throw InternalError(NoStageProductMessage("elaboration"));
  }
  return *product.compilation;
}

auto CompileArtifacts::Design() const -> const ElaboratedDesign& {
  return StageProduct(design, "HIR");
}

auto CompileArtifacts::DesignToLower() -> ElaboratedDesign& {
  if (!design.has_value()) {
    throw InternalError(NoStageProductMessage("HIR"));
  }
  return *design;
}

}  // namespace lyra::compiler
