#include "lyra/compiler/compile.hpp"

#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/driver/Driver.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/compiler/design_root.hpp"
#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/compiler/unit_pipeline.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

namespace {

// One place for the message, so a missing stage always reads the same whichever
// accessor found it.
template <typename T>
auto StageProduct(const std::optional<T>& product, std::string_view stage)
    -> const T& {
  if (!product.has_value()) {
    throw InternalError(
        std::format(
            "CompileArtifacts: no {} to read; the pipeline stopped before that "
            "stage",
            stage));
  }
  return *product;
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

  lowering::ast_to_hir::SensitivityAnalyzer sensitivity_analyzer;
  const lowering::ast_to_hir::LowerCompilationFacts facts(
      *result.artifacts.parse->compilation,
      result.artifacts.parse->source_mapper, sensitivity_analyzer,
      policy.disable_assertions);
  result.artifacts.top_unit_names = lowering::ast_to_hir::TopLevelUnitNames(
      *result.artifacts.parse->compilation);

  // Step 1: lower the whole compilation to a flat set of self-contained HIR
  // units -- every package, then every module body -- each tagged with its
  // kind.
  auto hir_units = lowering::ast_to_hir::LowerCompilationToHir(facts);
  if (!hir_units) {
    sink.Report(std::move(hir_units.error()));
    return result;
  }

  // Step 2: lower each unit down its own vertical (HIR -> MIR -> LIR),
  // independently -- a unit reads only itself and the shared frontend. The
  // parallel result vectors preserve the output contract: `mir_units` is a
  // superset of `lir_units`, since a package lowers to MIR but has no
  // executable body, and `unit_metadata` stays co-indexed with `lir_units`.
  const bool want_mir = stop_after >= StopAfter::kMir;
  const bool want_lir = stop_after >= StopAfter::kLir;
  std::vector<mir::CompilationUnit> mir_units;
  std::vector<lir::CompilationUnit> lir_units;
  std::vector<ElaboratedUnitMetadata> unit_metadata;
  mir_units.reserve(hir_units->size());
  lir_units.reserve(hir_units->size());
  unit_metadata.reserve(hir_units->size());
  for (const auto& hir_unit : *hir_units) {
    auto unit = LowerUnitPipeline(
        hir_unit, stop_after, result.artifacts.parse->diag_sources);
    if (!unit) {
      sink.Report(std::move(unit.error()));
      return result;
    }
    if (unit->mir) {
      mir_units.push_back(*std::move(unit->mir));
    }
    if (unit->lir) {
      lir_units.push_back(*std::move(unit->lir));
    }
    if (unit->metadata) {
      unit_metadata.push_back(*std::move(unit->metadata));
    }
  }

  // Step 3: write the design-root unit. The one whole-design step -- it reads
  // across the units to synthesize the root and resolve the package
  // initialization plan -- so it stands apart from the per-unit lowering
  // above.
  std::optional<mir::CompilationUnit> root_unit;
  std::optional<lir::CompilationUnit> root_lir_unit;
  std::optional<ElaboratedUnitMetadata> root_metadata;
  if (want_mir) {
    auto design_root = SynthesizeDesignRoot(
        mir_units, result.artifacts.top_unit_names, stop_after,
        result.artifacts.parse->diag_sources);
    if (!design_root) {
      sink.Report(std::move(design_root.error()));
      return result;
    }
    root_unit = std::move(design_root->mir);
    root_lir_unit = std::move(design_root->lir);
    root_metadata = std::move(design_root->metadata);
  }

  result.artifacts.hir_units = std::move(*hir_units);
  if (want_mir) {
    result.artifacts.mir_units = std::move(mir_units);
    result.artifacts.root_unit = std::move(root_unit);
  }
  if (want_lir) {
    result.artifacts.lir_units = std::move(lir_units);
    result.artifacts.unit_metadata = std::move(unit_metadata);
    result.artifacts.root_lir_unit = std::move(root_lir_unit);
    result.artifacts.root_metadata = std::move(root_metadata);
  }
  return result;
}

auto CompileArtifacts::HirUnits() const
    -> const std::vector<hir::CompilationUnit>& {
  return StageProduct(hir_units, "HIR");
}

auto CompileArtifacts::MirUnits() const
    -> const std::vector<mir::CompilationUnit>& {
  return StageProduct(mir_units, "MIR");
}

auto CompileArtifacts::RootUnit() const -> const mir::CompilationUnit& {
  return StageProduct(root_unit, "design-root MIR");
}

auto CompileArtifacts::LirUnits() const
    -> const std::vector<lir::CompilationUnit>& {
  return StageProduct(lir_units, "LIR");
}

auto CompileArtifacts::RootLirUnit() const -> const lir::CompilationUnit& {
  return StageProduct(root_lir_unit, "design-root LIR");
}

auto CompileArtifacts::UnitMetadata() const
    -> const std::vector<ElaboratedUnitMetadata>& {
  return StageProduct(unit_metadata, "unit metadata");
}

auto CompileArtifacts::RootMetadata() const -> const ElaboratedUnitMetadata& {
  return StageProduct(root_metadata, "design-root metadata");
}

}  // namespace lyra::compiler
