#include "lyra/compiler/compile.hpp"

#include <optional>
#include <utility>
#include <vector>

#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

namespace {

// A unit's definition metadata is a source-level fact known once elaboration
// fixes the unit's root scope: its def name is the root's name, its precision
// the root's declared resolution. Derived from MIR here, so the executable body
// downstream never carries these source-language concepts.
auto BuildUnitMetadata(const mir::CompilationUnit& unit)
    -> ElaboratedUnitMetadata {
  const mir::Class& root = unit.GetClass(unit.root);
  return ElaboratedUnitMetadata{
      .def_name = root.name,
      .time_precision_power = root.time_resolution.precision_power};
}

// The design-root unit is a module whose only members are the top-level units,
// instantiated as its owned children. Its constructor then elaborates the
// design through the same owned-child construction any parent uses for a
// submodule, so no code path is special-cased for the top level.
auto BuildRootHirUnit(const std::vector<std::string>& top_names)
    -> hir::ModuleUnit {
  hir::ModuleUnit root{std::string{kDesignRootUnitName}};
  for (const auto& name : top_names) {
    root.root_scope.instance_members.Add(
        hir::InstanceMemberDecl{
            .instance_name = name, .target_unit = name, .array_dims = {}});
  }
  return root;
}

}  // namespace

auto Compile(
    const frontend::CompilationInput& input, diag::DiagnosticSink& sink,
    StopAfter stop_after) -> CompileResult {
  CompileResult result;

  auto parse = frontend::LoadFiles(input, sink);
  if (!parse) {
    return result;
  }
  result.artifacts.parse = std::move(*parse);

  // Stop early if slang reports parse/elaboration errors; running
  // AST->HIR over a broken AST would emit confusing follow-on errors.
  result.slang_ok = !frontend::HasSlangErrors(*result.artifacts.parse);
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
      input.disable_assertions);
  result.artifacts.top_unit_names = lowering::ast_to_hir::TopLevelUnitNames(
      *result.artifacts.parse->compilation);

  // Each unit runs its whole lowering pipeline (HIR -> MIR -> LIR) as an
  // independent vertical: a unit reads only its own body and the shared
  // frontend, never another unit's lowered artifacts.
  const bool want_mir = stop_after >= StopAfter::kMir;
  const bool want_lir = stop_after >= StopAfter::kLir;
  if (auto ok = lowering::ast_to_hir::RejectDpiExports(facts); !ok) {
    sink.Report(std::move(ok.error()));
    return result;
  }
  const auto bodies = lowering::ast_to_hir::CollectUnitBodies(facts);

  std::vector<hir::ModuleUnit> hir_units;
  std::vector<mir::CompilationUnit> mir_units;
  std::vector<lir::CompilationUnit> lir_units;
  std::vector<ElaboratedUnitMetadata> unit_metadata;
  hir_units.reserve(bodies.size());
  mir_units.reserve(bodies.size());
  lir_units.reserve(bodies.size());
  unit_metadata.reserve(bodies.size());

  for (const auto* body : bodies) {
    auto hir_or = lowering::ast_to_hir::LowerUnit(facts, *body);
    if (!hir_or) {
      sink.Report(std::move(hir_or.error()));
      return result;
    }
    hir_units.push_back(*std::move(hir_or));
    if (!want_mir) {
      continue;
    }

    lowering::hir_to_mir::ModuleLowerer module(
        hir_units.back(), result.artifacts.parse->diag_sources);
    auto mir_or = module.Run();
    if (!mir_or) {
      sink.Report(std::move(mir_or.error()));
      return result;
    }
    mir_units.push_back(*std::move(mir_or));
    if (!want_lir) {
      continue;
    }

    auto lir_or = lowering::mir_to_lir::LowerUnit(mir_units.back());
    if (!lir_or) {
      sink.Report(std::move(lir_or.error()));
      return result;
    }
    lir_units.push_back(*std::move(lir_or));
    unit_metadata.push_back(BuildUnitMetadata(mir_units.back()));
  }

  // The design-root unit is synthesized after the source units it instantiates,
  // so it references them by the same cross-unit-by-name link every submodule
  // instantiation uses. It is a distinct compiler output, not one of the source
  // units, so it is held apart rather than mixed into `mir_units`. Its
  // constructor elaborates the design through cross-unit construction, which
  // MIR-to-LIR does not yet lower, so it is not carried to LIR; the execution
  // backend constructs each top unit directly.
  std::optional<mir::CompilationUnit> root_unit;
  if (want_mir) {
    const hir::ModuleUnit root_hir =
        BuildRootHirUnit(result.artifacts.top_unit_names);
    lowering::hir_to_mir::ModuleLowerer root_module(
        root_hir, result.artifacts.parse->diag_sources);
    auto root_mir = root_module.Run();
    if (!root_mir) {
      sink.Report(std::move(root_mir.error()));
      return result;
    }
    root_unit = *std::move(root_mir);
  }

  result.artifacts.hir_units = std::move(hir_units);
  if (want_mir) {
    result.artifacts.mir_units = std::move(mir_units);
    result.artifacts.root_unit = std::move(root_unit);
  }
  if (want_lir) {
    result.artifacts.lir_units = std::move(lir_units);
    result.artifacts.unit_metadata = std::move(unit_metadata);
  }

  return result;
}

}  // namespace lyra::compiler
