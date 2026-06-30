#include "lyra/compiler/compile.hpp"

#include <utility>
#include <vector>

#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

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
  hir_units.reserve(bodies.size());
  mir_units.reserve(bodies.size());
  lir_units.reserve(bodies.size());

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
  }

  result.artifacts.hir_units = std::move(hir_units);
  if (want_mir) {
    result.artifacts.mir_units = std::move(mir_units);
  }
  if (want_lir) {
    result.artifacts.lir_units = std::move(lir_units);
  }

  return result;
}

}  // namespace lyra::compiler
