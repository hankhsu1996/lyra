#include "lyra/compiler/compile.hpp"

#include <utility>
#include <vector>

#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/hir_to_mir/lower_module_unit.hpp"

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
  auto hir = lowering::ast_to_hir::LowerCompilation(
      lowering::ast_to_hir::LowerCompilationFacts(
          *result.artifacts.parse->compilation,
          result.artifacts.parse->source_mapper, sensitivity_analyzer));
  if (!hir) {
    sink.Report(std::move(hir.error()));
    return result;
  }
  result.artifacts.hir_units = std::move(*hir);

  if (stop_after == StopAfter::kHir) {
    return result;
  }

  std::vector<mir::CompilationUnit> units;
  units.reserve(result.artifacts.hir_units->size());
  for (const auto& hir_unit : *result.artifacts.hir_units) {
    auto mir = lowering::hir_to_mir::LowerModuleUnit(hir_unit);
    if (!mir) {
      sink.Report(std::move(mir.error()));
      return result;
    }
    units.push_back(std::move(*mir));
  }
  result.artifacts.mir_units = std::move(units);

  return result;
}

}  // namespace lyra::compiler
