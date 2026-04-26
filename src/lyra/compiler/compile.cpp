#include "lyra/compiler/compile.hpp"

#include <format>
#include <utility>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
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

  auto hir = lowering::ast_to_hir::LowerCompilation(
      lowering::ast_to_hir::LowerCompilationFacts(
          *result.artifacts.parse->compilation,
          result.artifacts.parse->source_mapper));
  if (!hir) {
    sink.Report(std::move(hir.error()));
    return result;
  }
  result.artifacts.hir_units = std::move(*hir);

  if (stop_after == StopAfter::kHir) {
    return result;
  }

  if (result.artifacts.hir_units->size() != 1) {
    sink.Report(
        diag::Diagnostic::HostError(
            diag::DiagCode::kHostExpectedSingleTopModule,
            std::format(
                "expected exactly one top module, got {}",
                result.artifacts.hir_units->size())));
    return result;
  }

  auto mir = lowering::hir_to_mir::LowerModuleUnit(
      result.artifacts.hir_units->front());
  if (!mir) {
    sink.Report(std::move(mir.error()));
    return result;
  }
  result.artifacts.mir_unit = std::move(*mir);

  return result;
}

}  // namespace lyra::compiler
