#include "pipeline.hpp"

#include <expected>
#include <utility>

#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "verbose_logger.hpp"

namespace lyra::driver {

auto CompileToMir(const CompilationInput& input, VerboseLogger& vlog)
    -> std::expected<CompilationResult, CompilationError> {
  std::optional<ParseResult> parse_result;
  {
    PhaseTimer timer(vlog, "parse");
    parse_result = ParseFiles(input);
  }
  if (!parse_result) {
    return std::unexpected(
        CompilationError::Simple("failed to parse input files"));
  }

  {
    PhaseTimer timer(vlog, "elaborate");
    if (!Elaborate(*parse_result, input)) {
      return std::unexpected(
          CompilationError::Simple("failed to elaborate design"));
    }
  }

  DiagnosticSink sink;
  lowering::ast_to_hir::LoweringResult hir_result;
  {
    PhaseTimer timer(vlog, "lower_hir");
    hir_result =
        lowering::ast_to_hir::LowerAstToHir(*parse_result->compilation, sink);
  }

  if (sink.HasErrors()) {
    return std::unexpected(
        CompilationError::FromDiagnostics(
            std::move(sink), std::move(hir_result.source_manager)));
  }

  lowering::hir_to_mir::LoweringInput mir_input{
      .design = &hir_result.design,
      .hir_arena = hir_result.hir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .constant_arena = hir_result.constant_arena.get(),
      .symbol_table = hir_result.symbol_table.get(),
      .builtin_types = {},
      .binding_plan = &hir_result.binding_plan,
      .global_precision_power = hir_result.global_precision_power,
      .instance_table = &hir_result.instance_table,
  };

  std::expected<lowering::hir_to_mir::LoweringResult, Diagnostic> mir_result;
  {
    PhaseTimer timer(vlog, "lower_mir");
    mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);
  }

  if (!mir_result) {
    DiagnosticSink error_sink;
    error_sink.Report(mir_result.error());
    return std::unexpected(
        CompilationError::FromDiagnostics(
            std::move(error_sink), std::move(hir_result.source_manager)));
  }

  return CompilationResult{
      .hir = std::move(hir_result),
      .mir = std::move(*mir_result),
  };
}

}  // namespace lyra::driver
