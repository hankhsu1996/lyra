#include "pipeline.hpp"

#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"

namespace lyra::driver {

auto CompileToMir(const CompilationInput& input)
    -> std::expected<CompilationResult, CompilationError> {
  auto parse_result = LoadFiles(input);
  if (!parse_result) {
    return std::unexpected(
        CompilationError::Simple("failed to parse input files"));
  }

  DiagnosticSink sink;
  auto hir_result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result->compilation, sink);

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
  };

  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);
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
