#include "pipeline.hpp"

#include <fmt/core.h>

#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "print.hpp"

namespace lyra::driver {

auto CompileToMir(const std::vector<std::string>& files)
    -> std::expected<CompilationResult, std::string> {
  auto parse_result = LoadFiles(files);
  if (!parse_result) {
    return std::unexpected("failed to parse input files");
  }

  DiagnosticSink sink;
  auto hir_result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result->compilation, sink);

  if (sink.HasErrors()) {
    return std::unexpected(FormatDiagnostics(sink));
  }

  lowering::hir_to_mir::LoweringInput mir_input{
      .design = &hir_result.design,
      .hir_arena = hir_result.hir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .constant_arena = hir_result.constant_arena.get(),
      .symbol_table = hir_result.symbol_table.get(),
      .builtin_types = {},
  };

  lowering::hir_to_mir::LoweringResult mir_result;
  try {
    mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);
  } catch (const std::exception& e) {
    return std::unexpected(
        fmt::format("HIR to MIR lowering failed: {}", e.what()));
  }

  return CompilationResult{
      .hir = std::move(hir_result),
      .mir = std::move(mir_result),
  };
}

}  // namespace lyra::driver
