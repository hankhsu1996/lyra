#include "pipeline.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "compilation_output.hpp"
#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"

namespace lyra::driver {

auto CompileToMir(const CompilationInput& input, CompilationOutput& output)
    -> std::expected<CompilationResult, CompilationError> {
  std::optional<ParseResult> parse_result;
  {
    PhaseTimer timer(output, Phase::kParse);
    parse_result = ParseFiles(input);
  }
  if (!parse_result) {
    return std::unexpected(
        CompilationError::Simple("failed to parse input files"));
  }

  {
    PhaseTimer timer(output, Phase::kElaborate);
    if (!Elaborate(*parse_result, input)) {
      return std::unexpected(
          CompilationError::Simple("failed to elaborate design"));
    }
  }

  DiagnosticSink sink;
  lowering::ast_to_hir::LoweringResult hir_result;
  {
    PhaseTimer timer(output, Phase::kLowerHir);
    lowering::ast_to_hir::HirLoweringOptions hir_options{
        .disable_assertions = input.disable_assertions,
    };
    hir_result = lowering::ast_to_hir::LowerAstToHir(
        *parse_result->compilation, sink, hir_options);
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
      .active_constant_arena = hir_result.constant_arena.get(),
      .symbol_table = hir_result.symbol_table.get(),
      .builtin_types = {},
      .binding_plan = &hir_result.binding_plan,
      .global_precision_power = hir_result.global_precision_power,
      .instance_table = &hir_result.instance_table,
      .specialization_map = &hir_result.specialization_map,
  };

  std::expected<lowering::hir_to_mir::LoweringResult, Diagnostic> mir_result;
  {
    PhaseTimer timer(output, Phase::kLowerMir);
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
