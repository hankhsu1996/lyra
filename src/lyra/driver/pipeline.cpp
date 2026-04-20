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
  lowering::ast_to_hir::AstToHirOutput ast_to_hir;
  {
    PhaseTimer timer(output, Phase::kLowerHir);
    lowering::ast_to_hir::HirLoweringOptions hir_options{
        .disable_assertions = input.disable_assertions,
    };
    ast_to_hir = lowering::ast_to_hir::LowerAstToHir(
        *parse_result->compilation, sink, hir_options);
  }

  auto& hir_result = ast_to_hir.hir;
  auto& composition = ast_to_hir.composition;

  if (sink.HasErrors()) {
    return std::unexpected(
        CompilationError::FromDiagnostics(
            std::move(sink), std::move(hir_result.source_manager)));
  }

  lowering::hir_to_mir::LoweringInput mir_input{
      .design = &hir_result.design,
      .module_bodies = &hir_result.module_bodies,
      .hir_arena = hir_result.hir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .active_constant_arena = hir_result.constant_arena.get(),
      .symbol_table = hir_result.symbol_table.get(),
      .builtin_types = {},
      .binding_plan = &composition.binding_plan,
      .global_precision_power = composition.global_precision_power,
      .instance_table = &composition.instance_table,
      .specialization_map = &composition.specialization_map,
      .child_coord_map = &composition.child_coord_map,
      .body_timescales = &composition.body_timescales,
      .hierarchy_nodes = &composition.hierarchy_nodes,
      .dpi_export_signatures = &composition.dpi_export_signatures,
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
      .composition = std::move(composition),
      .mir = std::move(*mir_result),
  };
}

}  // namespace lyra::driver
