#include "dump.hpp"

#include <expected>
#include <iostream>
#include <optional>

#include "compilation_output.hpp"
#include "driver_output_options.hpp"
#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/hir/dumper.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/mir/dumper.hpp"

namespace lyra::driver {

auto DumpHir(const CompilationInput& input) -> int {
  CompilationOutput output(BuildDumpDriverOutputOptions(input));

  std::optional<ParseResult> parse_result;
  {
    PhaseTimer timer(output, Phase::kParse);
    parse_result = ParseFiles(input);
  }
  if (!parse_result) {
    output.Flush();
    return 1;
  }

  {
    PhaseTimer timer(output, Phase::kElaborate);
    if (!Elaborate(*parse_result, input)) {
      output.Flush();
      return 1;
    }
  }

  DiagnosticSink sink;
  lowering::ast_to_hir::LoweringResult result;
  {
    PhaseTimer timer(output, Phase::kLowerHir);
    lowering::ast_to_hir::HirLoweringOptions hir_options{
        .disable_assertions = input.disable_assertions,
    };
    result = lowering::ast_to_hir::LowerAstToHir(
        *parse_result->compilation, sink, hir_options);
  }

  if (sink.HasErrors()) {
    output.PrintDiagnostics(sink, result.source_manager.get());
    output.Flush();
    return 1;
  }

  hir::Dumper dumper(
      result.hir_arena.get(), result.type_arena.get(),
      result.constant_arena.get(), result.symbol_table.get(), &std::cout);
  dumper.Dump(result.design);

  output.Flush();
  return 0;
}

auto DumpMir(const CompilationInput& input) -> int {
  CompilationOutput output(BuildDumpDriverOutputOptions(input));

  std::optional<ParseResult> parse_result;
  {
    PhaseTimer timer(output, Phase::kParse);
    parse_result = ParseFiles(input);
  }
  if (!parse_result) {
    output.Flush();
    return 1;
  }

  {
    PhaseTimer timer(output, Phase::kElaborate);
    if (!Elaborate(*parse_result, input)) {
      output.Flush();
      return 1;
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
    output.PrintDiagnostics(sink, hir_result.source_manager.get());
    output.Flush();
    return 1;
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
    output.PrintDiagnostic(mir_result.error(), *hir_result.source_manager);
    output.Flush();
    return 1;
  }

  mir::Dumper dumper(
      mir_result->design_arena.get(), hir_result.type_arena.get(), &std::cout);
  dumper.Dump(mir_result->design);

  output.Flush();
  return 0;
}

auto DumpDpiHeader(const CompilationInput& input) -> int {
  CompilationOutput output(BuildDumpDriverOutputOptions(input));

  std::optional<ParseResult> parse_result;
  {
    PhaseTimer timer(output, Phase::kParse);
    parse_result = ParseFiles(input);
  }
  if (!parse_result) {
    output.Flush();
    return 1;
  }

  {
    PhaseTimer timer(output, Phase::kElaborate);
    if (!Elaborate(*parse_result, input)) {
      output.Flush();
      return 1;
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
    output.PrintDiagnostics(sink, hir_result.source_manager.get());
    output.Flush();
    return 1;
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
    output.PrintDiagnostic(mir_result.error(), *hir_result.source_manager);
    output.Flush();
    return 1;
  }

  std::cout << mir_result->dpi_header;

  output.Flush();
  return 0;
}

auto DumpLlvm(const CompilationInput& input) -> int {
  CompilationOutput output(BuildDumpDriverOutputOptions(input));

  std::optional<ParseResult> parse_result;
  {
    PhaseTimer timer(output, Phase::kParse);
    parse_result = ParseFiles(input);
  }
  if (!parse_result) {
    output.Flush();
    return 1;
  }

  {
    PhaseTimer timer(output, Phase::kElaborate);
    if (!Elaborate(*parse_result, input)) {
      output.Flush();
      return 1;
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
    output.PrintDiagnostics(sink, hir_result.source_manager.get());
    output.Flush();
    return 1;
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
    output.PrintDiagnostic(mir_result.error(), *hir_result.source_manager);
    output.Flush();
    return 1;
  }

  lowering::OriginMapLookup origin_lookup(
      &mir_result->design_origins, &mir_result->body_origins,
      &hir_result.design, hir_result.hir_arena.get());
  lowering::DiagnosticContext diag_ctx(origin_lookup);

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &mir_result->design,
      .mir_arena = mir_result->design_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .diag_ctx = &diag_ctx,
      .source_manager = hir_result.source_manager.get(),
      .fs_base_dir = input.fs_base_dir.string(),
      .plusargs = {},
      .feature_flags = 0,
      .signal_trace_path = {},
      .iteration_limit = 0,
      .force_two_state = input.two_state,
      .collect_forwarding_analysis =
          output.IsEnabled(OutputCategory::kAnalysis),
      .dpi_export_wrappers = &mir_result->dpi_export_wrappers,
  };
  std::expected<lowering::mir_to_llvm::LoweringResult, Diagnostic> llvm_result;
  {
    PhaseTimer timer(output, Phase::kLowerLlvm);
    llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  }
  if (!llvm_result) {
    output.PrintDiagnostic(llvm_result.error(), *hir_result.source_manager);
    output.Flush();
    return 1;
  }

  std::cout << lowering::mir_to_llvm::DumpLlvmIr(*llvm_result);

  output.Flush();
  return 0;
}

}  // namespace lyra::driver
