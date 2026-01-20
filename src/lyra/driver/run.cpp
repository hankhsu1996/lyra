#include "run.hpp"

#include <iostream>

#include <fmt/color.h>
#include <fmt/core.h>

#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/interp/interpreter.hpp"

namespace lyra::driver {

namespace {

constexpr auto kToolColor = fmt::terminal_color::white;
constexpr auto kToolStyle = fmt::fg(kToolColor) | fmt::emphasis::bold;

void PrintDiagnostics(const DiagnosticSink& sink) {
  for (const auto& diag : sink.GetDiagnostics()) {
    const char* severity_str = nullptr;
    fmt::text_style severity_style;

    switch (diag.severity) {
      case DiagnosticSeverity::kError:
        severity_str = "error";
        severity_style = fmt::fg(fmt::terminal_color::bright_red);
        break;
      case DiagnosticSeverity::kWarning:
        severity_str = "warning";
        severity_style = fmt::fg(fmt::terminal_color::bright_yellow);
        break;
      case DiagnosticSeverity::kNote:
        severity_str = "note";
        severity_style = fmt::fg(fmt::terminal_color::bright_cyan);
        break;
    }

    fmt::print(
        stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
        fmt::styled(severity_str, severity_style),
        fmt::styled(diag.message, fmt::emphasis::bold));
  }
}

}  // namespace

auto RunMir(const std::string& path) -> int {
  auto parse_result = LoadFile(path);
  if (!parse_result) {
    return 1;
  }

  DiagnosticSink sink;
  auto hir_result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result->compilation, sink);

  if (sink.HasErrors()) {
    PrintDiagnostics(sink);
    return 1;
  }

  TypeId bit_type = hir_result.type_arena->Intern(
      TypeKind::kIntegral,
      IntegralInfo{.bit_width = 1, .is_signed = false, .is_four_state = true});
  lowering::hir_to_mir::LoweringInput mir_input{
      .design = hir_result.design,
      .hir_arena = *hir_result.hir_arena,
      .type_arena = *hir_result.type_arena,
      .constant_arena = *hir_result.constant_arena,
      .symbol_table = *hir_result.symbol_table,
      .bit_type = bit_type,
  };
  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);

  auto module_info =
      mir::interp::FindInitialModule(mir_result.design, *mir_result.mir_arena);
  if (!module_info) {
    fmt::print(
        stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
        fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
        fmt::styled("no initial process found", fmt::emphasis::bold));
    return 1;
  }

  auto design_state = mir::interp::CreateDesignState(
      *mir_result.mir_arena, *hir_result.type_arena, *module_info->module);

  mir::interp::Interpreter interp(
      mir_result.mir_arena.get(), hir_result.type_arena.get());
  interp.SetOutput(&std::cout);

  // Run all initial processes in order (synthetic init first, then
  // user-defined)
  try {
    for (mir::ProcessId proc_id : module_info->initial_processes) {
      auto state = mir::interp::CreateProcessState(
          *mir_result.mir_arena, *hir_result.type_arena, proc_id,
          &design_state);
      interp.Run(state);
    }
  } catch (const std::exception& e) {
    fmt::print(
        stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
        fmt::styled("runtime error", fmt::fg(fmt::terminal_color::bright_red)),
        fmt::styled(e.what(), fmt::emphasis::bold));
    return 1;
  }

  return 0;
}

}  // namespace lyra::driver
