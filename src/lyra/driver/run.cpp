#include "run.hpp"

#include <iostream>

#include <fmt/color.h>
#include <fmt/core.h>

#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/routine.hpp"

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

auto FindInitialProcess(const mir::Design& design, const mir::Arena& arena)
    -> std::optional<mir::ProcessId> {
  for (const auto& element : design.elements) {
    if (const auto* module = std::get_if<mir::Module>(&element)) {
      for (mir::ProcessId process_id : module->processes) {
        const auto& process = arena[process_id];
        if (process.kind == mir::ProcessKind::kOnce) {
          return process_id;
        }
      }
    }
  }
  return std::nullopt;
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

  lowering::hir_to_mir::LoweringInput mir_input{
      .design = hir_result.design,
      .hir_arena = *hir_result.hir_arena,
      .type_arena = *hir_result.type_arena,
      .constant_arena = *hir_result.constant_arena,
      .symbol_table = *hir_result.symbol_table,
  };
  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);

  auto process_id =
      FindInitialProcess(mir_result.design, *mir_result.mir_arena);
  if (!process_id) {
    fmt::print(
        stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
        fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
        fmt::styled("no initial process found", fmt::emphasis::bold));
    return 1;
  }

  auto state =
      mir::interp::CreateProcessState(*mir_result.mir_arena, *process_id);

  mir::interp::Interpreter interp(
      mir_result.mir_arena.get(), hir_result.type_arena.get());
  interp.SetOutput(&std::cout);

  try {
    interp.Run(state);
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
