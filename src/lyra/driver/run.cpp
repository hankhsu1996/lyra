#include "run.hpp"

#include <iostream>
#include <unordered_map>
#include <variant>

#include <fmt/color.h>
#include <fmt/core.h>

#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "lyra/runtime/engine.hpp"

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

auto RunMir(const std::vector<std::string>& files) -> int {
  auto parse_result = LoadFiles(files);
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
  TypeId offset_type = hir_result.type_arena->Intern(
      TypeKind::kIntegral,
      IntegralInfo{
          .bit_width = 32, .is_signed = false, .is_four_state = false});
  TypeId string_type =
      hir_result.type_arena->Intern(TypeKind::kString, std::monostate{});
  lowering::hir_to_mir::LoweringInput mir_input{
      .design = &hir_result.design,
      .hir_arena = hir_result.hir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .constant_arena = hir_result.constant_arena.get(),
      .symbol_table = hir_result.symbol_table.get(),
      .bit_type = bit_type,
      .offset_type = offset_type,
      .string_type = string_type,
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

  // Centralized storage for process states (keyed by process_id)
  std::unordered_map<uint32_t, mir::interp::ProcessState> process_states;

  // Create process states for all initial processes
  for (mir::ProcessId proc_id : module_info->initial_processes) {
    auto state = mir::interp::CreateProcessState(
        *mir_result.mir_arena, *hir_result.type_arena, proc_id, &design_state);
    process_states.emplace(proc_id.value, std::move(state));
  }

  // Create engine with process runner callback
  runtime::Engine engine([&](runtime::Engine& eng,
                             runtime::ProcessHandle handle,
                             runtime::ResumePoint resume) {
    // Look up process state
    auto it = process_states.find(handle.process_id);
    if (it == process_states.end()) {
      return;
    }
    auto& state = it->second;

    // Set resume point
    state.current_block = mir::BasicBlockId{resume.block_index};
    state.instruction_index = resume.instruction_index;
    state.status = mir::interp::ProcessStatus::kRunning;

    // Run until suspend
    auto reason = interp.RunUntilSuspend(state);

    // Handle suspension
    std::visit(
        Overloaded{
            [](const mir::interp::SuspendFinished&) {
              // Done, no rescheduling
            },
            [&](const mir::interp::SuspendDelay& d) {
              eng.Delay(
                  handle,
                  runtime::ResumePoint{
                      .block_index = d.resume_block.value,
                      .instruction_index = 0},
                  d.ticks);
            },
            [&](const mir::interp::SuspendWait& w) {
              // TODO(hankhsu): Subscribe to signal triggers
              // For now, just delay by 1 tick as placeholder
              eng.Delay(
                  handle,
                  runtime::ResumePoint{
                      .block_index = w.resume_block.value,
                      .instruction_index = 0},
                  1);
            },
            [&](const mir::interp::SuspendRepeat& r) {
              // Reschedule to next time slot
              eng.Delay(
                  handle,
                  runtime::ResumePoint{
                      .block_index = r.resume_block.value,
                      .instruction_index = 0},
                  1);
            },
        },
        reason);
  });

  // Schedule all initial processes
  for (mir::ProcessId proc_id : module_info->initial_processes) {
    engine.ScheduleInitial(
        runtime::ProcessHandle{.process_id = proc_id.value, .instance_id = 0});
  }

  // Run simulation
  try {
    engine.Run();
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
