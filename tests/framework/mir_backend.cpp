#include "tests/framework/mir_backend.hpp"

#include <cstddef>
#include <cstdint>
#include <exception>
#include <expected>
#include <filesystem>
#include <format>
#include <iterator>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/runtime/engine.hpp"
#include "tests/framework/llvm_backend.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {
namespace {

// Convert multi-word integral to lowercase hex string (no prefix)
auto IntegralToHex(const mir::interp::RuntimeIntegral& integral)
    -> std::string {
  std::string result;
  // Process words from most significant to least significant
  bool leading = true;
  for (auto it = integral.value.rbegin(); it != integral.value.rend(); ++it) {
    if (leading && *it == 0 && std::next(it) != integral.value.rend()) {
      // Skip leading zero words (but keep at least one word)
      continue;
    }
    if (leading) {
      // First non-zero word: no padding
      result += std::format("{:x}", *it);
      leading = false;
    } else {
      // Subsequent words: pad to 16 hex digits
      result += std::format("{:016x}", *it);
    }
  }
  return result.empty() ? "0" : result;
}

// Extract numeric value from RuntimeValue for assertion comparison.
// Returns int64_t for integers <= 64 bits, HexValue for integers > 64 bits,
// or double for reals.
//
// Note: RuntimeIntegral doesn't carry signedness info. The raw value is
// returned without sign-extension. The caller (assertion code) handles
// sign-extension when comparing against negative expected values.
auto ExtractNumericValue(const mir::interp::RuntimeValue& value)
    -> std::expected<ExtractedValue, std::string> {
  if (mir::interp::IsIntegral(value)) {
    const auto& integral = mir::interp::AsIntegral(value);
    if (integral.IsX() || integral.IsZ()) {
      return std::unexpected("X/Z values not supported in assertions");
    }
    if (integral.value.empty()) {
      return std::unexpected("Empty integral value");
    }
    // Wide values (>64 bits): return as hex string
    if (integral.bit_width > 64) {
      return HexValue{IntegralToHex(integral)};
    }
    // Return raw value masked to bit_width. No sign-extension here.
    uint64_t raw = integral.value[0];
    if (integral.bit_width < 64) {
      raw &= (uint64_t{1} << integral.bit_width) - 1;
    }
    return static_cast<int64_t>(raw);
  }
  if (mir::interp::IsReal(value)) {
    return mir::interp::AsReal(value).value;
  }
  return std::unexpected("Unsupported value type for assertion");
}

}  // namespace

auto RunMirInterpreter(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> TestResult {
  TestResult result;

  // Parse test case using slang
  auto parse_result = ParseTestCase(test_case, work_directory);
  if (!parse_result.Success()) {
    result.error_message = parse_result.error_message;
    return result;
  }

  // Lower AST to HIR
  DiagnosticSink sink;
  auto hir_result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result.compilation, sink);

  if (sink.HasErrors()) {
    std::ostringstream error_stream;
    for (const auto& diagnostic : sink.GetDiagnostics()) {
      if (diagnostic.severity == DiagnosticSeverity::kError) {
        error_stream << diagnostic.message << "\n";
      }
    }
    result.error_message = "HIR lowering errors:\n" + error_stream.str();
    return result;
  }

  // Lower HIR to MIR
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

  // Find the top HIR module (the one with processes) for variable assertions
  const hir::Module* hir_module = nullptr;
  for (const auto& element : mir_input.design->elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      if (!mod->processes.empty()) {
        hir_module = mod;
      }
    }
  }

  // Build variable name to slot index mapping (for variable assertions)
  // Slot layout: [package vars...] [child module vars...] [top module vars...]
  std::unordered_map<std::string, size_t> var_slots;
  if (!test_case.expected_values.empty()) {
    if (hir_module == nullptr) {
      result.error_message = "Variable assertions require a module";
      return result;
    }
    size_t var_offset = 0;
    for (const auto& element : mir_input.design->elements) {
      if (const auto* pkg = std::get_if<hir::Package>(&element)) {
        var_offset += pkg->variables.size();
      }
      if (const auto* mod = std::get_if<hir::Module>(&element)) {
        if (mod == hir_module) {
          break;
        }
        var_offset += mod->variables.size();
      }
    }
    for (size_t i = 0; i < hir_module->variables.size(); ++i) {
      const auto& sym = (*mir_input.symbol_table)[hir_module->variables[i]];
      if (var_slots.contains(sym.name)) {
        result.error_message =
            std::format("Duplicate variable name '{}' in module", sym.name);
        return result;
      }
      var_slots[sym.name] = var_offset + i;
    }
  }

  // Find initial module
  auto module_info =
      mir::interp::FindInitialModule(mir_result.design, *mir_result.mir_arena);
  if (!module_info) {
    result.error_message = "No initial process found (no kOnce process in MIR)";
    return result;
  }

  // Create design storage
  auto design_state = mir::interp::CreateDesignState(
      *mir_result.mir_arena, *hir_result.type_arena, mir_result.design);

  // Initialize design slots from HIR variable types.
  // CreateDesignState only initializes slots that are referenced in code; this
  // ensures all declared variables are properly initialized (2-state -> 0,
  // 4-state -> X) even if never used.
  //
  // INVARIANT: MIR lowering assigns slot IDs sequentially:
  //   [0, N_pkg) = package variables, [N_pkg, N_pkg+N_mod) = module variables
  // (see hir_to_mir/design.cpp and hir_to_mir/module.cpp).
  std::vector<SymbolId> all_design_vars;
  for (const auto& element : mir_input.design->elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      for (SymbolId var : pkg->variables) {
        all_design_vars.push_back(var);
      }
    }
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      for (SymbolId var : mod->variables) {
        all_design_vars.push_back(var);
      }
    }
  }
  if (all_design_vars.size() != design_state.storage.size()) {
    result.error_message = std::format(
        "Slot count mismatch: HIR has {} variables, MIR has {} slots",
        all_design_vars.size(), design_state.storage.size());
    return result;
  }
  for (size_t i = 0; i < all_design_vars.size(); ++i) {
    const auto& sym = (*mir_input.symbol_table)[all_design_vars[i]];
    design_state.storage[i] =
        mir::interp::CreateDefaultValue(*hir_result.type_arena, sym.type);
  }

  // Run interpreter with output capture
  std::ostringstream output_stream;
  mir::interp::Interpreter interpreter(
      mir_result.mir_arena.get(), hir_result.type_arena.get());
  interpreter.SetOutput(&output_stream);
  interpreter.SetPlusargs(test_case.plusargs);

  // Change to work_directory if we have one (for file I/O tests)
  std::optional<ScopedCurrentPath> scoped_path;
  if (!work_directory.empty()) {
    scoped_path.emplace(work_directory);
  }

  // Run design init processes (package variable initialization)
  for (mir::ProcessId proc_id : mir_result.design.init_processes) {
    auto state = mir::interp::CreateProcessState(
        *mir_result.mir_arena, *hir_result.type_arena, proc_id, &design_state);
    interpreter.Run(state);
  }

  // Create process states for all initial processes
  std::unordered_map<uint32_t, mir::interp::ProcessState> process_states;
  for (mir::ProcessId proc_id : module_info->initial_processes) {
    auto state = mir::interp::CreateProcessState(
        *mir_result.mir_arena, *hir_result.type_arena, proc_id, &design_state);
    process_states.emplace(proc_id.value, std::move(state));
  }

  // Run with Engine-based scheduler for proper delay handling
  try {
    runtime::Engine engine([&](runtime::Engine& eng,
                               runtime::ProcessHandle handle,
                               runtime::ResumePoint resume) {
      auto it = process_states.find(handle.process_id);
      if (it == process_states.end()) {
        return;
      }
      auto& state = it->second;

      state.current_block = mir::BasicBlockId{resume.block_index};
      state.instruction_index = resume.instruction_index;
      state.status = mir::interp::ProcessStatus::kRunning;

      auto reason = interpreter.RunUntilSuspend(state);

      std::visit(
          Overloaded{
              [](const mir::interp::SuspendFinished&) {},
              [&](const mir::interp::SuspendDelay& d) {
                eng.Delay(
                    handle,
                    runtime::ResumePoint{
                        .block_index = d.resume_block.value,
                        .instruction_index = 0},
                    d.ticks);
              },
              [&](const mir::interp::SuspendWait& w) {
                eng.Delay(
                    handle,
                    runtime::ResumePoint{
                        .block_index = w.resume_block.value,
                        .instruction_index = 0},
                    1);
              },
              [&](const mir::interp::SuspendRepeat& r) {
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

    // Schedule initial processes
    for (mir::ProcessId proc_id : module_info->initial_processes) {
      engine.ScheduleInitial(
          runtime::ProcessHandle{
              .process_id = proc_id.value, .instance_id = 0});
    }

    // Run simulation
    engine.Run();
  } catch (const std::exception& e) {
    result.error_message = std::string("Runtime error: ") + e.what();
    return result;
  }

  result.success = true;
  result.captured_output = output_stream.str();

  // Extract final variable values for assertions
  for (const auto& [name, expected] : test_case.expected_values) {
    auto it = var_slots.find(name);
    if (it == var_slots.end()) {
      result.error_message = std::format("Variable '{}' not found", name);
      result.success = false;
      return result;
    }
    const auto& value = design_state.Get(static_cast<int>(it->second));
    auto extracted = ExtractNumericValue(value);
    if (!extracted) {
      result.error_message =
          std::format("Variable '{}': {}", name, extracted.error());
      result.success = false;
      return result;
    }
    result.variables[name] = *extracted;
  }

  return result;
}

}  // namespace lyra::test
