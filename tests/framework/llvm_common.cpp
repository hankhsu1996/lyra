#include "tests/framework/llvm_common.hpp"

#include <expected>
#include <filesystem>
#include <format>
#include <memory>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {
namespace {

// Test framework hooks for variable inspection and timing.
class TestSimulationHooks : public lowering::mir_to_llvm::SimulationHooks {
 public:
  TestSimulationHooks(
      std::vector<lowering::mir_to_llvm::VariableInfo> variables,
      bool emit_time_report)
      : variables_(std::move(variables)), emit_time_report_(emit_time_report) {
  }

  void OnAfterRunSimulation(
      lowering::mir_to_llvm::Context& context,
      const std::vector<lowering::mir_to_llvm::SlotInfo>& slots,
      llvm::Value* design_state) override {
    lowering::mir_to_llvm::EmitVariableInspection(
        context, variables_, slots, design_state);
    if (emit_time_report_) {
      lowering::mir_to_llvm::EmitTimeReport(context);
    }
  }

 private:
  std::vector<lowering::mir_to_llvm::VariableInfo> variables_;
  bool emit_time_report_;
};

// Build tracked variables for inspection (maps variable names to slot IDs)
// Note: slot_id is the index into the module's variables list, which
// corresponds to the design slot ID (after any package variables).
auto BuildTrackedVariables(
    const hir::Module* hir_module,
    const lowering::hir_to_mir::LoweringInput& mir_input,
    const lowering::ast_to_hir::LoweringResult& hir_result, size_t base_slot_id)
    -> std::vector<lowering::mir_to_llvm::VariableInfo> {
  std::vector<lowering::mir_to_llvm::VariableInfo> variables;
  if (hir_module == nullptr) {
    return variables;
  }

  for (size_t i = 0; i < hir_module->variables.size(); ++i) {
    const auto& sym = (*mir_input.symbol_table)[hir_module->variables[i]];
    const Type& type = (*hir_result.type_arena)[sym.type];

    // Handle real types
    if (type.Kind() == TypeKind::kReal) {
      variables.push_back({.name = sym.name, .slot_id = base_slot_id + i});
      continue;
    }

    // Handle string types (no variable inspection yet)
    if (type.Kind() == TypeKind::kString) {
      continue;
    }

    // Handle packed integral types
    if (!IsPacked(type)) {
      continue;
    }

    variables.push_back({.name = sym.name, .slot_id = base_slot_id + i});
  }
  return variables;
}

// Holder for hooks - must outlive LLVM lowering
struct HooksHolder {
  std::vector<lowering::mir_to_llvm::VariableInfo> tracked_variables;
  std::unique_ptr<TestSimulationHooks> hooks;
};

// Thread-local storage for hooks (must outlive LLVM module execution)
thread_local std::unique_ptr<HooksHolder> g_hooks_holder;

}  // namespace

auto PrepareLlvmModule(
    const TestCase& test_case, const std::filesystem::path& work_directory)
    -> std::expected<LlvmPreparationResult, std::string> {
  // Parse test case using slang
  auto parse_result = ParseTestCase(test_case, work_directory);
  if (!parse_result.Success()) {
    return std::unexpected(parse_result.error_message);
  }

  // Lower AST to HIR
  DiagnosticSink sink;
  auto hir_result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result.compilation, sink);

  if (sink.HasErrors()) {
    std::ostringstream error_stream;
    for (const auto& diagnostic : sink.GetDiagnostics()) {
      if (diagnostic.primary.kind == DiagKind::kError ||
          diagnostic.primary.kind == DiagKind::kUnsupported ||
          diagnostic.primary.kind == DiagKind::kHostError) {
        error_stream << diagnostic.primary.message << "\n";
      }
    }
    return std::unexpected("HIR lowering errors:\n" + error_stream.str());
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
      .instance_table = &hir_result.instance_table,
  };
  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);
  if (!mir_result) {
    return std::unexpected(
        std::format(
            "MIR lowering error: {}", mir_result.error().primary.message));
  }

  // Find the top module (first module in elaboration order) and calculate base
  // slot ID. Slot ordering: packages first, then all modules' variables in
  // element order.
  const hir::Module* top_module = nullptr;
  for (const auto& element : mir_input.design->elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      top_module = mod;
      break;  // First module is the top module
    }
  }

  // Count package slots before module variables (packages come first)
  size_t base_slot_id = 0;
  for (const auto& element : mir_input.design->elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      base_slot_id += pkg->variables.size();
    }
  }

  // Build tracked variables and create hooks
  auto tracked_variables =
      BuildTrackedVariables(top_module, mir_input, hir_result, base_slot_id);

  // Store hooks in thread-local storage (must outlive LLVM execution)
  g_hooks_holder = std::make_unique<HooksHolder>();
  g_hooks_holder->tracked_variables = std::move(tracked_variables);
  g_hooks_holder->hooks = std::make_unique<TestSimulationHooks>(
      g_hooks_holder->tracked_variables, true);

  // Create diagnostic context for LLVM backend error reporting
  auto origin_lookup = std::make_unique<lowering::OriginMapLookup>(
      &mir_result->origin_map, hir_result.hir_arena.get());
  auto diag_ctx = std::make_unique<lowering::DiagnosticContext>(*origin_lookup);

  // Lower MIR to LLVM IR
  // Use work_directory for file I/O tests, otherwise fall back to CWD
  auto fs_base_dir =
      work_directory.empty()
          ? std::filesystem::absolute(std::filesystem::current_path()).string()
          : std::filesystem::absolute(work_directory).string();
  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &mir_result->design,
      .mir_arena = mir_result->mir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .diag_ctx = diag_ctx.get(),
      .hooks = g_hooks_holder->hooks.get(),
      .fs_base_dir = fs_base_dir,
      .plusargs = test_case.plusargs,
      .enable_trace = test_case.trace,
      .debug_dump_slot_meta = test_case.dump_slot_meta,
  };

  auto llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  if (!llvm_result) {
    const auto& diag = llvm_result.error();
    std::string location;
    std::visit(
        [&](const auto& span) {
          using T = std::decay_t<decltype(span)>;
          if constexpr (std::is_same_v<T, SourceSpan>) {
            location = FormatSourceLocation(span, *hir_result.source_manager);
          }
        },
        diag.primary.span);
    if (!location.empty()) {
      return std::unexpected(
          std::format("{}: error: {}", location, diag.primary.message));
    }
    return std::unexpected(
        std::format("LLVM lowering error: {}", diag.primary.message));
  }

  return LlvmPreparationResult{
      .hir_result = std::move(hir_result),
      .mir_result = std::move(*mir_result),
      .origin_lookup = std::move(origin_lookup),
      .diag_ctx = std::move(diag_ctx),
      .llvm_result = std::move(*llvm_result),
  };
}

}  // namespace lyra::test
