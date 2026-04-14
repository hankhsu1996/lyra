#include "tests/framework/llvm_common.hpp"

#include <chrono>
#include <cstdlib>
#include <expected>
#include <filesystem>
#include <format>
#include <memory>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/Value.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/link_request.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/llvm_backend/toolchain.hpp"
#include "lyra/lowering/ast_to_hir/generate_repertoire.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/repertoire_descriptor.hpp"
#include "lyra/lowering/ast_to_hir/repertoire_descriptor_debug.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/mir/dumper.hpp"
#include "lyra/runtime/artifact_names.hpp"
#include "lyra/runtime/feature_flags.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {
namespace {

// Test framework hooks for variable inspection and timing.
// Variable inspection is backend-owned: the hook provides identity
// (name + slot_id) via GetTrackedVariables(), and the backend builds
// typed placements from session data. This hook only emits non-inspection
// post-simulation reports (e.g., time report).
class TestSimulationHooks : public lowering::mir_to_llvm::SimulationHooks {
 public:
  TestSimulationHooks(
      std::vector<lowering::mir_to_llvm::InspectedVarRef> variables,
      bool emit_time_report)
      : variables_(std::move(variables)), emit_time_report_(emit_time_report) {
  }

  [[nodiscard]] auto GetTrackedVariables() const
      -> std::span<const lowering::mir_to_llvm::InspectedVarRef> override {
    return variables_;
  }

  void EmitPostSimulationReports(
      lowering::mir_to_llvm::Context& context, llvm::Value* /*design_state*/,
      llvm::Value* /*abi_ptr*/, llvm::Value* run_session_ptr) override {
    if (emit_time_report_) {
      lowering::mir_to_llvm::EmitTimeReport(context, run_session_ptr);
    }
  }

 private:
  std::vector<lowering::mir_to_llvm::InspectedVarRef> variables_;
  bool emit_time_report_;
};

// Build tracked variables for inspection (maps variable names to slot IDs).
// Returns identity-only records; the backend builds typed placements from
// session data during codegen.
auto BuildTrackedVariables(
    const hir::Module* hir_module,
    const lowering::hir_to_mir::LoweringInput& mir_input,
    const lowering::ast_to_hir::LoweringResult& hir_result, size_t base_slot_id)
    -> std::vector<lowering::mir_to_llvm::InspectedVarRef> {
  std::vector<lowering::mir_to_llvm::InspectedVarRef> variables;
  if (hir_module == nullptr) {
    return variables;
  }

  uint32_t slot_index = 0;
  for (size_t i = 0; i < hir_module->variables.size(); ++i) {
    const auto& sym = (*mir_input.symbol_table)[hir_module->variables[i]];
    const Type& type = (*hir_result.type_arena)[sym.type];

    // Non-value-storage types (e.g., event) are not allocated slots.
    if (!HasValueStorage(type.Kind())) {
      continue;
    }

    // Handle real types
    if (type.Kind() == TypeKind::kReal) {
      variables.push_back(
          {.name = sym.name,
           .slot_id = common::SlotId{
               static_cast<uint32_t>(base_slot_id + slot_index)}});
      ++slot_index;
      continue;
    }

    // Handle string types (no variable inspection yet)
    if (type.Kind() == TypeKind::kString) {
      ++slot_index;
      continue;
    }

    // Handle packed integral types
    if (!IsPacked(type)) {
      ++slot_index;
      continue;
    }

    variables.push_back(
        {.name = sym.name,
         .slot_id =
             common::SlotId{static_cast<uint32_t>(base_slot_id + slot_index)}});
    ++slot_index;
  }
  return variables;
}

// Holder for hooks - must outlive LLVM lowering
struct HooksHolder {
  std::vector<lowering::mir_to_llvm::InspectedVarRef> tracked_variables;
  std::unique_ptr<TestSimulationHooks> hooks;
};

// Thread-local storage for hooks (must outlive LLVM module execution)
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
thread_local std::unique_ptr<HooksHolder> g_hooks_holder;

}  // namespace

auto FindRuntimeLibrary(std::string_view lib_name)
    -> std::optional<std::filesystem::path> {
  // Prefer Bazel runfiles env vars (works in RBE and local)
  const char* test_srcdir = std::getenv("TEST_SRCDIR");
  const char* test_workspace = std::getenv("TEST_WORKSPACE");
  if (test_srcdir != nullptr && test_workspace != nullptr) {
    auto runfiles_path =
        std::filesystem::path(test_srcdir) / test_workspace / lib_name;
    if (std::filesystem::exists(runfiles_path)) {
      return runfiles_path;
    }
  }

  // Fallback: resolve from executable path
  std::filesystem::path exe_path;
  try {
    exe_path = std::filesystem::read_symlink("/proc/self/exe");
  } catch (const std::filesystem::filesystem_error&) {
    return std::nullopt;
  }

  auto runfiles_path = std::filesystem::path(exe_path.string() + ".runfiles") /
                       "_main" / lib_name;
  if (std::filesystem::exists(runfiles_path)) {
    return runfiles_path;
  }

  auto sibling_path = exe_path.parent_path() / lib_name;
  if (std::filesystem::exists(sibling_path)) {
    return sibling_path;
  }

  return std::nullopt;
}

auto PrepareLlvmModule(
    const TestCase& test_case, const std::filesystem::path& work_directory,
    bool force_two_state, lowering::mir_to_llvm::MainAbi main_abi)
    -> std::expected<LlvmPreparationResult, std::string> {
  using Clock = std::chrono::steady_clock;

  // Parse test case using slang
  auto t_parse = Clock::now();
  auto parse_result = ParseTestCase(test_case, work_directory);
  if (!parse_result.Success()) {
    return std::unexpected(parse_result.error_message);
  }
  double parse_seconds =
      std::chrono::duration<double>(Clock::now() - t_parse).count();

  // Lower AST to HIR
  auto t_hir = Clock::now();
  DiagnosticSink sink;
  lowering::ast_to_hir::HirLoweringOptions hir_options{
      .disable_assertions = test_case.disable_assertions,
  };
  auto hir_result = lowering::ast_to_hir::LowerAstToHir(
      *parse_result.compilation, sink, hir_options);

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

  double hir_lower_seconds =
      std::chrono::duration<double>(Clock::now() - t_hir).count();

  // Build specialization map dump (routed to
  // SimulationArtifacts::compiler_output)
  std::string compiler_output;
  if (test_case.dump_specialization_map) {
    const auto& spec_map = hir_result.specialization_map;
    compiler_output += std::format("spec_groups: {}\n", spec_map.groups.size());
    for (size_t i = 0; i < spec_map.groups.size(); ++i) {
      const auto& group = spec_map.groups[i];
      compiler_output += std::format(
          "spec[{}]: def={} fp=0x{:x} instances={}\n", i,
          group.spec_id.def_id.value, group.spec_id.fingerprint.value,
          group.instance_indices.size());
    }
  }

  // Build artifact inventory dump (routed to
  // SimulationArtifacts::compiler_output)
  if (test_case.dump_repertoire) {
    const auto& root = parse_result.compilation->getRoot();
    for (const auto* inst : root.topInstances) {
      compiler_output +=
          std::format("--- {} ({}) ---\n", inst->name, inst->body.name);
      auto inventory = lowering::ast_to_hir::BuildArtifactInventory(inst->body);
      compiler_output += lowering::ast_to_hir::DumpArtifactInventory(inventory);
    }
  }

  // Build repertoire descriptor dump (routed to
  // SimulationArtifacts::compiler_output)
  if (test_case.dump_repertoire_desc) {
    const auto& root = parse_result.compilation->getRoot();
    for (const auto* inst : root.topInstances) {
      compiler_output +=
          std::format("--- {} ({}) ---\n", inst->name, inst->body.name);
      auto [desc, debug_view] =
          lowering::ast_to_hir::BuildDefinitionRepertoireDescWithDebugView(
              inst->body);
      compiler_output +=
          lowering::ast_to_hir::DumpDefinitionRepertoireDesc(desc, debug_view);
    }
  }

  // Lower HIR to MIR
  auto t_mir = Clock::now();
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
      .child_coord_map = &hir_result.child_coord_map,
      .body_timescales = &hir_result.body_timescales,
  };
  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);
  if (!mir_result) {
    return std::unexpected(
        std::format(
            "MIR lowering error: {}", mir_result.error().primary.message));
  }

  if (test_case.dump_dpi_header && !mir_result->dpi_header.empty()) {
    compiler_output += mir_result->dpi_header;
  }

  if (test_case.dump_mir) {
    std::ostringstream mir_out;
    mir::Dumper dumper(
        mir_result->design_arena.get(), hir_result.type_arena.get(), &mir_out);
    dumper.Dump(mir_result->design);
    compiler_output += mir_out.str();
  }

  double mir_lower_seconds =
      std::chrono::duration<double>(Clock::now() - t_mir).count();

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
      &mir_result->design_origins, hir_result.hir_arena.get());
  auto diag_ctx = std::make_unique<lowering::DiagnosticContext>(*origin_lookup);

  // Lower MIR to LLVM IR
  auto t_llvm = Clock::now();
  // Use work_directory for file I/O tests, otherwise fall back to CWD
  auto fs_base_dir =
      work_directory.empty()
          ? std::filesystem::absolute(std::filesystem::current_path()).string()
          : std::filesystem::absolute(work_directory).string();
  uint32_t feature_flags = 0;
  if (test_case.trace_summary) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableTrace);
    feature_flags |=
        runtime::ToUint32(runtime::FeatureFlag::kEnableTraceSummary);
  }
  if (test_case.signal_trace) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableTrace);
    feature_flags |=
        runtime::ToUint32(runtime::FeatureFlag::kEnableSignalTrace);
  }
  if (test_case.dump_slot_meta) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kDumpSlotMeta);
  }

  auto origin_provenance = lowering::BuildBodyOriginProvenance(
      mir_result->body_origins, hir_result.design,
      mir_result->design.module_bodies);

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &mir_result->design,
      .construction = &mir_result->construction,
      .mir_arena = mir_result->design_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .diag_ctx = diag_ctx.get(),
      .source_manager = hir_result.source_manager.get(),
      .origin_provenance = &origin_provenance,
      .hooks = g_hooks_holder->hooks.get(),
      .fs_base_dir = fs_base_dir,
      .plusargs = test_case.plusargs,
      .feature_flags = feature_flags,
      .signal_trace_path = {},
      .iteration_limit = 0,
      .force_two_state = force_two_state,
      .collect_forwarding_analysis = false,
      .main_abi = main_abi,
      .dpi_export_wrappers = &mir_result->dpi_export_wrappers,
      .bound_connections = &mir_result->bound_connections,
      .expr_connections = &mir_result->expr_connections,
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

  if (test_case.dump_llvm_ir) {
    compiler_output += lowering::mir_to_llvm::DumpLlvmIr(*llvm_result->module);
  }

  double llvm_lower_seconds =
      std::chrono::duration<double>(Clock::now() - t_llvm).count();

  return LlvmPreparationResult{
      .hir_result = std::move(hir_result),
      .mir_result = std::move(*mir_result),
      .origin_lookup = std::move(origin_lookup),
      .diag_ctx = std::move(diag_ctx),
      .llvm_result = std::move(*llvm_result),
      .compiler_output = std::move(compiler_output),
      .parse_seconds = parse_seconds,
      .hir_lower_seconds = hir_lower_seconds,
      .mir_lower_seconds = mir_lower_seconds,
      .llvm_lower_seconds = llvm_lower_seconds,
  };
}

auto LinkTestExecutable(
    const std::filesystem::path& object_path,
    const std::filesystem::path& output_dir, const std::string& name,
    std::span<const std::filesystem::path> external_link_inputs)
    -> std::expected<TestLinkResult, std::string> {
  auto runtime_path = FindRuntimeLibrary(runtime::kSharedLibName);
  if (!runtime_path) {
    return std::unexpected("shared runtime library not found");
  }

  auto toolchain = lowering::mir_to_llvm::DetectToolchain();
  if (!toolchain) {
    return std::unexpected(
        std::format("toolchain detection failed: {}", toolchain.error()));
  }

  auto runtime_dir = runtime_path->parent_path();

  // Uses the shared runtime library for test speed (no rpath -- the test
  // harness sets LD_LIBRARY_PATH when executing the binary).
  lowering::mir_to_llvm::LinkRequest request{
      .output_path = output_dir / name,
      .object_inputs = {object_path},
      .runtime_link_inputs =
          {
              lowering::mir_to_llvm::RuntimeSearchLinkInput{
                  .search_dir = runtime_dir,
                  .library_name = std::string(runtime::kSharedLibName),
              },
          },
      .external_link_inputs =
          {external_link_inputs.begin(), external_link_inputs.end()},
      .system_libs = {"-lstdc++", "-lm", "-lpthread"},
  };

  auto link_result = lowering::mir_to_llvm::LinkExecutable(*toolchain, request);
  if (!link_result) {
    const auto& err = link_result.error();
    std::string msg = std::format("linker failed: {}", err.message);
    if (!err.stderr.empty()) {
      msg += std::format("\n{}", err.stderr);
    }
    return std::unexpected(msg);
  }

  return TestLinkResult{
      .exe_path = *link_result,
      .runtime_dir = runtime_dir,
  };
}

}  // namespace lyra::test
