#include "tests/framework/llvm_common.hpp"

#include <array>
#include <cerrno>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <memory>
#include <poll.h>
#include <spawn.h>
#include <sstream>
#include <string>
#include <sys/wait.h>
#include <type_traits>
#include <unistd.h>
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
#include "lyra/runtime/feature_flags.hpp"
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

auto FindRuntimeLibrary() -> std::optional<std::filesystem::path> {
  constexpr std::string_view kLibName = "liblyra_runtime.so";

  // Prefer Bazel runfiles env vars (works in RBE and local)
  const char* test_srcdir = std::getenv("TEST_SRCDIR");
  const char* test_workspace = std::getenv("TEST_WORKSPACE");
  if (test_srcdir != nullptr && test_workspace != nullptr) {
    auto runfiles_path =
        std::filesystem::path(test_srcdir) / test_workspace / kLibName;
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
                       "_main" / kLibName;
  if (std::filesystem::exists(runfiles_path)) {
    return runfiles_path;
  }

  auto sibling_path = exe_path.parent_path() / kLibName;
  if (std::filesystem::exists(sibling_path)) {
    return sibling_path;
  }

  return std::nullopt;
}

auto RunSubprocess(
    const std::filesystem::path& exe, std::span<const std::string> args)
    -> SubprocessResult {
  // Create pipes for stdout and stderr
  std::array<int, 2> stdout_pipe{};
  std::array<int, 2> stderr_pipe{};
  if (pipe(stdout_pipe.data()) != 0) {
    return {.exit_code = -1, .stderr_text = "failed to create stdout pipe"};
  }
  if (pipe(stderr_pipe.data()) != 0) {
    close(stdout_pipe[0]);
    close(stdout_pipe[1]);
    return {.exit_code = -1, .stderr_text = "failed to create stderr pipe"};
  }

  posix_spawn_file_actions_t actions{};
  posix_spawn_file_actions_init(&actions);
  posix_spawn_file_actions_adddup2(&actions, stdout_pipe[1], STDOUT_FILENO);
  posix_spawn_file_actions_adddup2(&actions, stderr_pipe[1], STDERR_FILENO);
  posix_spawn_file_actions_addclose(&actions, stdout_pipe[0]);
  posix_spawn_file_actions_addclose(&actions, stdout_pipe[1]);
  posix_spawn_file_actions_addclose(&actions, stderr_pipe[0]);
  posix_spawn_file_actions_addclose(&actions, stderr_pipe[1]);

  // Build argv: [exe, args..., nullptr]
  std::string exe_str = exe.string();
  std::vector<char*> argv;
  argv.reserve(args.size() + 2);
  argv.push_back(exe_str.data());
  // Store string copies so pointers remain valid
  std::vector<std::string> arg_copies(args.begin(), args.end());
  for (auto& arg : arg_copies) {
    argv.push_back(arg.data());
  }
  argv.push_back(nullptr);

  pid_t pid = 0;
  // NOLINTNEXTLINE(misc-include-cleaner) - environ is from unistd.h
  int spawn_result = posix_spawnp(
      &pid, exe_str.c_str(), &actions, nullptr, argv.data(), environ);
  posix_spawn_file_actions_destroy(&actions);

  // Close write ends in parent
  close(stdout_pipe[1]);
  close(stderr_pipe[1]);

  if (spawn_result != 0) {
    close(stdout_pipe[0]);
    close(stderr_pipe[0]);
    return {
        .exit_code = -1,
        .stderr_text = std::format(
            "posix_spawnp failed: {}", std::strerror(spawn_result))};
  }

  // Read stdout and stderr concurrently using poll
  std::string stdout_text;
  std::string stderr_text;
  std::array<struct pollfd, 2> fds{};
  fds[0] = {.fd = stdout_pipe[0], .events = POLLIN, .revents = 0};
  fds[1] = {.fd = stderr_pipe[0], .events = POLLIN, .revents = 0};
  int open_fds = 2;

  std::array<char, 4096> buffer{};
  while (open_fds > 0) {
    int ready = poll(fds.data(), fds.size(), -1);
    if (ready < 0) {
      break;
    }
    for (auto& fd : fds) {
      if (fd.fd < 0) {
        continue;
      }
      if ((fd.revents & (POLLIN | POLLHUP)) != 0) {
        ssize_t n = read(fd.fd, buffer.data(), buffer.size());
        if (n > 0) {
          auto& target = (fd.fd == stdout_pipe[0]) ? stdout_text : stderr_text;
          target.append(buffer.data(), static_cast<size_t>(n));
        } else {
          close(fd.fd);
          fd.fd = -1;
          --open_fds;
        }
      }
    }
  }

  int status = 0;
  if (waitpid(pid, &status, 0) == -1) {
    return {.exit_code = -1, .stderr_text = "waitpid failed"};
  }

  int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
  return {
      .exit_code = exit_code,
      .stdout_text = std::move(stdout_text),
      .stderr_text = std::move(stderr_text)};
}

auto PrepareLlvmModule(
    const TestCase& test_case, const std::filesystem::path& work_directory,
    bool force_two_state, bool share_procs)
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
  uint32_t feature_flags = 0;
  if (test_case.trace) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableTrace);
  }
  if (test_case.dump_slot_meta) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kDumpSlotMeta);
  }

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &mir_result->design,
      .mir_arena = mir_result->mir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .diag_ctx = diag_ctx.get(),
      .hooks = g_hooks_holder->hooks.get(),
      .fs_base_dir = fs_base_dir,
      .plusargs = test_case.plusargs,
      .feature_flags = feature_flags,
      .force_two_state = force_two_state,
      .share_procs = share_procs,
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
