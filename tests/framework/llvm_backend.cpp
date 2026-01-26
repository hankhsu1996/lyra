#include "tests/framework/llvm_backend.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <cerrno>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <format>
#include <fstream>
#include <map>
#include <optional>
#include <spawn.h>
#include <sstream>
#include <string>
#include <string_view>
#include <sys/wait.h>
#include <type_traits>
#include <unistd.h>
#include <utility>
#include <variant>
#include <vector>

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

// Find the runtime library for LLVM backend
auto FindRuntimeLibrary() -> std::optional<std::filesystem::path> {
  // Try to find liblyra_runtime.so relative to the test binary
  std::filesystem::path exe_path;
  try {
    exe_path = std::filesystem::read_symlink("/proc/self/exe");
  } catch (const std::filesystem::filesystem_error&) {
    return std::nullopt;
  }

  // Try runfiles path (Bazel test environment)
  auto runfiles_path = std::filesystem::path(exe_path.string() + ".runfiles") /
                       "_main" / "liblyra_runtime.so";
  if (std::filesystem::exists(runfiles_path)) {
    return runfiles_path;
  }

  // Try sibling path
  auto sibling_path = exe_path.parent_path() / "liblyra_runtime.so";
  if (std::filesystem::exists(sibling_path)) {
    return sibling_path;
  }

  return std::nullopt;
}

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

    uint32_t width = PackedBitWidth(type, *hir_result.type_arena);
    bool is_signed = IsPackedSigned(type, *hir_result.type_arena);

    // Add to variables for inspection (only types we can parse back)
    // Skip: width 0, or unsigned 64-bit (would overflow int64_t)
    // >64-bit uses 'h:' hex format, ≤64-bit uses 'i:' decimal format
    if (width > 64 || (width > 0 && width <= 64 && (width < 64 || is_signed))) {
      variables.push_back({.name = sym.name, .slot_id = base_slot_id + i});
    }
  }
  return variables;
}

// Parse a single __LYRA_VAR entry and extract variable value.
void ParseLyraVarEntry(
    std::string_view entry, std::map<std::string, ExtractedValue>& variables) {
  // Format: "i:name=value" or "r:name=value"
  if (entry.size() >= 2 && entry[1] == ':') {
    char type_tag = entry[0];
    auto name_value = entry.substr(2);  // Skip "X:"
    auto eq_pos = name_value.find('=');
    if (eq_pos != std::string::npos) {
      std::string name(name_value.substr(0, eq_pos));
      std::string value_str(name_value.substr(eq_pos + 1));

      if (type_tag == 'i') {  // Integral (signed decimal)
        int64_t value = std::stoll(value_str);
        variables[name] = value;
      } else if (type_tag == 'h') {  // Wide integral (hex)
        std::string hex(value_str);
        // Strip 0x/0X prefix if present
        if (hex.size() >= 2 && hex[0] == '0' &&
            (hex[1] == 'x' || hex[1] == 'X')) {
          hex = hex.substr(2);
        }
        // Normalize to lowercase
        std::ranges::transform(hex, hex.begin(), [](unsigned char c) {
          return static_cast<char>(std::tolower(c));
        });
        variables[name] = HexValue{std::move(hex)};
      } else if (type_tag == 'r') {  // Real
        double value = std::stod(value_str);
        variables[name] = value;
      }
    }
  }
}

struct ParsedOutput {
  std::string clean;
  std::map<std::string, ExtractedValue> variables;
  uint64_t final_time = 0;
};

// Parse __LYRA_VAR and __LYRA_TIME__ output, stripping protocol lines.
// Preserves all other output exactly, including trailing newlines.
// Handles __LYRA_VAR appearing mid-line (after $write with no newline).
auto ParseLyraVarOutput(const std::string& output) -> ParsedOutput {
  ParsedOutput parsed;
  std::string& clean_output = parsed.clean;
  std::map<std::string, ExtractedValue>& variables = parsed.variables;

  // Check if original output ends with newline - we need this to know
  // whether to add a trailing newline to the clean output
  bool output_ends_with_newline = !output.empty() && output.back() == '\n';

  std::istringstream stream(output);
  std::string line;
  bool first_clean_line = true;
  // Track whether user content should have trailing newline.
  // True if the last user content was on its own line (followed by \n),
  // false if user content was followed immediately by __LYRA_VAR (mid-line).
  bool user_ends_with_newline = false;
  constexpr std::string_view kPrefix = "__LYRA_VAR:";
  constexpr std::string_view kTimePrefix = "__LYRA_TIME__=";

  while (std::getline(stream, line)) {
    // Check for __LYRA_TIME__=<N> (always a full line)
    if (line.starts_with(kTimePrefix)) {
      parsed.final_time =
          std::stoull(std::string(line.substr(kTimePrefix.size())));
      continue;
    }

    // Look for __LYRA_VAR: anywhere in the line
    auto var_pos = line.find(kPrefix);

    if (var_pos == std::string::npos) {
      // No __LYRA_VAR: in this line - add whole line to clean output
      if (!first_clean_line) {
        clean_output += '\n';
      }
      clean_output += line;
      first_clean_line = false;
      // Assume this line had user content and ended with \n, but we'll
      // correct this assumption at the end using output_ends_with_newline
      user_ends_with_newline = true;
    } else if (var_pos == 0) {
      // Line starts with __LYRA_VAR: - previous user content ended with \n
      // (Don't change user_ends_with_newline - keep previous value)
      ParseLyraVarEntry(line.substr(kPrefix.size()), variables);
    } else {
      // __LYRA_VAR: appears mid-line (after $write output with no newline)
      if (!first_clean_line) {
        clean_output += '\n';
      }
      clean_output += line.substr(0, var_pos);
      first_clean_line = false;
      // User content was NOT followed by newline (it was followed by
      // __LYRA_VAR)
      user_ends_with_newline = false;
      ParseLyraVarEntry(line.substr(var_pos + kPrefix.size()), variables);
    }
  }

  // Add trailing newline only if:
  // 1. User content should have one (user_ends_with_newline)
  // 2. There was some clean output (!first_clean_line)
  // 3. The original output actually ended with a newline
  if (user_ends_with_newline && !first_clean_line && output_ends_with_newline) {
    clean_output += '\n';
  }

  return parsed;
}

// Run lli with output capture
auto RunLliWithCapture(
    const std::filesystem::path& runtime_path,
    const std::filesystem::path& ir_path) -> std::pair<int, std::string> {
  // Create a pipe for stdout capture
  std::array<int, 2> stdout_pipe{};
  if (pipe(stdout_pipe.data()) != 0) {
    return {-1, "failed to create pipe"};
  }

  posix_spawn_file_actions_t actions{};
  posix_spawn_file_actions_init(&actions);
  posix_spawn_file_actions_adddup2(&actions, stdout_pipe[1], STDOUT_FILENO);
  posix_spawn_file_actions_addclose(&actions, stdout_pipe[0]);

  std::string lli_cmd = "lli";
  std::string dlopen_arg = std::format("--dlopen={}", runtime_path.string());
  std::string ir_path_str = ir_path.string();

  // std::string::data() returns char* (non-const) in C++17+ for non-const
  // strings
  std::array<char*, 4> argv = {
      lli_cmd.data(), dlopen_arg.data(), ir_path_str.data(), nullptr};

  pid_t pid = 0;
  // NOLINTNEXTLINE(misc-include-cleaner) - environ is from unistd.h
  int spawn_result =
      posix_spawnp(&pid, "lli", &actions, nullptr, argv.data(), environ);
  posix_spawn_file_actions_destroy(&actions);

  if (spawn_result != 0) {
    close(stdout_pipe[0]);
    close(stdout_pipe[1]);
    return {-1, std::format("posix_spawnp failed: {}", std::strerror(errno))};
  }

  // Close write end of pipe in parent
  close(stdout_pipe[1]);

  // Read stdout
  std::string output;
  std::array<char, 4096> buffer{};
  ssize_t bytes_read = 0;
  while ((bytes_read = read(stdout_pipe[0], buffer.data(), buffer.size())) >
         0) {
    output.append(buffer.data(), static_cast<size_t>(bytes_read));
  }
  close(stdout_pipe[0]);

  // Wait for process
  int status = 0;
  if (waitpid(pid, &status, 0) == -1) {
    return {-1, "waitpid failed"};
  }

  if (WIFEXITED(status)) {
    return {WEXITSTATUS(status), output};
  }
  return {-1, "process did not exit normally"};
}

}  // namespace

auto RunLlvmBackend(
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
      if (diagnostic.primary.kind == DiagKind::kError ||
          diagnostic.primary.kind == DiagKind::kUnsupported ||
          diagnostic.primary.kind == DiagKind::kHostError) {
        error_stream << diagnostic.primary.message << "\n";
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
  if (!mir_result) {
    result.error_message =
        std::format("MIR lowering error: {}", mir_result.error().primary.message);
    return result;
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
  // Top module is first, so its variables start right after packages
  auto tracked_variables =
      BuildTrackedVariables(top_module, mir_input, hir_result, base_slot_id);

  // Create test hooks for variable inspection and timing
  TestSimulationHooks hooks(std::move(tracked_variables), true);

  // Create diagnostic context for LLVM backend error reporting
  lowering::OriginMapLookup origin_lookup(
      &mir_result->origin_map, hir_result.hir_arena.get());
  lowering::DiagnosticContext diag_ctx(origin_lookup);

  // Lower MIR to LLVM IR
  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &mir_result->design,
      .mir_arena = mir_result->mir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .diag_ctx = &diag_ctx,
      .hooks = &hooks,
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
      result.error_message =
          std::format("{}: error: {}", location, diag.primary.message);
    } else {
      result.error_message =
          std::format("LLVM lowering error: {}", diag.primary.message);
    }
    return result;
  }

  // Write IR to temp file
  auto ir_dir = MakeUniqueTempPath(test_case.name + "_ir");
  std::filesystem::create_directories(ir_dir);
  ScopedTempDirectory ir_guard(ir_dir);

  auto ir_path = ir_dir / "test.ll";
  {
    std::ofstream out(ir_path);
    if (!out) {
      result.error_message = "Failed to write IR file";
      return result;
    }
    out << lowering::mir_to_llvm::DumpLlvmIr(*llvm_result);
  }

  // Find runtime library
  auto runtime_path = FindRuntimeLibrary();
  if (!runtime_path) {
    result.error_message = "Runtime library not found";
    return result;
  }

  // Run lli
  auto [exit_code, output] = RunLliWithCapture(*runtime_path, ir_path);
  if (exit_code < 0) {
    result.error_message = std::format("Failed to run lli: {}", output);
    return result;
  }

  // Parse output to extract variables and time
  auto parsed = ParseLyraVarOutput(output);
  result.success = true;
  result.captured_output = std::move(parsed.clean);
  result.variables = std::move(parsed.variables);
  result.final_time = parsed.final_time;
  return result;
}

}  // namespace lyra::test
