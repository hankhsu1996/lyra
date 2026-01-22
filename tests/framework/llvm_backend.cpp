#include "tests/framework/llvm_backend.hpp"

#include <array>
#include <cctype>
#include <cerrno>
#include <cstdint>
#include <cstring>
#include <format>
#include <fstream>
#include <spawn.h>
#include <sstream>
#include <string>
#include <sys/wait.h>
#include <unistd.h>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/mir_to_llvm/lower.hpp"
#include "tests/framework/runner_common.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {
namespace {

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

// Result from building LLVM lowering info
struct LlvmLoweringInfo {
  std::vector<lowering::mir_to_llvm::SlotTypeInfo> slot_types;
  std::vector<TypeId> slot_type_ids;
  std::vector<lowering::mir_to_llvm::VariableInfo> variables;
};

// Build slot types (for ALL variables) and tracked variables (for inspection)
auto BuildLlvmLoweringInfo(
    const hir::Module* hir_module,
    const lowering::hir_to_mir::LoweringInput& mir_input,
    const lowering::ast_to_hir::LoweringResult& hir_result)
    -> LlvmLoweringInfo {
  LlvmLoweringInfo info;
  if (hir_module == nullptr) {
    return info;
  }

  info.slot_types.reserve(hir_module->variables.size());
  info.slot_type_ids.reserve(hir_module->variables.size());

  for (size_t i = 0; i < hir_module->variables.size(); ++i) {
    const auto& sym = (*mir_input.symbol_table)[hir_module->variables[i]];
    const Type& type = (*hir_result.type_arena)[sym.type];

    // Always store the TypeId for LLVM type derivation
    info.slot_type_ids.push_back(sym.type);

    // Handle real types
    if (type.Kind() == TypeKind::kReal) {
      info.slot_types.push_back({
          .kind = lowering::mir_to_llvm::VarTypeKind::kReal,
          .width = 64,
          .is_signed = true,
      });
      info.variables.push_back({.name = sym.name, .slot_id = i});
      continue;
    }

    // Handle string types (no variable inspection yet)
    if (type.Kind() == TypeKind::kString) {
      info.slot_types.push_back({
          .kind = lowering::mir_to_llvm::VarTypeKind::kString,
          .width = 0,
          .is_signed = false,
      });
      continue;
    }

    // Handle packed integral types
    if (!IsPacked(type)) {
      // Unsupported type - use placeholder for slot_types, skip for variables
      info.slot_types.push_back({
          .kind = lowering::mir_to_llvm::VarTypeKind::kIntegral,
          .width = 32,
          .is_signed = false,
      });
      continue;
    }
    if (IsPackedFourState(type, *hir_result.type_arena)) {
      // 4-state type - use placeholder for slot_types, skip for variables
      uint32_t width = PackedBitWidth(type, *hir_result.type_arena);
      info.slot_types.push_back({
          .kind = lowering::mir_to_llvm::VarTypeKind::kIntegral,
          .width = width > 0 ? width : 32,
          .is_signed = false,
      });
      continue;
    }

    uint32_t width = PackedBitWidth(type, *hir_result.type_arena);
    bool is_signed = IsPackedSigned(type, *hir_result.type_arena);

    // Add to slot_types for initialization (all supported packed types)
    info.slot_types.push_back({
        .kind = lowering::mir_to_llvm::VarTypeKind::kIntegral,
        .width = width > 0 ? width : 32,
        .is_signed = is_signed,
    });

    // Add to variables for inspection (only types we can parse back)
    // Skip: width 0, or unsigned 64-bit (would overflow int64_t)
    // >64-bit uses 'h:' hex format, ≤64-bit uses 'i:' decimal format
    if (width > 64 || (width > 0 && width <= 64 && (width < 64 || is_signed))) {
      info.variables.push_back({.name = sym.name, .slot_id = i});
    }
  }
  return info;
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

// Parse __LYRA_VAR output and extract variable values.
// Strips __LYRA_VAR entries while preserving all other output exactly,
// including whether the original had a trailing newline.
// Handles __LYRA_VAR appearing mid-line (after $write with no newline).
auto ParseLyraVarOutput(const std::string& output)
    -> std::pair<std::string, std::map<std::string, ExtractedValue>> {
  std::string clean_output;
  std::map<std::string, ExtractedValue> variables;

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

  while (std::getline(stream, line)) {
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

  return {clean_output, variables};
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

auto RunLlvmBackend(const TestCase& test_case) -> TestResult {
  TestResult result;

  // Parse test case using slang
  auto parse_result = ParseTestCase(test_case);
  if (!parse_result.Success()) {
    result.error_message = parse_result.error_message;
    return result;
  }
  result.work_directory = parse_result.work_directory;

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
  };
  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);

  // Find the HIR module and build lowering info
  const hir::Module* hir_module = nullptr;
  for (const auto& element : mir_input.design->elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      hir_module = mod;
    }
  }
  auto lowering_info = BuildLlvmLoweringInfo(hir_module, mir_input, hir_result);

  // Lower MIR to LLVM IR
  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &mir_result.design,
      .mir_arena = mir_result.mir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
      .slot_types = std::move(lowering_info.slot_types),
      .slot_type_ids = std::move(lowering_info.slot_type_ids),
      .variables = std::move(lowering_info.variables),
  };

  lowering::mir_to_llvm::LoweringResult llvm_result;
  try {
    llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  } catch (const common::UnsupportedErrorException& e) {
    // Try to resolve origin to source location
    const auto& error = e.GetError();
    std::string location;
    if (error.origin.IsValid()) {
      auto entry = mir_result.origin_map.Resolve(error.origin);
      if (entry &&
          std::holds_alternative<hir::StatementId>(entry->hir_source)) {
        auto stmt_id = std::get<hir::StatementId>(entry->hir_source);
        const hir::Statement& stmt = (*hir_result.hir_arena)[stmt_id];
        location = FormatSourceLocation(stmt.span, *hir_result.source_manager);
      }
    }
    if (!location.empty()) {
      result.error_message = std::format("{}: error: {}", location, e.what());
    } else {
      result.error_message = std::format("LLVM lowering error: {}", e.what());
    }
    return result;
  } catch (const std::exception& e) {
    result.error_message = std::format("LLVM lowering error: {}", e.what());
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
    out << lowering::mir_to_llvm::DumpLlvmIr(llvm_result);
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

  // Parse output to extract variables
  auto [clean_output, variables] = ParseLyraVarOutput(output);
  result.success = true;
  result.captured_output = clean_output;
  result.variables = std::move(variables);
  return result;
}

}  // namespace lyra::test
