#include "tests/framework/runner.hpp"

#include <algorithm>
#include <array>
#include <atomic>
#include <cerrno>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <ios>
#include <map>
#include <memory>
#include <optional>
#include <spawn.h>
#include <sstream>
#include <stdexcept>
#include <string>
#include <sys/wait.h>
#include <unistd.h>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/diagnostics/DiagnosticEngine.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/syntax/SyntaxTree.h>
#include <slang/text/SourceManager.h>
#include <slang/util/LanguageVersion.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/mir_to_llvm/lower.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "tests/framework/assertions.hpp"
#include "tests/framework/suite.hpp"
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
    -> std::expected<std::variant<int64_t, double, HexValue>, std::string> {
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

// RAII guard for temporarily changing current directory
class ScopedCurrentPath {
 public:
  ScopedCurrentPath(const ScopedCurrentPath&) = delete;
  ScopedCurrentPath(ScopedCurrentPath&&) = delete;
  auto operator=(const ScopedCurrentPath&) -> ScopedCurrentPath& = delete;
  auto operator=(ScopedCurrentPath&&) -> ScopedCurrentPath& = delete;

  explicit ScopedCurrentPath(const std::filesystem::path& path)
      : previous_(std::filesystem::current_path()) {
    std::filesystem::current_path(path);
  }

  ~ScopedCurrentPath() noexcept {
    try {
      std::filesystem::current_path(previous_);
    } catch (...) {
      // Swallow exceptions to avoid terminate during stack unwinding
    }
  }

 private:
  std::filesystem::path previous_;
};

// RAII guard for temp directory cleanup (unless LYRA_TEST_KEEP_TMP is set)
class ScopedTempDirectory {
 public:
  explicit ScopedTempDirectory(std::filesystem::path path)
      : path_(std::move(path)) {
  }

  ScopedTempDirectory(const ScopedTempDirectory&) = delete;
  ScopedTempDirectory(ScopedTempDirectory&&) = delete;
  auto operator=(const ScopedTempDirectory&) -> ScopedTempDirectory& = delete;
  auto operator=(ScopedTempDirectory&&) -> ScopedTempDirectory& = delete;

  ~ScopedTempDirectory() noexcept {
    if (std::getenv("LYRA_TEST_KEEP_TMP") != nullptr) {
      return;
    }
    try {
      if (!path_.empty() && std::filesystem::exists(path_)) {
        std::filesystem::remove_all(path_);
      }
    } catch (...) {
      // Swallow exceptions in destructor
    }
  }

  [[nodiscard]] auto Path() const -> const std::filesystem::path& {
    return path_;
  }

 private:
  std::filesystem::path path_;
};

// Generate unique temp directory path using atomic counter
auto MakeUniqueTempPath(const std::string& test_name) -> std::filesystem::path {
  static std::atomic<uint64_t> counter{0};
  auto unique_suffix = std::to_string(counter.fetch_add(1));
  return std::filesystem::temp_directory_path() / "lyra_test" /
         (test_name + "_" + unique_suffix);
}

auto WriteTempFiles(
    const std::vector<SourceFile>& files, const std::string& test_name)
    -> std::pair<std::vector<std::string>, std::filesystem::path> {
  auto temp_directory = MakeUniqueTempPath(test_name);
  std::filesystem::create_directories(temp_directory);

  std::vector<std::string> result_paths;
  for (const auto& file : files) {
    auto path = temp_directory / file.name;

    // Validate extension - don't silently ignore unknown files
    auto extension = path.extension().string();
    if (extension != ".sv" && extension != ".v" && extension != ".hex" &&
        extension != ".mem" && extension != ".txt") {
      throw std::runtime_error(
          std::format(
              "Unsupported file extension in test files: {} ({})", file.name,
              extension));
    }

    std::ofstream output(path, std::ios::binary);
    output << file.content;
    if (!output.good()) {
      throw std::runtime_error("Failed to write temp file: " + path.string());
    }
    result_paths.push_back(path.string());
  }
  return {result_paths, temp_directory};
}

// Result from running a test
struct TestResult {
  bool success = false;
  std::string error_message;
  std::string captured_output;
  std::map<std::string, ExtractedValue> variables;
  uint64_t final_time = 0;
  std::filesystem::path work_directory;
};

// Format slang diagnostics for error output
auto FormatSlangDiagnostics(
    const slang::Diagnostics& diagnostics, slang::SourceManager& source_manager)
    -> std::string {
  slang::DiagnosticEngine engine(source_manager);
  auto client = std::make_shared<slang::TextDiagnosticClient>();
  engine.addClient(client);

  for (const auto& diagnostic : diagnostics) {
    engine.issue(diagnostic);
  }

  return client->getString();
}

// Run test using MIR interpreter
auto RunMirInterpreter(const TestCase& test_case) -> TestResult {
  TestResult result;
  std::optional<ScopedTempDirectory> temp_guard;

  // Create slang compilation from source
  slang::SourceManager source_manager;

  slang::ast::CompilationOptions compilation_options;
  compilation_options.languageVersion = slang::LanguageVersion::v1800_2023;
  auto compilation =
      std::make_unique<slang::ast::Compilation>(compilation_options);

  if (test_case.IsMultiFile()) {
    // Multi-file: write to temp and load from files
    auto [file_paths, temp_dir] =
        WriteTempFiles(test_case.files, test_case.name);
    temp_guard.emplace(temp_dir);
    result.work_directory = temp_dir;

    for (const auto& path : file_paths) {
      auto extension = std::filesystem::path(path).extension();
      if (extension == ".sv" || extension == ".v") {
        auto tree_result =
            slang::syntax::SyntaxTree::fromFile(path, source_manager);
        if (!tree_result) {
          result.error_message = "Failed to parse: " + path;
          return result;
        }
        compilation->addSyntaxTree(tree_result.value());
      }
      // Non-SV files (hex, mem, txt) are auxiliary data files, not parsed
    }
  } else {
    // Single source from string
    auto tree = slang::syntax::SyntaxTree::fromText(
        test_case.sv_code, source_manager, "test.sv");
    compilation->addSyntaxTree(tree);

    // Create work directory for single-file tests with file expectations
    if (!test_case.expected_files.empty()) {
      auto temp_dir = MakeUniqueTempPath(test_case.name);
      std::filesystem::create_directories(temp_dir);
      temp_guard.emplace(temp_dir);
      result.work_directory = temp_dir;
    }
  }

  // Check for slang errors (only format diagnostics if there are errors)
  auto diagnostics = compilation->getAllDiagnostics();
  bool has_errors = std::ranges::any_of(
      diagnostics, [](const auto& diag) { return diag.isError(); });
  if (has_errors) {
    result.error_message =
        "Parse errors:\n" + FormatSlangDiagnostics(diagnostics, source_manager);
    return result;
  }

  // Lower AST to HIR
  DiagnosticSink sink;
  auto hir_result = lowering::ast_to_hir::LowerAstToHir(*compilation, sink);

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

  // Find the HIR module (needed for variable initialization and assertions)
  const hir::Module* hir_module = nullptr;
  size_t module_count = 0;
  for (const auto& element : mir_input.design->elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      hir_module = mod;
      ++module_count;
    }
  }
  if (module_count > 1) {
    result.error_message = std::format(
        "Test framework supports single module only, got {}", module_count);
    return result;
  }

  // Build variable name to slot index mapping (for variable assertions)
  std::unordered_map<std::string, size_t> var_slots;
  if (!test_case.expected_values.empty()) {
    if (hir_module == nullptr) {
      result.error_message = "Variable assertions require a module";
      return result;
    }
    for (size_t i = 0; i < hir_module->variables.size(); ++i) {
      const auto& sym = (*mir_input.symbol_table)[hir_module->variables[i]];
      if (var_slots.contains(sym.name)) {
        result.error_message =
            std::format("Duplicate variable name '{}' in module", sym.name);
        return result;
      }
      var_slots[sym.name] = i;
    }
  }

  // Find initial module
  auto module_info =
      mir::interp::FindInitialModule(mir_result.design, *mir_result.mir_arena);
  if (!module_info) {
    result.error_message = "No initial process found (no kOnce process in MIR)";
    return result;
  }

  // Create module storage
  auto design_state = mir::interp::CreateDesignState(
      *mir_result.mir_arena, *hir_result.type_arena, *module_info->module);

  // Initialize module slots from HIR variable types.
  // CreateDesignState only initializes slots that are referenced in code; this
  // ensures all declared variables are properly initialized (2-state → 0,
  // 4-state → X) even if never used.
  //
  // INVARIANT: MIR lowering assigns slot IDs sequentially in
  // hir_module->variables order (see hir_to_mir/module.cpp). If this changes,
  // initialization will break.
  if (hir_module != nullptr) {
    if (hir_module->variables.size() != design_state.storage.size()) {
      result.error_message = std::format(
          "Slot count mismatch: HIR has {} variables, MIR has {} slots",
          hir_module->variables.size(), design_state.storage.size());
      return result;
    }
    for (size_t i = 0; i < hir_module->variables.size(); ++i) {
      const auto& sym = (*mir_input.symbol_table)[hir_module->variables[i]];
      design_state.storage[i] =
          mir::interp::CreateDefaultValue(*hir_result.type_arena, sym.type);
    }
  }

  // Run interpreter with output capture
  std::ostringstream output_stream;
  mir::interp::Interpreter interpreter(
      mir_result.mir_arena.get(), hir_result.type_arena.get());
  interpreter.SetOutput(&output_stream);

  // Change to work_directory if we have one (for file I/O tests)
  std::optional<ScopedCurrentPath> scoped_path;
  if (!result.work_directory.empty()) {
    scoped_path.emplace(result.work_directory);
  }

  // Run all initial processes in order
  try {
    for (mir::ProcessId proc_id : module_info->initial_processes) {
      auto state = mir::interp::CreateProcessState(
          *mir_result.mir_arena, *hir_result.type_arena, proc_id,
          &design_state);
      interpreter.Run(state);
    }
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

  // On success, temp_guard destructor will clean up (unless LYRA_TEST_KEEP_TMP)
  return result;
}

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

  std::string dlopen_arg = std::format("--dlopen={}", runtime_path.string());
  std::string ir_path_str = ir_path.string();

  std::array<char*, 4> argv = {
      const_cast<char*>("lli"), const_cast<char*>(dlopen_arg.c_str()),
      const_cast<char*>(ir_path_str.c_str()), nullptr};

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

// Run test using LLVM JIT backend
auto RunLlvmBackend(const TestCase& test_case) -> TestResult {
  TestResult result;
  std::optional<ScopedTempDirectory> temp_guard;

  // Create slang compilation from source
  slang::SourceManager source_manager;

  slang::ast::CompilationOptions compilation_options;
  compilation_options.languageVersion = slang::LanguageVersion::v1800_2023;
  auto compilation =
      std::make_unique<slang::ast::Compilation>(compilation_options);

  if (test_case.IsMultiFile()) {
    auto [file_paths, temp_dir] =
        WriteTempFiles(test_case.files, test_case.name);
    temp_guard.emplace(temp_dir);
    result.work_directory = temp_dir;

    for (const auto& path : file_paths) {
      auto extension = std::filesystem::path(path).extension();
      if (extension == ".sv" || extension == ".v") {
        auto tree_result =
            slang::syntax::SyntaxTree::fromFile(path, source_manager);
        if (!tree_result) {
          result.error_message = "Failed to parse: " + path;
          return result;
        }
        compilation->addSyntaxTree(tree_result.value());
      }
    }
  } else {
    auto tree = slang::syntax::SyntaxTree::fromText(
        test_case.sv_code, source_manager, "test.sv");
    compilation->addSyntaxTree(tree);
  }

  // Check for slang errors
  auto diagnostics = compilation->getAllDiagnostics();
  bool has_errors = std::ranges::any_of(
      diagnostics, [](const auto& diag) { return diag.isError(); });
  if (has_errors) {
    result.error_message =
        "Parse errors:\n" + FormatSlangDiagnostics(diagnostics, source_manager);
    return result;
  }

  // Lower AST to HIR
  DiagnosticSink sink;
  auto hir_result = lowering::ast_to_hir::LowerAstToHir(*compilation, sink);

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

  // Lower MIR to LLVM IR
  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &mir_result.design,
      .mir_arena = mir_result.mir_arena.get(),
      .type_arena = hir_result.type_arena.get(),
  };

  lowering::mir_to_llvm::LoweringResult llvm_result;
  try {
    llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
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

  result.success = true;
  result.captured_output = output;
  return result;
}

}  // namespace

void RunTestCase(const TestCase& test_case, BackendKind backend) {
  switch (backend) {
    case BackendKind::kMir: {
      auto result = RunMirInterpreter(test_case);
      ASSERT_TRUE(result.success)
          << "[" << test_case.source_yaml << "] " << result.error_message;

      // Check expected variables
      if (!test_case.expected_values.empty()) {
        AssertVariables(
            result.variables, test_case.expected_values, test_case.source_yaml);
      }

      // Check expected time
      if (test_case.expected_time.has_value()) {
        GTEST_SKIP() << "Time assertions not yet supported by MIR interpreter";
      }

      // Check expected stdout
      if (test_case.expected_stdout.has_value()) {
        AssertOutput(result.captured_output, test_case.expected_stdout.value());
      }

      // Check expected files
      if (!test_case.expected_files.empty()) {
        AssertFiles(result.work_directory, test_case.expected_files);
      }
      break;
    }

    case BackendKind::kLlvm: {
      // LLVM backend only supports stdout assertions for now
      // (no variable inspection in JIT'd code)
      if (!test_case.expected_values.empty()) {
        GTEST_SKIP() << "LLVM backend does not support variable assertions";
      }
      if (test_case.expected_time.has_value()) {
        GTEST_SKIP() << "LLVM backend does not support time assertions";
      }
      if (!test_case.expected_files.empty()) {
        GTEST_SKIP() << "LLVM backend does not support file assertions";
      }

      auto result = RunLlvmBackend(test_case);
      ASSERT_TRUE(result.success)
          << "[" << test_case.source_yaml << "] " << result.error_message;

      // Check expected stdout
      if (test_case.expected_stdout.has_value()) {
        AssertOutput(result.captured_output, test_case.expected_stdout.value());
      }
      break;
    }
  }
}

}  // namespace lyra::test
