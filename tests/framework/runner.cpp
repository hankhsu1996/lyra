#include "tests/framework/runner.hpp"

#include <algorithm>
#include <atomic>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <ios>
#include <map>
#include <memory>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
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
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/routine.hpp"
#include "tests/framework/assertions.hpp"
#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {
namespace {

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
  std::map<std::string, std::variant<int64_t, double>> variables;
  uint64_t final_time = 0;
  std::filesystem::path work_directory;
};

// Find initial process in MIR design
auto FindInitialProcess(const mir::Design& design, const mir::Arena& arena)
    -> std::optional<mir::ProcessId> {
  std::vector<mir::ProcessKind> found_kinds;

  for (const auto& element : design.elements) {
    if (const auto* module = std::get_if<mir::Module>(&element)) {
      for (mir::ProcessId process_id : module->processes) {
        const auto& process = arena[process_id];
        found_kinds.push_back(process.kind);
        if (process.kind == mir::ProcessKind::kOnce) {
          return process_id;
        }
      }
    }
  }
  return std::nullopt;
}

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
      .design = hir_result.design,
      .hir_arena = *hir_result.hir_arena,
      .type_arena = *hir_result.type_arena,
      .constant_arena = *hir_result.constant_arena,
      .symbol_table = *hir_result.symbol_table,
  };
  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);

  // Find initial process
  auto process_id =
      FindInitialProcess(mir_result.design, *mir_result.mir_arena);
  if (!process_id) {
    result.error_message = "No initial process found (no kOnce process in MIR)";
    return result;
  }

  // Create interpreter state
  auto state =
      mir::interp::CreateProcessState(*mir_result.mir_arena, *process_id);

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

  try {
    interpreter.Run(state);
  } catch (const std::exception& e) {
    result.error_message = std::string("Runtime error: ") + e.what();
    return result;
  }

  result.success = true;
  result.captured_output = output_stream.str();

  // On success, temp_guard destructor will clean up (unless LYRA_TEST_KEEP_TMP)
  return result;
}

}  // namespace

void RunTestCase(const TestCase& test_case, BackendKind backend) {
  switch (backend) {
    case BackendKind::kMir: {
      auto result = RunMirInterpreter(test_case);
      ASSERT_TRUE(result.success)
          << "[" << test_case.source_yaml << "] " << result.error_message;

      // Check expected variables (not yet supported by MIR interpreter)
      if (!test_case.expected_values.empty()) {
        GTEST_SKIP()
            << "Variable assertions not yet supported by MIR interpreter";
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

    case BackendKind::kLlvm:
      GTEST_SKIP() << "LLVM backend not yet implemented";
      break;
  }
}

}  // namespace lyra::test
