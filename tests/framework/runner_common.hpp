#pragma once

#include <atomic>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/diagnostics/DiagnosticEngine.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/parsing/Lexer.h>
#include <slang/parsing/Parser.h>
#include <slang/parsing/Preprocessor.h>
#include <slang/syntax/SyntaxTree.h>
#include <slang/text/SourceManager.h>
#include <slang/util/Bag.h>
#include <slang/util/LanguageVersion.h>

#include "tests/framework/test_case.hpp"

namespace lyra::test {

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
  auto operator=(const ScopedTempDirectory&) -> ScopedTempDirectory& = delete;

  ScopedTempDirectory(ScopedTempDirectory&& other) noexcept
      : path_(std::exchange(other.path_, {})) {}
  auto operator=(ScopedTempDirectory&& other) noexcept -> ScopedTempDirectory& {
    if (this != &other) {
      Cleanup();
      path_ = std::exchange(other.path_, {});
    }
    return *this;
  }

  ~ScopedTempDirectory() noexcept { Cleanup(); }

  [[nodiscard]] auto Path() const -> const std::filesystem::path& {
    return path_;
  }

 private:
  void Cleanup() noexcept {
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

  std::filesystem::path path_;
};

// Generate unique temp directory path using atomic counter
inline auto MakeUniqueTempPath(const std::string& test_name)
    -> std::filesystem::path {
  static std::atomic<uint64_t> counter{0};
  auto unique_suffix = std::to_string(counter.fetch_add(1));
  return std::filesystem::temp_directory_path() / "lyra_test" /
         (test_name + "_" + unique_suffix);
}

// Write test source files to a temp directory
inline auto WriteTempFiles(
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

// Format slang diagnostics for error output
inline auto FormatSlangDiagnostics(
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

// Result of parsing a test case with slang
struct SlangParseResult {
  std::unique_ptr<slang::ast::Compilation> compilation;
  std::unique_ptr<slang::SourceManager> source_manager;
  std::optional<ScopedTempDirectory> temp_guard;
  std::filesystem::path work_directory;
  std::string error_message;  // Empty on success

  [[nodiscard]] auto Success() const -> bool {
    return error_message.empty() && compilation != nullptr;
  }
};

// Parse a test case using slang with SV 2023 language features enabled
inline auto ParseTestCase(const TestCase& test_case) -> SlangParseResult {
  SlangParseResult result;
  result.source_manager = std::make_unique<slang::SourceManager>();

  // Configure lexer/preprocessor/parser to use 2023 language features
  slang::parsing::LexerOptions lexer_options;
  lexer_options.languageVersion = slang::LanguageVersion::v1800_2023;
  slang::parsing::PreprocessorOptions pp_options;
  pp_options.languageVersion = slang::LanguageVersion::v1800_2023;
  slang::parsing::ParserOptions parser_options;
  parser_options.languageVersion = slang::LanguageVersion::v1800_2023;
  slang::Bag parse_options;
  parse_options.set(lexer_options);
  parse_options.set(pp_options);
  parse_options.set(parser_options);

  slang::ast::CompilationOptions compilation_options;
  compilation_options.languageVersion = slang::LanguageVersion::v1800_2023;
  result.compilation =
      std::make_unique<slang::ast::Compilation>(compilation_options);

  if (test_case.IsMultiFile()) {
    auto [file_paths, temp_dir] =
        WriteTempFiles(test_case.files, test_case.name);
    result.temp_guard.emplace(temp_dir);
    result.work_directory = temp_dir;

    for (const auto& path : file_paths) {
      auto extension = std::filesystem::path(path).extension();
      if (extension == ".sv" || extension == ".v") {
        auto tree_result = slang::syntax::SyntaxTree::fromFile(
            path, *result.source_manager, parse_options);
        if (!tree_result) {
          result.error_message = "Failed to parse: " + path;
          return result;
        }
        result.compilation->addSyntaxTree(tree_result.value());
      }
      // Non-SV files (hex, mem, txt) are auxiliary data files, not parsed
    }
  } else {
    auto tree = slang::syntax::SyntaxTree::fromText(
        test_case.sv_code, *result.source_manager, "test.sv", "",
        parse_options);
    result.compilation->addSyntaxTree(tree);

    // Create work directory for single-file tests with file expectations
    if (!test_case.expected_files.empty()) {
      auto temp_dir = MakeUniqueTempPath(test_case.name);
      std::filesystem::create_directories(temp_dir);
      result.temp_guard.emplace(temp_dir);
      result.work_directory = temp_dir;
    }
  }

  // Check for slang errors
  auto diagnostics = result.compilation->getAllDiagnostics();
  bool has_errors = std::ranges::any_of(
      diagnostics, [](const auto& diag) { return diag.isError(); });
  if (has_errors) {
    result.error_message =
        "Parse errors:\n" +
        FormatSlangDiagnostics(diagnostics, *result.source_manager);
  }

  return result;
}

}  // namespace lyra::test
