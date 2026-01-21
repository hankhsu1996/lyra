#pragma once

#include <atomic>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include <slang/diagnostics/DiagnosticEngine.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/text/SourceManager.h>

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

}  // namespace lyra::test
